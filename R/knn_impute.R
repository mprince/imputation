

#' @title Imputation using weighted-kNN
#' @description Imputation using weighted k-nearest neighbors.
#' For each record, identify missinng features.  For each missing feature
#' find the \eqn{k} nearest neighbors which have that feature.  Impute the missing
#' value using the \eqn{k} nearest neighbors having that feature. Weights are computed
#' using a Gaussian kernal bandwidth parameter using 'Silverman's rule of thumb'
#' as described by Silverman (1998)
#' 
#' @param x a \code{matrix} or \code{data.frame} which can be coerced to a matrix
#'  where each row represents a different record
#' @param k the number of neighbors to use for imputation
#' @param q An integer specifying the which norm to take the L-q distance of.
#' @param impute_fn The imputation function to run on the length k vector of values for
#'   a missing feature.  Defaults to weighted mean-KNN; see Details. 
#' @param verbose if \code{TRUE} print status updates
#' @param check.scale Logical. If \code{TRUE} compute pairwise variance tests to see if
#' variables are on a common scale. Bonferroni correction applied.
#' @references Improved Methods for the Impution of Missing Data by Nearest Neighbors Methods
#' Tutz and Ramzan (2015)
#' @examples
#'   x = matrix(rnorm(100),10,10)
#'   x_missing = x > 1
#'   x[x_missing] = NA
#'   kNN_impute(x, 3)
#' @export
kNN_impute = function(x, k, q= 2, verbose=TRUE, check.scale= TRUE) {
  
  # 01a. Do some preliminaries
  #--------------------------------------------------------
  if (is.data.frame(x)) x <- as.matrix(x)
  if (!is.matrix(x)) stop("x should be a numeric data matrix")
  if(k < 1 | k >= nrow(x)) stop("k must be an integer in {1, nrow(x) - 1}")
  if (q < 1) stop("q must be an integer >= 1")
  
  prelim = impute_prelim(x)
  if (prelim$numMissing == 0) return (x) # no missing
  
  col_na <- apply(x, 2, function(j) all(is.na(j)))
  row_na <- apply(x, 1, function(i) all(is.na(i)))
  
  if (any(col_na)) {
    cat("column(s)", which(col_na), "are entirely missing.")
    stop("Please fix missing columns.")
  }
  
  # 01b. Test if variables on same scale
  #--------------------------------------------------------
  unequal_var <- var_tests(x, bonf= TRUE)
  if (!is.null(unequal_var)) warning(paste("Some variables appear to have unequal variances.",
                                           "KNN is best with equally scaled variables."))
  
  # 01d. Get Gaussian Kernal
  #--------------------------------------------------------
  # https://en.wikipedia.org/wiki/Kernel_density_estimation#Practical_estimation_of_the_bandwidth
  opt_h <- (4 * sd(x, na.rm=T)^5 / (3 * nrow(x)))^(1/5)
  kern <- rbfdot(opt_h)
  
  # 02a. Impute missing rows to complete-data column means 
  #--------------------------------------------------------
  if (any(row_na)) {
    cat("row(s)", which(row_na), "are entirely missing. These row(s)' values will be imputed to column means.")
    warning("Rows with entirely missing values imputed to column means.")
    
    col_means <- colMeans(x, na.rm=T)
    for (i in row_na) {
      x[i,] <- col_means
    }
  }
  
  # 02b. Impute
  #--------------------------------------------------------
  # impute row-by-row
  x_missing_imputed <- apply(prelim$x_missing, 1, function(i) {
    rowIndex = as.numeric(i[1])
    i_original = unlist(i[-1])
    # verbose option
    if(verbose) print(paste("Imputing row", rowIndex, sep=" "))
    missing_cols <- which(prelim$missing_matrix[rowIndex,])
    
    # calculate distances
    distances <- dist_q.matrix(rbind(x[rowIndex, ], x[-rowIndex,]), ref= 1, q= q)
    
    # within the given row, impute by column
    imputed_values <- sapply(missing_cols, function(j, distances) {
      # which neighbors have data on column j?
      neighbor_indices = which(!prelim$missing_matrix[,j])
      # impute
      return(impute_fn_knn(x[neighbor_indices, j], distances[neighbor_indices], k=k, kern= kern))
    }, distances= distances)
    i_original[missing_cols] <- imputed_values
    return(i_original)
  })
  
  # insert imputations
  x[prelim$missing_rows_indices,] <- x_missing_imputed
  
  # 03. Validate and return
  #--------------------------------------------------------
  num_errors = sum(is.na(x))
  if (num_errors > 0) {
    return(list(x=x, missing_matrix= prelim$missing_matrix, num_errors= num_errors))
  } else {
    return(list(x=x, missing_matrix= prelim$missing_matrix))  
  }
}