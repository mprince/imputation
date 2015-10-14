

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
#' @param verbose if \code{TRUE} print status updates
#' @param check_scale Logical. If \code{TRUE} compute pairwise variance tests to see if
#' variables are on a common scale. Bonferroni correction applied.
#' @param parallel Logical. Do you wish to parallelize the code? Defaults to \code{TRUE}
#' @param leave_cores How many cores do you wish to leave open to other processing?
#' @references Improved Methods for the Impution of Missing Data by Nearest Neighbors Methods
#' Tutz and Ramzan (2015)
#' @examples
#'   x = matrix(rnorm(100),10,10)
#'   x[x > 1] = NA
#'   kNN_impute(x, k=3, q=2)
#' @export
kNN_impute.no_canopies <- function(x, k, q= 2, verbose=TRUE, check_scale= TRUE,
                      parallel= TRUE, leave_cores= ifelse(detectCores() <= 4, 1, 2)) {
  
  # 01. Do some preliminaries -- to remove after updating tests (duplicated work)
  #--------------------------------------------------------
  if (parallel == TRUE) {
    if (leave_cores < 0 | leave_cores > detectCores() | leave_cores %% 1 != 0) {
      stop("leave_cores must be an integer between 0 (not recommended) 
           and ", detectCores())
    }
  }
  
  if (is.data.frame(x)) x <- as.matrix(x)
  if (!is.numeric(x) | !is.matrix(x)) stop("x should be a numeric data matrix")
  if(k < 1 | k >= nrow(x) | k %% 1 != 0) stop("k must be an integer in {1, nrow(x) - 1}")
  if (q < 1 | q %% 1 != 0) stop("q must be an integer >= 1")
  
  col_na <- apply(x, 2, function(j) all(is.na(j)))
  row_na <- apply(x, 1, function(i) all(is.na(i)))
  
  if (any(col_na)) {
    cat("column(s)", which(col_na), "are entirely missing.")
    stop("Please fix missing columns.")
  }
  
  # 01b. Test if variables on same scale
  #--------------------------------------------------------
  if (check_scale) unequal_var <- var_tests(x, bonf= TRUE)
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
    if (verbose) cat("row(s)", which(row_na), "are entirely missing. 
                     These row(s)' values will be imputed to column means.")
    warning("Rows with entirely missing values imputed to column means.")
    
    col_means <- colMeans(x, na.rm=T)
    for (i in which(row_na)) {
      x[i,] <- col_means
    }
  }
  
  # [10/13] add rownames such that impute_prelim() can work with or without canopies
  # and so that impute_fn_knn_all.** work properly.
  # Specifically, this is needed if kNN_impute.default() is called outside kNN_impute()
  if (is.null(rownames(x))) {
    rownames(x) <- 1:nrow(x)
  }
  
  # 02b. Impute 
  #--------------------------------------------------------
  prelim = impute_prelim(x, parallel= parallel, leave_cores= leave_cores)
  if (prelim$numMissing == 0) return (x) # no missing
  
  if (parallel == FALSE) {
    x_missing_imputed <- impute_fn_knn_all.nonPar(x_missing= prelim$x_missing,
                               x_complete= x, k=k, q=q, 
                               kern= kern, verbose= verbose)
  } else if (parallel == TRUE) {
    x_missing_imputed <- impute_fn_knn_all.Par(x_missing= prelim$x_missing,
                               x_complete= x, k=k, q=q,
                               kern= kern, leave_cores= leave_cores)
  }
  
  # insert imputations
  x[prelim$missing_rows_indices,] <- x_missing_imputed
  
  # 03. Validate and return
  #--------------------------------------------------------
  num_errors = sum(is.na(x))
  if (num_errors > 0) {
    return(list(x=x, num_errors= num_errors))
  } else {
    return(list(x=x))  
  }
}



