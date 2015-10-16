

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
#' @param n_canopies An integer specifying how many canopies (ie- subsets) to create.
#' @references Improved Methods for the Impution of Missing Data by Nearest Neighbors Methods
#' Tutz and Ramzan (2015)
#' @examples
#'   x = matrix(rnorm(100),10,10)
#'   x[x > 1] = NA
#'   kNN_impute(x, k=3, q=2)
#' @export
kNN_impute.canopies <- function(x, k, q= 2, verbose=TRUE, check_scale= TRUE,
                      parallel= TRUE, leave_cores= ifelse(detectCores() <= 4, 1, 2),
                      n_canopies) {
  
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
  if (k < 1 | k >= nrow(x) | k %% 1 != 0) stop("k must be an integer in {1, nrow(x) - 1}")
  if (q < 1 | q %% 1 != 0) stop("q must be an integer >= 1")
  if (n_canopies <= 3 | n_canopies %% 1 != 0) stop("n_canopies must be an integer > 3")
  
  # 01a. ID canopy and subset of interest
  #--------------------------------------------------------
  can_id <- ncol(x)
  if (length(table(x[, can_id])) == 2) {
    can <- ifelse(max(x[, can_id]) == n_canopies, max(x[, can_id]), min(x[, can_id]))
  } else {
    can <- median(x[, can_id])
  }
  
  x_can <- x[x[, can_id] == can,] # subset of interest
  
  col_na <- apply(x_can[, -can_id], 2, function(j) all(is.na(j)))
  row_na <- apply(x_can[, -can_id], 1, function(i) all(is.na(i)))
  
  if (any(col_na)) {
    cat("column(s)", which(col_na), "are entirely missing.")
    stop("Please fix missing columns.")
  }
  
  # 01b. Test if variables on same scale
  #--------------------------------------------------------
  if (check_scale) unequal_var <- var_tests(x[, -can_id], bonf= TRUE)
  if (!is.null(unequal_var)) warning(paste("Some variables appear to have unequal variances.",
                                           "KNN is best with equally scaled variables."))
  
  # 01d. Get Gaussian Kernal
  #--------------------------------------------------------
  # https://en.wikipedia.org/wiki/Kernel_density_estimation#Practical_estimation_of_the_bandwidth
  opt_h <- (4 * sd(x[, -can_id], na.rm=T)^5 / (3 * nrow(x)))^(1/5)
  kern <- rbfdot(opt_h)
  
  # 02a. Impute missing rows to complete-data column means 
  #--------------------------------------------------------
  if (any(row_na)) {
    if (verbose) cat("row(s)", which(row_na), "are entirely missing. 
                     These row(s)' values will be imputed to column means.")
    warning("Rows with entirely missing values imputed to column means.")
    
    col_means <- colMeans(x[, -can_id], na.rm=T)
    for (i in which(row_na)) {
      x_can[i, -can_id] <- col_means
    }
  }
  
  # 02b. Impute 
  #--------------------------------------------------------
  prelim = impute_prelim(x_can, parallel= parallel, leave_cores= leave_cores)
  if (prelim$numMissing == 0) return (x_can) # no missing
  
  if (parallel == FALSE) {
    x_missing_imputed <- impute_fn_knn_all.nonPar(
          x_missing= prelim$x_missing[, -ncol(prelim$x_missing)],
          x_complete= x[,-ncol(x)], k=k, q=q, 
          kern= kern, verbose= verbose)
  } else if (parallel == TRUE) {
    x_missing_imputed <- impute_fn_knn_all.Par(
          x_missing= prelim$x_missing[, -ncol(prelim$x_missing)],
          x_complete= x[,-ncol(x)], k=k, q=q, 
          kern= kern, leave_cores= leave_cores)
  }
  
  # insert imputations
  x_can[prelim$missing_rows_indices,] <- x_missing_imputed
  
  # 03. Validate and return
  #--------------------------------------------------------
  num_errors = sum(is.na(x_can))
  if (num_errors > 0) {
    x_can <- x_can[order(as.integer(rownames(x_can))),]
    return(list(x=x_can, num_errors= num_errors))
  } else {
    x_can <- x_can[order(as.integer(rownames(x_can))),]
    return(list(x=x_can))  
  }
}



