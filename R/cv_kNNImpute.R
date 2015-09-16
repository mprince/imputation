#' CV for kNNImpute
#'
#' Cross Validation for kNNImpute
#' Artificially erase some data and run kNNImpute multiple times,
#' varying k from 1 to k.max.  For each k, compute the RMSE on the subset of x
#' for which data was artificially erased.
#' @param x a data frame or matrix where each row represents a different record
#' @param k.max the largest amount of neighbors to try kNN Impute
#' @param parallel runs each run for k = 1 to k = k.max in parallel.  Requires
#'   a parallel backend to be registered
#' @examples
#'   x = matrix(rnorm(100),10,10)
#'   x.missing = x > 1
#'   x[x.missing] = NA
#'   cv.kNNImpute(x)
#' @export
cv.kNNImpute = function(x, k.max=5, parallel = F) {
  if (!is.matrix(x)) stop("x should be a numeric data matrix")
  if (k.max >= nrow(x)) stop("k.max must be less than nrow(x)")
  
  prelim = cv.impute.prelim(x)
  remove.indices = prelim$remove.indices
  x.train = prelim$x.train
  
  x.dist = dist(x)
  if (parallel) {
    if (!require(foreach)) stop("R package foreach is required for parallel execution, as well
                                as a registered parallel backend")
    rmse = foreach (i=1:k.max, .combine = c, .packages = c('imputation')) %dopar% {
      x.imputed = kNNImpute(x.train, i, x.dist, verbose=F)$x
      error = (x.imputed[remove.indices] - x[remove.indices])
      sqrt(mean(error^2))
    }
  }
  else {
    rmse = sapply(1:k.max, function(i) {
      x.imputed = kNNImpute(x.train, i, x.dist, verbose=F)$x
      error = (x.imputed[remove.indices] - x[remove.indices])
      sqrt(mean(error^2))
    })
  }
  list(k = which.min(rmse), rmse = rmse[which.min(rmse)],
       k.full = 1:k.max, rmse.full = rmse)
  }
