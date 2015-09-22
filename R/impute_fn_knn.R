

#' @title Imputation function for kNN
#' @description Function for KNN imputation. Distances are weighted by a kernal function
#' @param values The values from which imputation will take place
#' @param distances The distances associated with each value
#' @param k The number of neighbors used to impute
#' @param kern The Gaussian kernal used for weighting
#' @seealso \code{\link[kernlab]{dots}}
#' @export
impute_fn_knn <- function(values, distances, k, kern) {
  ranks <- order(distances)
  smallest_distances <- distances[ranks][1:k]
  knn_values <- values[ranks][1:k]
  # calculate weights
  # d <- kernelMatrix(kern, c(0, smallest_distances))[1, -1, drop= TRUE]
  # 9/22: rather than use kernlab, calculate kernels by hand
  d <- kern_wt(kern, c(0, smallest_distances))
  knn_weights <- d / sum(d)
  
  weighted.mean(knn_values, knn_weights)
}


# this function is equivalent to calculating via
# kernelMatrix(kern, c(0, smallest_distances))[1, -1, drop= TRUE]
kern_wt <- function (kernel, x) {
  if (is(x, "vector")) x <- as.matrix(x)
  
  sigma = kpar(kernel)$sigma
  n <- dim(x)[1]
  dota <- rowSums(x * x)/2
  return(exp(2 * sigma * (-dota))[-1])
}
  
 