

#' @title Imputation function for kNN
#' @description Function for KNN imputation for a single element. 
#' Distances are weighted by a kernal function to produce a weighted 
#' imputation.
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
  
  sigma = kernlab::kpar(kernel)$sigma
  n <- dim(x)[1]
  dota <- rowSums(x * x)/2
  return(exp(2 * sigma * (-dota))[-1])
}
  
# @description wrapper to impute_fn_knn for all observations and all columns.
# This function is not parallelized
# @param x_missing From impute_prelim(...)$x_missing
# @param x_complete The complete data matrix x
# @param k ... input to kNN_impute
# @param q ... input to kNN_impute
# @param kern ... calculated inside kNN_impute
# @param verbose if \code{TRUE} print status updates
impute_fn_knn_all.nonPar <- function(x_missing, x_complete, k, q, kern,
                                     verbose) {
  # impute row-by-row -- non parallel
  x_missing_imputed <- t(apply(x_missing, 1, function(i) {
    rowID = as.numeric(i[1])
    i_original = unlist(i[-1])
    x_comp_rowID <- which(as.integer(rownames(x_complete)) == rowID)
    # verbose option
    if(verbose) {print(paste("Imputing row", rowID, sep=" "))}
    missing_cols <- which(is.na(x_complete[x_comp_rowID,]))
    
    # calculate distances
    
    distances <- dist_q.matrix(rbind(x_complete[x_comp_rowID, ], x_complete[-x_comp_rowID,]), 
                               ref= 1, q= q)
    
    # within the given row, impute by column
    imputed_values <- unlist(lapply(missing_cols, function(j, distances) {
      # which neighbors have data on column j?
      neighbor_indices = which(!is.na(x_complete)[,j])
      # impute
      return(impute_fn_knn(x_complete[neighbor_indices, j], distances[neighbor_indices], 
                           k=k, kern= kern))
    }, distances= distances))
    i_original[missing_cols] <- imputed_values
    return(i_original)
  }))
  return(x_missing_imputed)
}




# @description wrapper to impute_fn_knn for all observations and all columns.
# This function is parallelized
# @param x_missing From impute_prelim(...)$x_missing
# @param x_complete The complete data matrix x
# @param k ... input to kNN_impute
# @param q ... input to kNN_impute
# @param kern ... calculated inside kNN_impute
# @param leave_cores How many cores do you wish to leave open to other processing?
impute_fn_knn_all.Par <- function(x_missing, x_complete, k, q, kern,
                                  leave_cores) { 
  # impute row-by-row -- parallel 
  cl <- makeCluster(detectCores() - leave_cores)
  
  x_missing_imputed <- parRapply(cl= cl, x_missing, function(i) {
    rowID = as.numeric(i[1])
    i_original = unlist(i[-1])
    x_comp_rowID <- which(as.integer(rownames(x_complete)) == rowID)
    missing_cols <- which(is.na(x_complete[x_comp_rowID,]))
    
    # calculate distances
    distances <- dist_q.matrix(x=rbind(x_complete[x_comp_rowID, ], x_complete[-x_comp_rowID,]), ref= 1, 
                               q= q)
    
    # within the given row, impute by column
    imputed_values <- unlist(lapply(missing_cols, function(j, distances) {
      # which neighbors have data on column j?
      neighbor_indices = which(!is.na(x_complete)[,j])
      # impute
      return(impute_fn_knn(x_complete[neighbor_indices, j], distances[neighbor_indices], 
                           k=k, kern= kern))
    }, distances= distances))
    i_original[missing_cols] <- imputed_values
    return(i_original)
  })
  stopCluster(cl)
  x_missing_imputed <- matrix(x_missing_imputed, nrow= dim(x_missing)[1],
                              ncol= dim(x_missing)[2] - 1, byrow= TRUE)
  return(x_missing_imputed)
}
