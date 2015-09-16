

# @title Imputation function for kNN
# @description Function for KNN imputation. Distances are weighted by a kernal function
# @param values 
# @seealso \code{\link[kernlab]{dots}}}
impute_fn_knn <- function(values, distances, k) {
  ranks <- order(distances)
  smallest.distances <- distances[ranks]
  #values corresponding to smallest distances
  knn.values = values[ranks][1:k]
  
  knn.weights = 1 - (smallest.distances / max(distances)) [1:k]
  weighted.mean(knn.values, knn.weights)
}