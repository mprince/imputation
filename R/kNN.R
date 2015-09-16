#' kNN Impute
#'
#' Imputation using k-nearest neighbors.
#' For each record, identify missinng features.  For each missing feature
#' find the \eqn{k} nearest neighbors which have that feature.  Impute the missing
#' value using the imputation function on the k-length vector of values
#' found from the neighbors.
#' 
#' The default \code{impute_fn} weighs the \eqn{k} values by their respective distances.
#'   The result is the weighted mean of the values of the nearest neighbors 
#'   with weights based on their distance.  It is implemented as follows:
#' \preformatted{impute_fn = function(values, distances, k) {
#'   ranks = order(distances)
#'   smallest.distances = distances[ranks][1:k]
#'   knn.values = values[ranks][1:k]
#'   knn.weights = 1 - (smallest.distances / max(distances))
#'   weighted.mean(knn.values, knn.weights)
#' }}
#' Alternatively, a simple mean can be implemented as follows:
#' \preformatted{impute_fn = function(values, distances, k) {
#'   ranks = order(distances)
#'   mean(distances[ranks][1:k])
#' }}
#' 
#' @param x a \code{matrix} or \code{data.frame} which can be coerced to a matrix
#'  where each row represents a different record
#' @param k the number of neighbors to use for imputation
#' @param x.dist Optional. A pre-computed distance matrix to be used for kNN
#' @param impute_fn The imputation function to run on the length k vector of values for
#'   a missing feature.  Defaults to weighted mean-KNN; see Details. 
#' @param verbose if \code{TRUE} print status updates
#' @param check.scale Logical. If \code{TRUE} compute pairwise variance tests to see if
#' variables are on a common scale. Bonferroni correction applied.
#' @references Missing value estimation methods for DNA microarrays.  Troyanskaya et al.
#' @examples
#'   x = matrix(rnorm(100),10,10)
#'   x.missing = x > 1
#'   x[x.missing] = NA
#'   kNNImpute(x, 3)
#' @export
kNNImpute = function(x, k, x.dist = NULL, impute_fn, verbose=TRUE, check.scale= TRUE) {
  
  # 01a. Do some preliminaries
  #--------------------------------------------------------
  #kernal <- match.arg(kernal, several.ok= FALSE)
  if (is.data.frame(x)) x <- as.matrix(x)
  if (!is.matrix(x)) stop("x should be a numeric data matrix")
  if(k >= nrow(x)) stop("k must be less than the number of rows in x")

  prelim = impute.prelim(x)
  if (prelim$numMissing == 0) return (x) # no missing

  if (missing(impute_fn)) {
    impute_fn <- imputation:::impute_fn_knn
  }
  
  col_na <- apply(x, 2, function(j) all(is.na(j)))
  row_na <- apply(x, 1, function(i) all(is.na(i)))
  
  if (any(col_na)) {
    cat("column(s)", which(col_na), "are entirely missing.")
    stop("Please fix missing columns.")
  }
  
  # 01b. Test if variables on same scale
  #--------------------------------------------------------
  unequal_var <- var_tests(x, bonf= TRUE)
  if (nrow(unequal_var) > 0) warning(paste("Some variables appear to have unequal variances.",
                                           "KNN is best with equally scaled variables."))
  
  
  # 01c. Compute distance matrix if needed
  #--------------------------------------------------------
  if (verbose) print("Computing distance matrix...")
  if (is.null(x.dist)) x.dist = dist(x)
  if (verbose) print("Distance matrix complete")
  
  # 01d. Get Kernal
  #--------------------------------------------------------
#   if (kernal= "rbfdot") {
#     kern <- rbfdot(sigma= 1)
#   }
#   else if (kernal == "vanilla") {
#     kern <- vanilladot()
#   }
  
  # 02a. Impute missing rows to complete-data column means 
  #--------------------------------------------------------
  if (any(row_na)) {
    cat("row(s)", which(row_na), "are entirely missing.")
    warning("These row(s)' values will be imputed to column means.")
    
    col_means <- colMeans(x, na.rm=T)
    for (i in row_na) {
      x[i,] <- col_means
    }
  }
  
  # 02b. Impute
  #--------------------------------------------------------
  x.missing.imputed <- apply(prelim$x.missing, 1, function(i) {
    rowIndex = as.numeric(i[1])
    i.original = unlist(i[-1])
    
    if(verbose) print(paste("Imputing row", rowIndex,sep=" "))
    
    missing.cols = which(prelm$missing.matrix[rowIndex,])
    
    if(length(missing.cols) == ncol(x))
      warning( paste("Row",rowIndex,"is completely missing",sep=" ") )
    
    imputed.values = sapply(missing.cols, function(j) {
      #find neighbors that have data on the jth column
      neighbor.indices = which(!prelim$missing.matrix[,j])

      #lookup the distance to these neighbors
      #order the neighbors to find the closest ones
      if (!is.null(x.dist)) {
        indices.1d = .dist.2dto1d(rowIndex, neighbor.indices, nrow(x))
        knn.dist = x.dist[indices.1d]
      }
      else {
        knn.dist = pdist::pdist(x, indices.A = rowIndex,
                            indices.B = neighbor.indices)@dist
      }
      return(impute_fn(x[neighbor.indices, j], knn.dist, k))
    })
    i.original[missing.cols] = imputed.values
    return(i.original)
  })
  x[prelim$missing.rows.indices,] = x.missing.imputed

  #Things that were not able to be imputed are set to 0
  num_errors = sum(is.na(x))
  if (num_errors > 0) {
    return(list(x=x, missing_matrix=missing.matrix, num_errors= num_errors))
  } else {
    return(list(x=x, missing_matrix=missing.matrix))  
  }
  
}


#' 2D indices to 1D indices
#'
#' Helper function to convert 2D indices to 1D indices.
#' The return value of the function dist does not by default return a
#' 2D object, instead it returns an array.  When wanting to access an
#' element at the i,jth position of the distance matrix, this function
#' converts the 2D index to a 1D index that can be used on the distance array
.dist.2dto1d = function(i,j,n) {
  ret = rep(0, length(j))
  j.larger.indices = which(j > i)
  j.smaller.indices = which(j < i)
  if (length(j.larger.indices) > 0) {
    j.larger = j[j.larger.indices]
    ret[j.larger.indices] = (i-1)*n - i^2/2 + j.larger - i/2
  }
  if (length(j.smaller.indices) > 0) {
    j.smaller = j[j.smaller.indices]
    ret[j.smaller.indices] = (j.smaller-1)*n - j.smaller^2/2 + i - j.smaller/2
  }
  ret
}
