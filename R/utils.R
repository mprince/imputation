

# @param x a data frame or matrix where each row represents a different record
# @param verbose Logical. If \code{TRUE} output some statistics on imputation size.
impute.prelim = function(x, verbose=F) {
  
  # 00. get some initial statistics on missingness.
  missing.matrix = is.na(x)
  numMissing = sum(missing.matrix) 
  if(verbose) {
    print(paste("imputing on", numMissing, "missing values with matrix size",
      nrow(x)*ncol(x), sep=" "))
  }
  if(numMissing == 0) {
    return ( list (missing.matrix = missing.matrix,
                   numMissing = numMissing,
                   missing.rows.indices = NULL,
                   missing.cols.indices = NULL,
                   x.missing = NULL) )
  }

  missing.rows.indices = which(apply(missing.matrix, 1, function(i) {
    any(i)
  }))
  missing.cols.indices = which(apply(missing.matrix, 2, function(i) {
    any(i)
  }))
  
  # 01. create x.missing
  x.missing = rbind(1:ncol(x),x)[,missing.cols.indices,drop=F]
  
  # 02. return
  return(list(missing.matrix= missing.matrix,
                 numMissing= numMissing,
                 missing.rows.indices= missing.rows.indices,
                 missing.cols.indices= missing.cols.indices,
                 x.missing= x.missing))
}

# @title Imputation function
# @description Function for KNN imputation via weighted Euclidean distance
# @param values 
impute.fn <- function(values, distances, k) {
  ranks = order(distances)
  smallest.distances = distances[ranks]
  #values corresponding to smallest distances
  knn.values = values[ranks][1:k]
  knn.weights = 1 - (smallest.distances / max(distances)) [1:k]
  weighted.mean(knn.values, knn.weights)
}

### cross-validation impute.prelim
cv.impute.prelim = function(x, test.fraction = 1/3) {
  n = nrow(x) * ncol(x)
  missing.matrix = is.na(x)
  valid.data = which(!missing.matrix)

  remove.indices = sample(valid.data, test.fraction*length(valid.data))
  x.train = x; x.train[remove.indices] = NA

  return (list(remove.indices = remove.indices,
               x.train = x.train))
}

### pairwise tests of a dataset's columns for equal variance
var_tests <- function(x, bonf=TRUE) {
  p <- ncol(x)
  ntests <- choose(p,2)
  ret <- matrix(NA, ncol= p, nrow= p)
  for (i in 1:(p-1)) {
    for (j in (i+1):p) {
      # fill lower triangular
      ret[j,i] <- var.test(x[,i], x[,j])$p.val
    }
  }
  if (bonf == TRUE) ret2 <- calc_i_j(ret, alpha= 0.05 / choose(p,2))
  else ret2 <- calc_i_j(ret, alpha= 0.05)
  return(unequal_scaled_vars= ret2)
}


calc_i_j <- function(mat, alpha= 0.05) {
  n <- which(mat < alpha)
  if (length(n) > 0) {
    d <- dim(x)
    i <- ifelse(n %% d[1] == 0, d[1], n %% d[1])
    j <- ceiling(n / d[2])
    ret <- data.frame(i= i, j= j, alpha= mat[n])
    return(ret[order(ret$i),])
  } else {return(NULL)}
}