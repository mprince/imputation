
#' @title Calculate \eqn{L_q} distance of two vectors
#' @description Calculate \eqn{L_q} distance of two vectors
#' @param x A numeric vector. Missing values are allowed.
#' @param y A numeric vector. Missing values are allowed.
#' @param q An integer specifying the which norm to take the L-q distance of.
#' @return a scalar
#' @export
dist_q <- function(x, y, q= 2) {
  if (!is.numeric(x) | !is.numeric(y)) stop("Both x and y must be numeric.")
  if (q < 1 | q %% 1 != 0) stop("q must be an integer >= 1")
  
  x_obs <- !is.na(x)
  y_obs <- !is.na(y)
  m <- sum(x_obs * y_obs)
  
  return((1 / m * sum(abs(x - y)^q, na.rm=TRUE))^(1/q))
}

#' @title Calculate \eqn{L_q} distance 
#' @description Calculate \eqn{L_q} distance of all vectors in a matrix to a reference
#' vector.
#' @param x A numeric matrix Missing values are allowed.
#' @param ref An integer specifying the reference row.
#' @param q An integer specifying the which norm to take the L-q distance of.
#' @return a numeric vector of length \code{nrow(x) - 1}
#' @export
dist_q.matrix <- function(x, ref= 1, q= 2) {
  if (!is.numeric(x) | !is.matrix(x)) stop("x must be a numeric matrix.")
  if (ref < 1 | ref > nrow(x) | ref %% 1 != 0) 
    stop("ref must be an ingeter in {1, nrow(x)}.")
  if (q < 1 | q %% 1 != 0) stop("q must be an integer >= 1")
  
  x_ref <- x[ref,]
  x_rest <- x[-ref,]
  
  return(apply(x_rest, 1, dist_q, y= x_ref, q= q))
}



# @param x a data frame or matrix where each row represents a different record
impute_prelim = function(x, parallel= FALSE, leave_cores= 2) {
  
  # 00. get some initial statistics on missingness.
  numMissing = sum(is.na(x)) 
  
  if(numMissing == 0) {
    return(list (numMissing = numMissing,
                 missing_rows_indices = NULL,
                 missing_cols_indices = NULL,
                 x_missing = NULL))
  }
  
  if (parallel == FALSE) {
    missing_rows_indices = which(apply(x, 1, function(i) {
      any(is.na(i))
    }))
    missing_cols_indices = which(apply(x, 2, function(j) {
      any(is.na(j))
    }))
  } else {
    cl <- makeCluster(detectCores() - leave_cores)
    
    missing_rows_indices = which(parRapply(cl= cl, x, function(i) {
      any(is.na(i))
    }))
    missing_cols_indices = which(parCapply(cl= cl, x, function(j) {
      any(is.na(j))
    }))
    
    stopCluster(cl)
  }
  
  # 01. add a row identifier to x[, <missing columns>]
  x_missing = cbind(1:nrow(x),x)[missing_rows_indices,,drop=F]
  
  # 02. return
  return(list (numMissing = numMissing,
               missing_rows_indices = missing_rows_indices,
               missing_cols_indices = missing_cols_indices,
               x_missing = x_missing))
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
    d <- dim(mat)
    i <- ifelse(n %% d[1] == 0, d[1], n %% d[1])
    j <- ceiling(n / d[2])
    ret <- data.frame(i= i, j= j, alpha= mat[n])
    return(ret[order(ret$i),])
  } else {return(NULL)}
}