
#' @title Calculate \eqn{L_q} distance 
#' @description Calculate \eqn{L_q} distance of all vectors in a matrix to a reference
#' vector.
#' @param x A numeric matrix Missing values are allowed.
#' @param ref An integer specifying the reference row.
#' @param q An integer specifying the which norm to take the L-q distance of.
#' @return a numeric vector of length \code{nrow(x) - 1}
#' @export
dist_q.matrix <- function(x, ref= 1L, q= 2) {
  if (!is.numeric(x) | !is.matrix(x)) stop("x must be a numeric matrix.")
  if (ref < 1 | ref > nrow(x) | ref %% 1 != 0) 
    stop("ref must be an integer in {1, nrow(x)}.")
  if (q < 1 | q %% 1 != 0) stop("q must be an integer >= 1")
  
  x_ref <- x[ref,]
  x_rest <- x[-ref,]
  
  return(.Call("imputation_dist_q_matrix", PACKAGE= 'imputation', x_ref, x_rest, q))
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