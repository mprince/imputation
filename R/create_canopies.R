#' @title Create canopies within a dataset
#' @description Compute high level splits of a dataset (ie- subsets or canopies) 
#' based on L-q distance to the dataset centroid. Use of canopies allows for reducing
#' the time complexity of kNN by reducing it from 1 large \eqn{2^{O(n)}} problem
#' to several smaller problems.
#' @param x a \code{matrix} or \code{data.frame} which can be coerced to a matrix
#'  where each row represents a different record.
#' @param n_canopies An integer specifying how many canopies (ie- subsets) to create.
#' @param q An integer specifying the which norm to take the L-q distance of.
#' @return A list of subset datasets
#' 
#' @section Details:
#' The dataset is split into \code{n_canopies}. Canopies are created to be overlapping 
#' with their neighbors. Specifically, let the calculated distance to the dataset centroid
#' be split into c ordered groups \eqn{c = 1, \ldots, n_canopies}. Define canopy 1 as all
#' observations of x with distance in \eqn{c_1 \cup c_2}, canopy n_canopies as all 
#' observations of x with distance in \eqn{c_{n_canopies - 1} \cup c_{n_canopies}}, 
#' and all other canopies \eqn{j \in [2, n_canopies - 1]} as the observations of x 
#' with distance in \eqn{c_{i-1} \cup c_i \cup c_{i+1}}.
#' 
#' @references McCallum, Andrew, Kamal Nigam, and Lyle H. Ungar. 
#' "Efficient clustering of high-dimensional data sets with application to reference matching." 
#' Proceedings of the sixth ACM SIGKDD international conference on Knowledge 
#' discovery and data mining. ACM, 2000.
#' @export
create_canopies <- function(x, n_canopies, q) {
  # error checking
  if (q < 1 | q %% 1 != 0) stop("q must be an integer >= 1")
  if (n_canopies <= 3 | n_canopies %% 1 != 0) stop("n_canopies must be an integer > 3")
  if (is.data.frame(x)) x <- as.matrix(x)
  if (!is.numeric(x) | !is.matrix(x)) stop("x should be a numeric data matrix")
  
  # calculate distance to data centroid and create canopies
  col_means <- colMeans(x, na.rm=T)
  mean_dist <- dist_q.matrix(x= rbind(col_means, x), ref= 1L, q= q)
  
  # if any missing rows, denote missing distance as VERY LARGE:
  mean_dist <- ifelse(is.na(mean_dist) | is.nan(mean_dist), 10e5, mean_dist)
  
  # create canopies based on distance to centroid, missing rows in top canopy
  mean_dist_q <- quantile(mean_dist, seq(0,1, 1 / (n_canopies)))
  x2 <- data.frame(x, d_factor= cut(mean_dist, breaks= mean_dist_q, include.lowest= TRUE,
                                    labels= FALSE))
  
  out <- vector("list", length= n_canopies)
  for (i in 1:n_canopies) {
    out[[i]] <- x2[x2$d_factor %in% c(i-1, i, i+1),]
    out[[i]] <- as.matrix(out[[i]])
  }
  # return
  return(out)
}



