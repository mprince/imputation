
#' @title Imputation using weighted-kNN
#' @description Imputation using weighted k-nearest neighbors.
#' For each record, identify missinng features.  For each missing feature
#' find the \eqn{k} nearest neighbors which have that feature.  Impute the missing
#' value using the \eqn{k} nearest neighbors having that feature. Weights are computed
#' using a Gaussian kernal bandwidth parameter using 'Silverman's rule of thumb'
#' as described by Silverman (1998)
#' @section Details:
#' Imputation can be done on all observations in a single group or via overlapping
#' canopies (eg. subsets). Canopies are based on distance to the dataset centroid.
#' The use of canopies allows for reducing the time complexity of kNN by reducing 
#' it from 1 large problem to several smaller problems. In general, since canopies 
#' overlap with their neighbors, the use of canopies reduces the time complexity 
#' from  \eqn{2^{O(n)}} to approximately \eqn{2^{O(9n / c)}} where c is the number
#' of canopies. Since, in large datasets, c can be quite large, this is a substantial 
#' savings.
#' 
#' In small datasets (eg. roughly < 100000), canopies are not recommended. Canopies produce
#' an approximate solution although they may produce an equivalent solution. Equivalence
#' is guaranteed under the following condition. If for all observations x with k nearest 
#' neighbors, the canopy containing x also contains all k nearest neighbors. This should
#' be the case when distance to each ovservation x is highly correlated to distance of
#' each observation to the dataset centroid.
#' @seealso \code{\link{create_canopies}}, \code{\link{kNN_impute.default}}.
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
#' @param n_subsets if a positive integer > 1, kNN imputation will be calculated on data subsets 
#' which are created in the data \code{x} based on a cheap distance metric. See details 
#' @references "Improved Methods for the Impution of Missing Data by Nearest Neighbors Methods"
#' Tutz and Ramzan (2015)
#' @references McCallum, Andrew, Kamal Nigam, and Lyle H. Ungar. 
#' "Efficient clustering of high-dimensional data sets with application to reference matching." 
#' Proceedings of the sixth ACM SIGKDD international conference on Knowledge 
#' discovery and data mining. ACM, 2000.
#' @examples
#'   x = matrix(rnorm(100),10,10)
#'   x[x > 1] = NA
#'   kNN_impute(x, k=3, q=2)
#' @export

kNN_impute <- function(x, k, q= 2, verbose=TRUE, check_scale= TRUE,
                       parallel= TRUE, leave_cores= ifelse(detectCores() <= 4, 1, 2),
                       n_canopies= NULL) {
  
  # 01. Do some preliminaries
  #--------------------------------------------------------
  if (parallel == TRUE) {
    if (leave_cores < 0 | leave_cores > detectCores() | leave_cores %% 1 != 0) {
      stop("leave_cores must be an integer between 0 (not recommended) 
           and ", detectCores())
    }
  }
  
  if (is.data.frame(x)) x <- as.matrix(x)
  if (!is.numeric(x) | !is.matrix(x)) stop("x should be a numeric data matrix")
  if(k < 1 | k >= nrow(x) | k %% 1 != 0) stop("k must be an integer in {1, nrow(x) - 1}")
  if (q < 1 | q %% 1 != 0) stop("q must be an integer >= 1")
  
  col_na <- apply(x, 2, function(j) all(is.na(j)))
  row_na <- apply(x, 1, function(i) all(is.na(i)))
  
  if (any(col_na)) {
    cat("column(s)", which(col_na), "are entirely missing.")
    stop("Please fix missing columns.")
  }
  
  # [10/13] add rownames such that impute_prelim() can work with or without canopies
  # and so that impute_fn_knn_all.** work properly
  if (is.null(rownames(x))) {
    rownames(x) <- 1:nrow(x)
    has_rownames <- FALSE
  } else {
    row_key <- data.frame(key= rownames(x), id= 1:nrow(x))
    rownames(x) <- 1:nrow(x)
    has_rownames <- TRUE
  }
  
  if (is.null(n_canopies)) { 
    # 02. Send to kNN_impute.default w/o canopies:
    #--------------------------------------------------------
    knn <- kNN_impute.no_canopies(x= x, k= k, q= q, verbose= verbose, 
                      check_scale= check_scale,
                      parallel= parallel, leave_cores= leave_cores)
    # check for rownames and exit
    if (has_rownames) {
      row_key <- row_key[rownames(knn$x),]
      rownames(knn$x) <- row_key$key
      return(knn)
    } else {
      return(knn)
    }
  } else {
    # 03. Send to kNN_impute.canopies w/ canopies
    #--------------------------------------------------------
    if (n_canopies <= 3 | n_canopies %% 1 != 0) stop("n_canopies must be an integer > 3")
    
    # verbose option
    if (verbose) print("Computing canopies kNN solution provided within canopies")
    # create canopies
    x <- create_canopies(x, n_canopies= n_canopies, q= q)
    if (verbose) print("Canopies complete... calculating kNN.")
    
    knn <- lapply(x, kNN_impute.canopies, k= k, q= q, verbose= verbose, 
                  check_scale= check_scale,
                  parallel= parallel, leave_cores= leave_cores, n_canopies= n_canopies)
    
    ### 03b. combine and return results
    if (all(sapply(knn, is.list)) & any(lapply(knn, length) == 2)) { # have some errors
      which_err <- which(lapply(knn, length) == 2)
      num_errors <- sum(do.call("c", lapply(knn[which_err], "[[", 2)))  
      
      x_return <- do.call("rbind", lapply(knn, function(l) return(l$x)))
      x_return <- x_return[order(as.integer(rownames(x_return))),]
      x_return <- x_return[, -ncol(x_return)]
      # check for rownames and exit
      if (has_rownames) {
        row_key <- row_key[rownames(x_return),]
        rownames(x_return) <- row_key$key
        return(list(x=x_return, num_errors= num_errors))
      } else {
        return(list(x=x_return, num_errors= num_errors))
      }
    } 
    else { # no errors
      x_return <- do.call("rbind", lapply(knn, function(l) return(l$x)))
      x_return <- x_return[order(as.integer(rownames(x_return))),]
      x_return <- x_return[, -ncol(x_return)]
      # check for rownames and exit
      if (has_rownames) {
        row_key <- row_key[rownames(x_return),]
        rownames(x_return) <- row_key$key
        return(list(x=x_return))
      } else {
        return(list(x=x_return))
      }
    }
  }
}

