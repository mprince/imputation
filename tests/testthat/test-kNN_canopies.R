
library(testthat)
library(imputation)

context("kNN_impute.canopies")

test_that("errors work correctly", {
  x1 <- matrix(rnorm(100), 10, 10)
  x2 <- matrix(letters[1:25], 5,5)
  x3 <- rnorm(100)
  
  expect_error(kNN_impute.canopies(x1, k= 0))
  expect_error(kNN_impute.canopies(x1, k= nrow(x1) + 1))
  expect_error(kNN_impute.canopies(x1, k= 5.5))
  expect_error(kNN_impute.canopies(x1, k= 5, q= -1))
  expect_error(kNN_impute.canopies(x1, k= 5, q= 0))
  expect_error(kNN_impute.canopies(x1, k= 5, q= 2.5))
  expect_error(kNN_impute.canopies(x1, k= 5, q= -1))
  expect_error(kNN_impute.canopies(x2, k= 2))
  expect_error(kNN_impute.canopies(x3, k= 2))
  expect_error(kNN_impute.canopies(x1, k= 5, q= 1, parallel= TRUE, leave_cores= -1))
  expect_error(kNN_impute.canopies(x1, k= 5, q= 1, parallel= TRUE, leave_cores= 2.5))
  expect_error(kNN_impute.canopies(x1, k= 5, q= 1, parallel= TRUE, leave_cores= 2, n_canopies= -1))
  expect_error(kNN_impute.canopies(x1, k= 5, q= 1, parallel= TRUE, leave_cores= 2, n_canopies= 1))
  expect_error(kNN_impute.canopies(x1, k= 5, q= 1, parallel= TRUE, leave_cores= 2, n_canopies= 5.5))
})

test_that("trivial cases work", {
  x1 <- matrix(rnorm(200), 20, 10)
  x2 <- create_canopies(x1, n_canopies= 5, q= 2)
  
  ## trivial case equality -- no missing
  k1 <- lapply(x2, kNN_impute.canopies, k= 3, verbose= FALSE, n_canopies= 5)
  k1 <- do.call("rbind", k1)
  k1 <- k1[order(as.integer(rownames(k1))),]
  k1 <- k1[, -ncol(k1)]
  
  k2 <- lapply(x2, kNN_impute.canopies, k= 3, verbose= FALSE, parallel= FALSE, n_canopies= 5)
  k2 <- do.call("rbind", k2)
  k2 <- k2[order(as.integer(rownames(k2))),]
  k2 <- k2[, -ncol(k2)]
  
  attributes(k1) <- NULL
  attributes(k2) <- NULL
  attributes(x1) <- NULL
  expect_equal(k1, x1)
  expect_equal(k2, x1)
  
  # column NA
  x1 <- matrix(rnorm(200), 20, 10)
  x1[,1] <- NA
  expect_error(kNN_impute.canopies(x1, k=3, q=2, parallel= FALSE, n_canopies= 5))
  expect_error(kNN_impute.canopies(x1, k=3, q=2, parallel= TRUE, n_canopies= 5))
  
})


test_that("simple cases work correctly", {
  x1 <- matrix(rnorm(200), 20, 10)
  x1[x1 > 1.25] <- NA
  x3 <- create_canopies(x1, n_canopies= 5, q= 2)
  
  # testing function
  can5 <- function(l, k, q, ...) {
    l2 <- lapply(l, kNN_impute.canopies, k= k, q=q, verbose= FALSE, n_canopies= 5, ... = ...)
    l2 <- do.call("rbind", lapply(l2, "[[", 1))
    l2 <- l2[, -ncol(l2)]
    return(l2[order(as.integer(rownames(l2))),])
  }
  
  # parallel equivalence
  expect_equal(can5(x3, k=3, q=2, parallel= FALSE), can5(x3, k=3, q=2, parallel= TRUE))
  expect_equal(can5(x3, k=2, q=2, parallel= FALSE), can5(x3, k=2, q=2, parallel= TRUE))
  expect_equal(can5(x3, k=3, q=4, parallel= FALSE), can5(x3, k=3, q=4, parallel= TRUE))
  
  # returns matrix
  expect_true(is.matrix(can5(x3, k=3, q=2, parallel= TRUE)))
  expect_true(is.matrix(can5(x3, k=3, q=2, parallel= FALSE)))
  
  
  # NA rows imputed to colMeans 
  x2 <- x1; x2[5,] <- NA
  x3 <- create_canopies(x2, n_canopies= 5, q= 2)
  
  expect_warning(lapply(x3, kNN_impute.canopies, k= 3, q=2, 
                        verbose= FALSE, n_canopies= 5, parallel= FALSE))
  expect_warning(lapply(x3, kNN_impute.canopies, k= 3, q=2, 
                        verbose= FALSE, n_canopies= 5, parallel= TRUE))
})
