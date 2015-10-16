
library(testthat)
library(imputation)

context("kNN_impute.no_canopies")

test_that("errors work correctly", {
  x1 <- matrix(rnorm(100), 10, 10)
  x2 <- matrix(letters[1:25], 5,5)
  x3 <- rnorm(100)
  
  expect_error(kNN_impute.no_canopies(x1, k= 0))
  expect_error(kNN_impute.no_canopies(x1, k= nrow(x1) + 1))
  expect_error(kNN_impute.no_canopies(x1, k= 5.5))
  expect_error(kNN_impute.no_canopies(x1, k= 5, q= -1))
  expect_error(kNN_impute.no_canopies(x1, k= 5, q= 0))
  expect_error(kNN_impute.no_canopies(x1, k= 5, q= 2.5))
  expect_error(kNN_impute.no_canopies(x1, k= 5, q= -1))
  expect_error(kNN_impute.no_canopies(x2, k= 2))
  expect_error(kNN_impute.no_canopies(x3, k= 2))
  expect_error(kNN_impute.no_canopies(x1, k= 5, q= 1, parallel= TRUE, leave_cores= -1))
  expect_error(kNN_impute.no_canopies(x1, k= 5, q= 1, parallel= TRUE, leave_cores= 2.5))
  
  ## trivial case equality -- no missing
  rownames(x1) <- 1:nrow(x1)
  expect_equal(kNN_impute.no_canopies(x1, k= 3), x1)
  expect_equal(kNN_impute.no_canopies(x1, k= 3, parallel= FALSE), x1)
  
  # column NA
  x1[,1] <- NA
  expect_error(kNN_impute.no_canopies(x1, k=3, q=2, parallel= FALSE))
  expect_error(kNN_impute.no_canopies(x1, k=3, q=2, parallel= TRUE))
})


test_that("simple cases work correctly", {
  x1 <- matrix(rnorm(100), 10, 10)
  x1[sample(1:100, size= 10, replace= FALSE)] <- NA
  x2 <- x1; x2[5,] <- NA
  
  # NA rows imputed to colMeans
  expect_equal(kNN_impute.no_canopies(x2, k=3, q=2, parallel= FALSE, verbose= FALSE)$x[5,],
               colMeans(x2, na.rm=T))
  expect_equal(kNN_impute.no_canopies(x2, k=3, q=2, parallel= TRUE, verbose= FALSE)$x[5,],
               colMeans(x2, na.rm=T))
  
  # parallel equivalence
  expect_equal(kNN_impute.no_canopies(x1, k=5, q=2, parallel= FALSE, verbose= FALSE),
               kNN_impute.no_canopies(x1, k=5, q=2, parallel= TRUE, verbose= FALSE))
  expect_equal(kNN_impute.no_canopies(x1, k=3, q=2, parallel= FALSE, verbose= FALSE),
               kNN_impute.no_canopies(x1, k=3, q=2, parallel= TRUE, verbose= FALSE))
  expect_equal(kNN_impute.no_canopies(x1, k=3, q=4, parallel= FALSE, verbose= FALSE),
               kNN_impute.no_canopies(x1, k=3, q=4, parallel= TRUE, verbose= FALSE))
  
  # returns matrix
  expect_true(is.matrix(kNN_impute.no_canopies(x1, k=3, q=2, parallel= FALSE, verbose= FALSE)$x))
  #expect_true(is.matrix(kNN_impute.no_canopies(x1, k=3, q=2, parallel= TRUE, verbose= FALSE)$x))
  
})