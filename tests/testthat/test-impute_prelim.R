
library(testthat)
library(imputation)

context("impute_prelim")

test_that("no missing: returns accurately", {
  x1 <- matrix(rnorm(100), 10, 10)
  
  expect_null(impute_prelim(x1)$missing_rows_indices)
  expect_null(impute_prelim(x1)$missing_cols_indices)
  expect_null(impute_prelim(x1)$x_missing)
  expect_equal(impute_prelim(x1)$numMissing, 0)
  
  expect_null(impute_prelim(x1, parallel= TRUE)$missing_rows_indices)
  expect_null(impute_prelim(x1, parallel= TRUE)$missing_cols_indices)
  expect_null(impute_prelim(x1, parallel= TRUE)$x_missing)
  expect_equal(impute_prelim(x1, parallel= TRUE)$numMissing, 0)
  
})

test_that("missing: returns accurately", {
  x1 <- matrix(rnorm(100), 10, 10)
  x1[sample(1:100, size= 10, replace= FALSE)] <- NA
  
  miss_rows <- which(apply(x1, 1, function(x) any(is.na(x))))
  
  
  expect_equal(impute_prelim(x1)$numMissing, 10)
  expect_equal(impute_prelim(x1)$missing_rows_indices, 
               miss_rows)
  expect_equal(impute_prelim(x1)$missing_cols_indices, 
               which(apply(x1, 2, function(x) any(is.na(x)))))
  expect_equal(impute_prelim(x1, parallel= TRUE)$missing_rows_indices, 
               miss_rows)
  expect_equal(impute_prelim(x1, parallel= TRUE)$missing_cols_indices, 
               which(apply(x1, 2, function(x) any(is.na(x)))))
  expect_equal(dim(impute_prelim(x1)$x_missing), c(length(miss_rows), ncol(x1) + 1))
  expect_equal(dim(impute_prelim(x1, parallel= TRUE)$x_missing), 
               c(length(miss_rows), ncol(x1) + 1))
  
  names(miss_rows) <- miss_rows
  expect_equal(impute_prelim(x1)$x_missing[,1, drop= TRUE],
               miss_rows)
})