
library(testthat)
library(imputation)

context("create_canopies")


test_that("errors work", {
  x1 <- matrix(rnorm(100), 10, 10)
  x2 <- rnorm(100)
  x3 <- as.data.frame(x1)
  
  expect_error(create_canopies(x2))
  expect_error(create_canopies(x1, n_canopies= 3))
  expect_error(create_canopies(x1, n_canopies= -1))
  expect_error(create_canopies(x1, n_canopies= 5.5))
  expect_error(create_canopies(x1, n_canopies= 4, q= -1))
  expect_error(create_canopies(x1, n_canopies= 4, q= 2.2))
  expect_error(create_canopies(x1, n_canopies= 4, q= 0))
  
})

test_that("splitting works as expected", {
  x1 <- matrix(rnorm(200), 20, 10)
  x1[x1 > 1.25] <- NA
  
  expect_true(is.list(create_canopies(x1, n_canopies= 4, q=2)))
  expect_equal(dim(do.call("rbind", create_canopies(x1, n_canopies= 4, q=2))),
               dim(x1) * c(2.5, 1) + c(0,1))
  expect_equal(dim(do.call("rbind", create_canopies(x1, n_canopies= 10, q=2))),
               c(4 * nrow(x1) / 10 + 3 * (10-2) * nrow(x1) / 10, ncol(x1) + 1))
  expect_equal(sapply(create_canopies(x1, n_canopies= 4, q=2), function(x) dim(x)[2]),
               rep(ncol(x1) + 1, 4))
  expect_equal(sapply(create_canopies(x1, n_canopies= 4, q=2), function(x) dim(x)[1]),
               c(nrow(x1) / 2, nrow(x1) * 3/4, nrow(x1) * 3/4,  nrow(x1) / 2))
  
  # canopy id's returned
  can_id_exists <- function(x) {all(x[, ncol(x)] %% 1 == 0)}
  expect_equal(sapply(create_canopies(x1, n_canopies= 4, q=2), can_id_exists), rep(TRUE, 4))
  
})