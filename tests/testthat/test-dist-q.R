

library(testthat)
library(imputation)

context("dist-q calculations")

test_that("errors work", {
  x1 <- rnorm(100)
  x2 <- matrix(letters[1:25],5,5)
  x3 <- matrix(x1, 10, 10)
  
  expect_error(dist_q.matrix(x1))
  expect_error(dist_q.matrix(x2))
  expect_error(dist_q.matrix(x3, q= 0))
  expect_error(dist_q.matrix(x3, q= -1))
  expect_error(dist_q.matrix(x3, q= 1.5))
  expect_error(dist_q.matrix(x3, ref= -1 ,q= 2))
  expect_error(dist_q.matrix(x3, ref= nrow(x3) + 1,q= 0))
  expect_error(dist_q.matrix(x3, ref= 3.5,q= 2))
  expect_error(dist_q(x1, x2,q= 2))
  expect_error(dist_q(x2, x1,q= 2))
})

test_that("dist-q calculates accurately, no missing", {
  x1 <- rnorm(10)
  y1 <- rnorm(10)
  
  d_test <- function(x,y, q) {
    return(mean(abs(x - y)^q)^(1/q))
  }
  
  expect_equal(dist_q(x1, y1, q=1), d_test(x1, y1, q=1))
  expect_equal(dist_q(x1, y1, q=2), d_test(x1, y1, q=2))
  expect_equal(dist_q(x1, y1, q=3), d_test(x1, y1, q=3))
  expect_equal(dist_q(x1, y1, q=4), d_test(x1, y1, q=4))
})



test_that("dist-q calculates accurately, with missing", {
  x1 <- sample(c(rnorm(10),NA), size= 10, replace=TRUE, prob= c(rep(.07, 10), .3))
  y1 <- sample(c(rnorm(10),NA), size= 10, replace=TRUE, prob= c(rep(.07, 10), .3))
  
  d_test <- function(x,y, q) {
    return((mean(abs(x - y)^q, na.rm=TRUE))^(1/q))
  }
  
  expect_equal(dist_q(x1, y1, q=1), d_test(x1, y1, q=1))
  expect_equal(dist_q(x1, y1, q=2), d_test(x1, y1, q=2))
  expect_equal(dist_q(x1, y1, q=3), d_test(x1, y1, q=3))
  expect_equal(dist_q(x1, y1, q=4), d_test(x1, y1, q=4))
})

