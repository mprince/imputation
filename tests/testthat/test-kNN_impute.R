
library(testthat)
library(imputation)

context("kNN_impute")

test_that("errors work correctly", {
  x1 <- matrix(rnorm(100), 10, 10)
  x2 <- matrix(letters[1:25], 5,5)
  x3 <- rnorm(100)
  
  expect_error(kNN_impute(x1, k= 0))
  expect_error(kNN_impute(x1, k= nrow(x1) + 1))
  expect_error(kNN_impute(x1, k= 5.5))
  expect_error(kNN_impute(x1, k= 5, q= -1))
  expect_error(kNN_impute(x1, k= 5, q= 0))
  expect_error(kNN_impute(x1, k= 5, q= 2.5))
  expect_error(kNN_impute(x1, k= 5, q= -1))
  expect_error(kNN_impute(x2, k= 2))
  expect_error(kNN_impute(x3, k= 2))
  expect_error(kNN_impute(x1, k= 5, q= 1, parallel= TRUE, leave_cores= -1))
  expect_error(kNN_impute(x1, k= 5, q= 1, parallel= TRUE, leave_cores= 2.5))
  ## trivial case equality
  k1 <- kNN_impute(x1, k= 3)
  k2 <- kNN_impute(x1, k= 3, parallel= FALSE)
  dimnames(k1) <- dimnames(k2) <-NULL
  expect_equal(k1, x1)
  expect_equal(k2, x1)
  # column NA
  x1[,1] <- NA
  expect_error(kNN_impute(x1, parallel= FALSE))
})

test_that("calculations same to wrapper", {
  x1 <- matrix(rnorm(300), 30, 10)
  x1[x1 > 1.25] <- NA
  x3 <- create_canopies(x1, n_canopies= 5, q= 2)
  
  # testing function
  can5 <- function(l, k, q, ...) {
    l2 <- lapply(l, kNN_impute.canopies, k= k, q=q, verbose= FALSE, n_canopies= 5, ... = ...)
    l2 <- do.call("rbind", lapply(l2, "[[", 1))
    l2 <- l2[, -ncol(l2)]
    return(l2[order(as.integer(rownames(l2))),])
  }
  
  #-------------------------
  ### kNN no_canopies
  knn1 <- kNN_impute.no_canopies(x1, k=3, q=2, parallel= FALSE, verbose= FALSE)
  knn2 <- kNN_impute(x1, k=3, q=2, parallel= FALSE, verbose= FALSE)
  
  knn3 <- kNN_impute.no_canopies(x1, k=3, q=2, parallel= TRUE, verbose= FALSE)
  knn4 <- kNN_impute(x1, k=3, q=2, parallel= TRUE, verbose= FALSE)
  
  # call to no_canopies works correctly
  expect_equal(knn1, knn2)
  expect_equal(knn3, knn4)
  
  ### kNN canopies
  knn1 <- can5(x3, k=3, q=2, parallel= FALSE)
  knn2 <- kNN_impute(x1, k=3, q=2, parallel= FALSE, verbose= FALSE, n_canopies= 5)
  
  knn3 <- can5(x3, k=3, q=2, parallel= TRUE)
  knn4 <- kNN_impute(x1, k=3, q=2, parallel= TRUE, verbose= FALSE, n_canopies= 5)
  
  # call to canopies works correctly
  expect_equal(knn1, knn2$x)
  expect_equal(knn3, knn4$x)
  expect_equal(knn2, knn4)
   
})

test_that("equivalence with canopies / no canopies", {
  x1 <- matrix(rnorm(300), 30, 10)
  x1[x1 > 1.25] <- NA
  x3 <- create_canopies(x1, n_canopies= 5, q= 2)
  
  # testing function
  can5 <- function(l, k, q, ...) {
    l2 <- lapply(l, kNN_impute.canopies, k= k, q=q, verbose= FALSE, n_canopies= 5, ... = ...)
    l2 <- do.call("rbind", lapply(l2, "[[", 1))
    l2 <- l2[, -ncol(l2)]
    return(l2[order(as.integer(rownames(l2))),])
  }
  
  #-------------------------
  
  # 
  knn1 <- kNN_impute(x1, k=3, q=2, parallel= FALSE, verbose= FALSE)
  knn2 <- kNN_impute(x1, k=3, q=2, parallel= TRUE, verbose= FALSE)
  
  knn5 <- kNN_impute(x1, k=3, q=2, parallel= FALSE, verbose= FALSE, n_canopies= 5)
  knn6 <- kNN_impute(x1, k=3, q=2, parallel= TRUE, verbose= FALSE, n_canopies= 5)
  
  # expect same dimensions between canopies and no canopies
  expect_equal(dim(knn1$x), dim(knn5$x))
  expect_equal(dim(knn2$x), dim(knn5$x))
  expect_equal(dim(knn1$x), dim(knn6$x))
  expect_equal(dim(knn2$x), dim(knn6$x))
  
  # expect same rownames between canopies and no canopies
  expect_equal(rownames(knn1$x), rownames(knn5$x))
  expect_equal(rownames(knn2$x), rownames(knn5$x))
  expect_equal(rownames(knn1$x), rownames(knn6$x))
  expect_equal(rownames(knn2$x), rownames(knn6$x))
  
  # expect differences between canopies and no canopies -- values
  t1 <- table(!is.na(x1))
  expect_equal(t1, table(knn1$x == knn5$x))
  expect_equal(t1, table(knn2$x == knn5$x))
  expect_equal(t1, table(knn1$x == knn6$x))
  expect_equal(t1, table(knn2$x == knn6$x))
  
})


