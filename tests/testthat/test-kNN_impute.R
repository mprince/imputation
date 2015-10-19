
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

# test_that("calculations work", {
#   x1 <- matrix(rnorm(100), 10, 10)
#   x1[sample(1:100, size= 10, replace= FALSE)] <- NA
#   x2 <- data.frame(id=1:nrow(x1), as.data.frame(x1))
#   
#   knn1 <- kNN_impute.default(x2, k=3, q=2, parallel= FALSE, verbose= FALSE)
#   knn2 <- kNN_impute(x2, k=3, q=2, parallel= FALSE, verbose= FALSE)
#   
#   knn3 <- kNN_impute.default(x1, k=3, q=2, parallel= TRUE, verbose= FALSE)
#   knn4 <- kNN_impute(x1, k=3, q=2, parallel= TRUE, verbose= FALSE)
#   
#   # call to default works correctly
#   expect_equal(knn1, knn2)
#   expect_equal(knn3, knn4)
#   
#   # parallel is same
#   knn5 <- kNN_impute(x2, k=3, q=2, parallel= FALSE, verbose= FALSE, n_canopies= 3,
#                      cheap_metric= "id")
#   knn6 <- kNN_impute(x2, k=3, q=2, parallel= TRUE, verbose= FALSE, n_canopies= 3,
#                      cheap_metric= "id")
#   
#   expect_equal(knn5, knn6)
#   expect_equal(dim(knn1), dim(knn5))
#   expect_equal(dim(knn2), dim(knn5))
#   expect_equal(dim(knn3), dim(knn6))
#   expect_equal(dim(knn4), dim(knn6))
#   
#   expect_false(all.equal(knn1, knn5))
#   expect_false(all.equal(knn3, knn6))
# })


