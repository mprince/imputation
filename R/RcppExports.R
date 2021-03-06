# This file was generated by Rcpp::compileAttributes
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

weighted_mean <- function(x, w) {
    .Call('imputation_weighted_mean', PACKAGE = 'imputation', x, w)
}

#' @title Imputation function for kNN
#' @description Function for KNN imputation for a single element.
#' Distances are weighted by a kernal function to produce a weighted
#' imputation.
#' @param values The values from which imputation will take place
#' @param distances The distances associated with each value
#' @param k The number of neighbors used to impute
#' @param sigma The standard deviation (ie sigma parameter) of the Gaussian kernal used for weighting
impute_fn_knn <- function(values, distances, k, sigma) {
    .Call('imputation_impute_fn_knn', PACKAGE = 'imputation', values, distances, k, sigma)
}

sort_indexes <- function(values) {
    .Call('imputation_sort_indexes', PACKAGE = 'imputation', values)
}

#' @title Calculate \eqn{L_q} distance of two vectors
#' @description Calculate \eqn{L_q} distance of two vectors
#' @param x A numeric vector. Missing values are allowed.
#' @param y A numeric vector. Missing values are allowed.
#' @param q An integer specifying the which norm to take the L-q distance of.
#' @return a scalar
dist_q <- function(x, y, q) {
    .Call('imputation_dist_q', PACKAGE = 'imputation', x, y, q)
}

#' @title Calculate \eqn{L_q} distance
#' @description Calculate \eqn{L_q} distance of all vectors in a matrix to a reference
#' vector.
#' @param x A numeric matrix Missing values are allowed.
#' @param ref An integer specifying the reference row.
#' @param q An integer specifying the which norm to take the L-q distance of.
#' @return a numeric vector of length \code{nrow(x) - 1}
dist_q_matrix <- function(x_ref, x_rest, q) {
    .Call('imputation_dist_q_matrix', PACKAGE = 'imputation', x_ref, x_rest, q)
}

which_na <- function(x) {
    .Call('imputation_which_na', PACKAGE = 'imputation', x)
}

