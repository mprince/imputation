
#include <Rcpp.h>
#include <algorithm>
#include "which_na.cpp"
#include "sort_indices.cpp"
#include <iostream>
#include <vector>

using namespace Rcpp;
using namespace std;


// this function is equivalent to calculating via
// kernelMatrix(kern, c(0, smallest_distances))[1, -1, drop= TRUE]
// in R library(kernlab)
// [[Rcpp:export]]
NumericVector kern_wt (double& sigma, NumericVector& x) {
  NumericVector ret(x.size() - 1);

  for (int i = 1; i < x.size(); i++) {
    ret[i-1] = std::exp(2 * sigma * -1 * (double) (x[i] * x[i] / 2));
  }

  return ret;
}

// @title Weighted Mean (C++)
// @description Compute a weighted mean
// @param x A \code{NumericVector} of values whose mean is to be computed.
// @param w A \code{NumericVector} of weights of the same length as \code{x}
// giving the weights to use for the elements of x.
// @return a scalar.
// @seealso \code{\link[stats]{weighted.mean}}
// [[Rcpp::export]]
double weighted_mean(NumericVector& x, NumericVector w) {
  if (x.size() != w.size()) {
    cout << "ERROR: Length of x and w differ." << endl;
    return -1;
  }

  // normalize weights and compute weighted mean
  double s= 0.0, wm= 0.0;
  for (int i = 0; i < w.size(); i++) {
    s += w[i];
  }
  for (int i = 0; i < w.size(); i++) {
    w[i] = w[i] / s;
    wm += x[i] * w[i];
  }

  return wm;
}

IntegerVector callRfunc (NumericVector& x, Function f) {
  IntegerVector res = f(x);
  return res;
}


//' @title Imputation function for kNN
//' @description Function for KNN imputation for a single element.
//' Distances are weighted by a kernal function to produce a weighted
//' imputation.
//' @param values The values from which imputation will take place
//' @param distances The distances associated with each value
//' @param k The number of neighbors used to impute
//' @param sigma The standard deviation (ie sigma parameter) of the Gaussian kernal used for weighting
// [[Rcpp::export]]
double impute_fn_knn (NumericVector& values, NumericVector& distances, int& k, double& sigma) {
  NumericVector small_dist(k), knn_values(k);
  // IntegerVector rnks = callRfunc(x, order) - 1;
  IntegerVector rnks = (IntegerVector) sort_indexes(distances);

  for (int i = 0; i < k; i++) {
    small_dist[i] = distances[ rnks[i] ];
    knn_values[i] = values[ rnks[i] ];
  }

  NumericVector d = kern_wt(sigma, small_dist);
  return weighted_mean(knn_values, d);
}
