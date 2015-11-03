
#include <Rcpp.h>
#include <math.h>
#include <iostream>
using namespace Rcpp;

//' @title Calculate \eqn{L_q} distance of two vectors
//' @description Calculate \eqn{L_q} distance of two vectors
//' @param x A numeric vector. Missing values are allowed.
//' @param y A numeric vector. Missing values are allowed.
//' @param q An integer specifying the which norm to take the L-q distance of.
//' @return a scalar
// [[Rcpp::export]]
double dist_q (NumericVector x, NumericVector y, int& q) {
  int nx= x.size(), ny = y.size();

  if (nx != ny) {
    std::cout << "ERROR: Length of x and y differ." << std::endl;
    return -1;
  }

  double temp = 0.0;
  int m = 0;
  for (int i = 0; i < nx; i++) {
    if (!NumericVector::is_na(x[i]) && !NumericVector::is_na(y[i])) {
      ++m;
      temp += pow(fabs(x[i] - y[i]), (double) q);
    }
  }
  temp = (1 / (double) m * temp);
  return pow(temp, (1/ (double) q));
}


//' @title Calculate \eqn{L_q} distance
//' @description Calculate \eqn{L_q} distance of all vectors in a matrix to a reference
//' vector.
//' @param x A numeric matrix Missing values are allowed.
//' @param ref An integer specifying the reference row.
//' @param q An integer specifying the which norm to take the L-q distance of.
//' @return a numeric vector of length \code{nrow(x) - 1}
// [[Rcpp::export]]
NumericVector dist_q_matrix (NumericVector& x_ref, NumericMatrix& x_rest, int& q) {
  int nr = x_rest.nrow();
  NumericVector out(nr);
  for (int k = 0; k < nr; k++) {
    out[k] = dist_q(x_ref, x_rest.row(k), q);
  }
  return out;
}


NumericMatrix row_erase (NumericMatrix& x, IntegerVector& rowID) {
  rowID = rowID.sort();

  NumericMatrix x2(Dimension(x.nrow()- rowID.size(), x.ncol()));
  int iter = 0;
  int del = 1; // to count deleted elements
  for (int i = 0; i < x.nrow(); i++) {
    if (i != rowID[del - 1]) {
      x2.row(iter) = x.row(i);
      iter++;
    } else {
      del++;
    }
  }
  return x2;
}

NumericMatrix col_erase (NumericMatrix& x, IntegerVector& colID) {
  colID = colID.sort();

  NumericMatrix x2(Dimension(x.nrow(), x.ncol()- colID.size()));
  int iter = 0;
  int del = 1;
  for (int i = 0; i < x.ncol(); i++) {
    if (i != colID[del - 1]) {
      x2.column(iter) = x.column(i);
      iter++;
    } else {
      del++;
    }
  }
  return x2;
}

