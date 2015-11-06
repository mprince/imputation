
#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;

//' @title Sort vector and return indexes
//' @description Sort the vector by the values 
//' Return the indexes of the sorted vector according to original 
//' @param values The vector that should be sorted
// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
IntegerVector sort_indexes(NumericVector& values) {

  // initialize original index locations
  IntegerVector idx(values.size());
  for (size_t i = 0; i != idx.size(); ++i) {
    idx[i] = i;
  }

  // sort indexes based on comparing values in v
  std::sort(idx.begin(), idx.end(),
       [&values](size_t i1, size_t i2) {return values[i1] < values[i2];});

  return idx;
}
