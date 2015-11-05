
#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
std::vector<size_t> sort_indexes(std::vector<double>& values) {

  // initialize original index locations
  std::vector<size_t> idx(values.size());
  for (size_t i = 0; i != idx.size(); ++i) {
    idx[i] = i;
  }

  // sort indexes based on comparing values in v
  std::sort(idx.begin(), idx.end(),
       [values](size_t i1, size_t i2) {return values[i1] < values[i2];});

  return idx;
}
