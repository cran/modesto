#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector p_hat(NumericVector rs, double r) {
  int n = rs.size();
  NumericVector output = rs;
  for(int i = 0; i < n; i++) {
    output[i] = 1 - (rs[i]/r);
  }
  return output;
}
