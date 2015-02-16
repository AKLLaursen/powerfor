#include <Rcpp.h>
using namespace Rcpp;

//' Function for calculating exponentially weighted moving average. Written in
//' C++.
//' @param x An atomic vector
//' @param lamb A double
//' @export
// [[Rcpp::export]]
NumericVector ewma(NumericVector x, double lambda = 0.975) {
  int n = x.size();
  NumericVector exp = clone(x);
  for(int i = 1; i < n; ++i) {
    exp[i] = (1 - lambda) * x[i] + lambda * exp[i - 1];
  }
  return exp;
}