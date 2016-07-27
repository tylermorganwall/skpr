// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
double IOptimality(arma::mat currentDesign, const arma::mat momentsMatrix) {
  double variance = arma::trace(arma::inv_sympd(currentDesign.t()*currentDesign)*momentsMatrix);
  return(variance);
}

