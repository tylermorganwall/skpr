// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
double IOptimality(const arma::mat& currentDesign, const arma::mat& momentsMatrix, const arma::mat& blockedVar) {
  return(arma::trace(arma::inv_sympd(currentDesign.t()*arma::inv_sympd(blockedVar)*currentDesign)*momentsMatrix));
}

