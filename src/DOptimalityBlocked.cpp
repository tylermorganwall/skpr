#define ARMA_DONT_PRINT_OPENMP_WARNING
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
double DOptimalityBlocked(const arma::mat& currentDesign, const arma::mat& blockedVar) {
  double val;
  double sign;
  arma::log_det(val,sign,currentDesign.t()*arma::inv_sympd(blockedVar)*currentDesign);
  return(exp(val)*sign);
}

