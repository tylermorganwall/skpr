// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
double DOptimalityBlocked(arma::mat currentDesign, arma::mat blockedVar) {
  double val;
  double sign;
  arma::log_det(val,sign,currentDesign.t()*arma::inv_sympd(blockedVar)*currentDesign);
  return(exp(val)*sign);
}

