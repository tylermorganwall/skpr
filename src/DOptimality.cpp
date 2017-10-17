#define ARMA_DONT_PRINT_OPENMP_WARNING
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
double DOptimality(const arma::mat& currentDesign) {
  double val;
  double sign;
  arma::log_det(val,sign,currentDesign.t()*currentDesign);
  return(exp(val)*sign);
}

