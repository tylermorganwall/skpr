#define ARMA_DONT_PRINT_OPENMP_WARNING
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat getPseudoInverse(const arma::mat& currentDesign) {
  return(arma::pinv(currentDesign));
}

