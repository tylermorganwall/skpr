#define ARMA_DONT_PRINT_OPENMP_WARNING
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat covarianceMatrix(const arma::mat& design) {
  return(inv_sympd(design.t()*design));
}
