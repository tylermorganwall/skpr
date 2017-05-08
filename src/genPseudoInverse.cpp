// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat getPseudoInverse(arma::mat currentDesign) {
  return(arma::pinv(currentDesign));
}

