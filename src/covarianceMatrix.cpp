// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat covarianceMatrix(arma::mat design) {
  return(inv_sympd(design.t()*design));
}
