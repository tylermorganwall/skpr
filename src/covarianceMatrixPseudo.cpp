// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat covarianceMatrixPseudo(arma::mat design) {
  return(arma::pinv(design.t()*design));
}
