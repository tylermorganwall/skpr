// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat covarianceMatrixPseudo(const arma::mat& design) {
  return(arma::pinv(design.t()*design));
}
