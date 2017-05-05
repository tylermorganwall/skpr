// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
double calcAliasTrace(arma::mat currentDesign, arma::mat aliasMatrix) {
  arma::mat A = inv_sympd(currentDesign.t()*currentDesign)*currentDesign.t()*aliasMatrix;
  return(trace(A.t() * A));
}
