// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
double DOptimality(arma::mat currentDesign) {
  return(arma::det(currentDesign.t()*currentDesign));
}

