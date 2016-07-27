// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
double DOptimality(arma::mat currentDesign) {
  double determinant = det(currentDesign.t()*currentDesign);
  return(determinant);
}

