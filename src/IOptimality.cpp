// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
double IOptimality(arma::mat currentDesign, const arma::mat momentsMatrix, const NumericVector blocking = 0, const double varRatio = 0) {
  if(blocking.size() != 1) {
    int blockMatrixSize = sum(blocking);
    arma::mat V = arma::mat(blockMatrixSize, blockMatrixSize, arma::fill::zeros);
    V.diag() += 1;
    int placeholder = blocking[0];
    V(arma::span(0,blocking[0]-1),arma::span(0,blocking[0]-1)) += varRatio;
    for(int i = 1; i < blocking.size(); i++) {
      V(arma::span(placeholder,placeholder+blocking[i]-1),arma::span(placeholder,placeholder+blocking[i]-1)) += varRatio;
      placeholder += blocking[i];
    }
    const arma::mat vInv = inv_sympd(V);
    return(arma::trace(arma::inv_sympd(currentDesign.t()*vInv*currentDesign)*momentsMatrix));
  } else {
    return(arma::trace(arma::inv_sympd(currentDesign.t()*currentDesign)*momentsMatrix));
  }
}

