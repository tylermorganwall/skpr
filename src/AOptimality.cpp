// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
double AOptimality(const arma::mat& currentDesign) {
  return(100/(currentDesign.n_rows*trace(inv_sympd(currentDesign.t()*currentDesign))/currentDesign.n_cols));
}

