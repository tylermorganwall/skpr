#define ARMA_DONT_PRINT_OPENMP_WARNING
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
double calculateAOptimalityPseudo(const arma::mat& currentDesign) {
  return(trace(arma::pinv(currentDesign.t()*currentDesign)));
}
