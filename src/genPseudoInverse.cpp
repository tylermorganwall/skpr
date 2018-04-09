// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
Eigen::MatrixXd getPseudoInverse(const Eigen::MatrixXd& currentDesign) {
  return(currentDesign.completeOrthogonalDecomposition().pseudoInverse());
}

