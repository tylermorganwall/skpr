// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
Eigen::MatrixXd covarianceMatrixPseudo(const Eigen::MatrixXd& design) {
  Eigen::MatrixXd XtX = design.transpose() * design;
  return(XtX.completeOrthogonalDecomposition().pseudoInverse());
}
