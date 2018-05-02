// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
Eigen::MatrixXd covarianceMatrix(const Eigen::MatrixXd& design) {
  Eigen::MatrixXd XtX = design.transpose() * design;
  return(XtX.inverse());
}
