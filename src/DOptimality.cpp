// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
double DOptimality(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose() * currentDesign;
  return(XtX.partialPivLu().determinant());
}

// [[Rcpp::export]]
double DOptimalityLog(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(exp(XtX.llt().matrixL().toDenseMatrix().diagonal().array().log().sum()));
}

