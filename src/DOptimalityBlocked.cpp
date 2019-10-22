// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
double DOptimalityBlocked(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& blockedVar) {
  Eigen::MatrixXd XtX = currentDesign.transpose() * blockedVar.householderQr().solve(currentDesign);
  return(XtX.partialPivLu().determinant());
}

// [[Rcpp::export]]
double DOptimalityBlockedLog(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& blockedVar) {
  Eigen::MatrixXd XtX = currentDesign.transpose() * blockedVar.householderQr().solve(currentDesign);
  return(exp(XtX.llt().matrixL().toDenseMatrix().diagonal().array().log().sum()));
}
