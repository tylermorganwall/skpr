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
  return(exp(XtX.completeOrthogonalDecomposition().logAbsDeterminant()/currentDesign.cols())/currentDesign.rows());
}

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


// [[Rcpp::export]]
double calculateDEfficiency(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose() * currentDesign;
  return(pow(XtX.partialPivLu().determinant(), 1/currentDesign.cols()) / currentDesign.rows());  //works without partialPivLu()
}

// [[Rcpp::export]]
double AOptimality(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(100 * currentDesign.cols() / ((currentDesign.rows() * (XtX.partialPivLu().inverse())).trace()));
}



