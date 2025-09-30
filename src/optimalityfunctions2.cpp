#include <RcppEigen.h>

double calculateDOptimality(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(XtX.partialPivLu().determinant());  //works without partialPivLu()
}

double calculateDOptimalityLog(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(XtX.llt().matrixL().toDenseMatrix().diagonal().array().log().sum());
}

double calculateIOptimality(const Eigen::MatrixXd& currentV, const Eigen::MatrixXd& momentsMatrix) {
  return((currentV * momentsMatrix).trace());
}


double calculateGOptimality(const Eigen::MatrixXd& currentV, const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd results = currentDesign*currentV*currentDesign.transpose();
  return(results.diagonal().maxCoeff());
}

double calculateTOptimality(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(XtX.trace());
}

double calculateEOptimality(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigensolver(XtX);
  return(eigensolver.eigenvalues().minCoeff());
}

double calculateDEffLog(const Eigen::MatrixXd& currentDesign, double numbercols, double numberrows) {
  return(exp(calculateDOptimalityLog(currentDesign)/numbercols) / numberrows);
}
