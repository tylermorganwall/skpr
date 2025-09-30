// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
double calculateAOptimalityPseudo(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(XtX.completeOrthogonalDecomposition().pseudoInverse().trace());
}

// [[Rcpp::export]]
double IOptimality(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& momentsMatrix, const Eigen::MatrixXd& blockedVar) {
  Eigen::MatrixXd XtX = (currentDesign.transpose() * (blockedVar.householderQr().solve(currentDesign))).householderQr().solve(momentsMatrix);
  return(XtX.trace());
}


// [[Rcpp::export]]
double calcAliasTrace(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& aliasMatrix) {
  Eigen::MatrixXd XtX = currentDesign.transpose() * currentDesign;
  Eigen::MatrixXd A = XtX.llt().solve(currentDesign.transpose() * aliasMatrix);
  return((A.transpose() * A).trace());
}

// [[Rcpp::export]]
Eigen::MatrixXd covarianceMatrixPseudo(const Eigen::MatrixXd& design) {
  Eigen::MatrixXd XtX = design.transpose() * design;
  return(XtX.completeOrthogonalDecomposition().pseudoInverse());
}

// [[Rcpp::export]]
Eigen::MatrixXd getPseudoInverse(const Eigen::MatrixXd& currentDesign) {
  return(currentDesign.completeOrthogonalDecomposition().pseudoInverse());
}

// [[Rcpp::export]]
double GEfficiency(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& candset) {
  Eigen::MatrixXd V = (currentDesign.transpose()*currentDesign).partialPivLu().inverse();
  Eigen::MatrixXd results = candset*V*candset.transpose();
  return(100*((double)currentDesign.cols()/(double)currentDesign.rows())*1/results.diagonal().maxCoeff());
}
