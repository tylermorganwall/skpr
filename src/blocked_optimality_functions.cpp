
#include <RcppEigen.h>

//**********************************************************
//Everything below is for generating blocked optimal designs
//**********************************************************

double calculateBlockedDOptimality(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& gls) {
  return((currentDesign.transpose()*gls*currentDesign).partialPivLu().determinant());
}

double calculateBlockedDOptimalityLog(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& gls) {
  Eigen::MatrixXd XtX = (currentDesign.transpose()*gls*currentDesign);
  return(exp(XtX.llt().matrixL().toDenseMatrix().diagonal().array().log().sum()));
}

double calculateBlockedIOptimality(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& momentsMatrix,const Eigen::MatrixXd& gls) {
  return(((currentDesign.transpose()*gls*currentDesign).llt().solve(momentsMatrix)).trace());
}

double calculateBlockedAOptimality(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls) {
  return((currentDesign.transpose()*gls*currentDesign).partialPivLu().inverse().trace());
}

double calculateBlockedAliasTrace(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& aliasMatrix,const Eigen::MatrixXd& gls) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*gls*currentDesign;
  Eigen::MatrixXd A = XtX.llt().solve(currentDesign.transpose()*aliasMatrix);
  return((A.transpose() * A).trace());
}

double calculateBlockedGOptimality(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& gls) {
  Eigen::MatrixXd results = currentDesign*(currentDesign.transpose()*gls*currentDesign).partialPivLu().solve(currentDesign.transpose())*gls;
  return(results.diagonal().maxCoeff());
}

double calculateBlockedTOptimality(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls) {
  return((currentDesign.transpose()*gls*currentDesign).trace());
}

double calculateBlockedEOptimality(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*gls*currentDesign;
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigensolver(XtX);
  return(eigensolver.eigenvalues().minCoeff());
}

double calculateBlockedDEff(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*gls*currentDesign;
  return(pow(XtX.partialPivLu().determinant(), 1/currentDesign.cols()) / currentDesign.rows());
}

double calculateBlockedDEffNN(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*gls*currentDesign;
  return(pow(XtX.partialPivLu().determinant(), 1.0/currentDesign.cols()));
}

double calculateBlockedAliasTracePseudoInv(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& aliasMatrix,const Eigen::MatrixXd& gls) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*gls*currentDesign;
  Eigen::MatrixXd A = XtX.partialPivLu().solve(currentDesign.transpose()*aliasMatrix);
  return((A.transpose() * A).trace());
}

bool isSingularBlocked(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*gls*currentDesign;
  return(!XtX.colPivHouseholderQr().isInvertible());
}

double calculateBlockedCustomOptimality(const Eigen::MatrixXd& currentDesign, Rcpp::Function customBlockedOpt, const Eigen::MatrixXd& gls) {
  return Rcpp::as<double>(customBlockedOpt(Rcpp::Named("currentDesign", currentDesign),Rcpp::Named("vInv", gls)));
}
