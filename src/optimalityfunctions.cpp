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

double calculateAOptimality(const Eigen::MatrixXd& currentV) {
  return(currentV.trace());
}

double calculateAliasTraceSlow(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& aliasMatrix) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  Eigen::MatrixXd A = XtX.llt().solve(currentDesign.transpose())*aliasMatrix;
  return((A.transpose() * A).trace());
}

double calculateAliasTrace(const Eigen::MatrixXd& currentV,
                           const Eigen::MatrixXd& currentDesign,
                           const Eigen::MatrixXd& aliasMatrix) {
  Eigen::MatrixXd A = currentV*currentDesign.transpose()*aliasMatrix;
  return((A.transpose() * A).trace());
}

double calculateDEff(const Eigen::MatrixXd& currentDesign, double numbercols, double numberrows) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(pow(XtX.partialPivLu().determinant(), 1/numbercols) / numberrows);
}

double calculateDEffLog(const Eigen::MatrixXd& currentDesign, double numbercols, double numberrows) {
  return(pow(exp(calculateDOptimalityLog(currentDesign)), 1/numbercols) / numberrows);
}

double calculateDEffNN(const Eigen::MatrixXd& currentDesign, double numbercols) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(pow(XtX.partialPivLu().determinant(), 1.0/numbercols));
}

bool isSingular(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(!XtX.colPivHouseholderQr().isInvertible());
}

double calculateCustomOptimality(const Eigen::MatrixXd& currentDesign, Rcpp::Function customOpt) {
  return Rcpp::as<double>(customOpt(Rcpp::Named("currentDesign", currentDesign)));
}

void rankUpdate(Eigen::MatrixXd& vinv, const Eigen::VectorXd& pointold, const Eigen::VectorXd& pointnew,
                const Eigen::MatrixXd& identity,
                Eigen::MatrixXd& f1, Eigen::MatrixXd& f2,Eigen::MatrixXd& f2vinv) {
  f1.col(0) = pointnew; f1.col(1) = -pointold;
  f2.col(0) = pointnew; f2.col(1) = pointold;
  f2vinv = f2.transpose()*vinv;
  Eigen::MatrixXd tmp = vinv - vinv * f1 * (identity + f2vinv*f1).householderQr().solve(f2vinv);
  vinv = tmp;
}

Eigen::MatrixXd rankUpdateValue(Eigen::MatrixXd& vinv, const Eigen::VectorXd& pointold, const Eigen::VectorXd& pointnew,
                                const Eigen::MatrixXd& identity,
                                Eigen::MatrixXd& f1, Eigen::MatrixXd& f2,Eigen::MatrixXd& f2vinv) {
  f1.col(0) = pointnew; f1.col(1) = -pointold;
  f2.col(0) = pointnew; f2.col(1) = pointold;
  f2vinv = f2.transpose()*vinv;
  Eigen::MatrixXd tmp = vinv - vinv * f1 * (identity + f2vinv*f1).householderQr().solve(f2vinv);
  return(tmp);
}

void search_candidate_set(const Eigen::MatrixXd& V, const Eigen::MatrixXd& candidatelist_trans,
                          const Eigen::VectorXd& designrow,
                          double xVx, int& entryy, bool& found, double& del) {
  Eigen::VectorXd yV(candidatelist_trans.rows());
  double newdel = 0;
  int ncols = candidatelist_trans.cols();
  for (int j = 0; j < ncols; j++) {
    yV = V * candidatelist_trans.col(j);
    newdel = yV.dot(candidatelist_trans.col(j))*(1 - xVx) - xVx + pow(yV.dot(designrow),2);
    if(newdel > del) {
      found = true;
      entryy = j;
      del = newdel;
    }
  }
}

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

