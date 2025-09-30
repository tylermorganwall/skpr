#include <RcppEigen.h>

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
