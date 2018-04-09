// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
double calcAliasTrace(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& aliasMatrix) {
  Eigen::MatrixXd XtX = currentDesign.transpose() * currentDesign;
  Eigen::MatrixXd A = XtX.llt().solve(currentDesign.transpose() * aliasMatrix);
  return((A.transpose() * A).trace());
}
