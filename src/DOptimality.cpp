// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
double DOptimality(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose() * currentDesign;
  return(XtX.partialPivLu().determinant());
}

