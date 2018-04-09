// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
double calculateDEfficiency(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose() * currentDesign;
  return(pow(XtX.partialPivLu().determinant(), 1/currentDesign.cols()) / currentDesign.rows());  //works without partialPivLu()
}
