// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
double AOptimality(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(100 / (currentDesign.rows() * XtX.partialPivLu().inverse().trace() / currentDesign.cols()));
}

