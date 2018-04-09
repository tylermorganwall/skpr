#include <RcppArmadilloExtensions/sample.h>
#include <RcppEigen.h>

using namespace Rcpp;

Eigen::VectorXd sample_eigen(const Eigen::VectorXd& sample_from, int totalPoints, bool replace) {
  //convert Eigen matrix to std::vector so we can use RcppArmadillo's sample function
  std::vector<double> tmp(sample_from.size());
  Eigen::VectorXd::Map(tmp.data(), tmp.size()) = sample_from;
  std::vector<double> sampled_values = RcppArmadillo::sample(tmp, totalPoints, replace);
  Eigen::VectorXd retval = Eigen::VectorXd::Map(sampled_values.data(), sampled_values.size());
  return(retval);
}

Eigen::VectorXi sample_eigen(const Eigen::VectorXi& sample_from, int totalPoints, bool replace) {
  //convert Eigen matrix to std::vector so we can use RcppArmadillo's sample function
  std::vector<int> tmp(sample_from.size());
  Eigen::VectorXi::Map(tmp.data(), tmp.size()) = sample_from;
  std::vector<int> sampled_values = RcppArmadillo::sample(tmp, totalPoints, replace);
  Eigen::VectorXi retval = Eigen::VectorXi::Map(sampled_values.data(), sampled_values.size());
  return(retval);
}
