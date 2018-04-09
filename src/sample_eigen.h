#include <RcppEigen.h>

using namespace Rcpp;

Eigen::VectorXd sample_eigen(const Eigen::VectorXd& sample_from, int totalPoints, bool replace);

Eigen::VectorXi sample_eigen(const Eigen::VectorXi& sample_from, int totalPoints, bool replace);
