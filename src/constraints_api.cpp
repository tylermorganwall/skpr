#include <Rcpp.h>
#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

#include "constraint_set.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::LogicalVector skpr_constraints_allowed(Eigen::MatrixXd points,
                                            Rcpp::IntegerMatrix level_pos,
                                            Rcpp::List constraints_ir) {
  const int n = points.rows();
  const int q = points.cols();
  if (level_pos.nrow() != n || level_pos.ncol() != q) {
    stop("skpr: level_pos dimension mismatch.");
  }

  ConstraintSet cs(constraints_ir);
  if (cs.q() != q) stop("skpr: constraints_ir$q must match ncol(points).");

  LogicalVector out(n);
  std::vector<int> codes(q);

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < q; ++j) codes[j] = level_pos(i, j);
    out[i] = cs.allowed_row(points.row(i), codes.data());
  }

  return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector skpr_constraints_allowed_change(
    Rcpp::NumericVector row_values, Rcpp::IntegerVector row_codes,
    Rcpp::List constraints_ir, int var1, double new_value, int new_code) {
  ConstraintSet cs(constraints_ir);
  const int q = cs.q();
  if (row_values.size() != q || row_codes.size() != q) {
    stop("skpr: row dimension mismatch.");
  }
  int var = var1 - 1;
  if (var < 0 || var >= q) stop("skpr: var out of range.");

  std::vector<int> codes(q);
  for (int j = 0; j < q; ++j) codes[j] = row_codes[j];

  Eigen::Map<Eigen::VectorXd> row_map(row_values.begin(), q);
  Eigen::RowVectorXd row_values_eig = row_map.transpose();

  ConstraintSet::RowCache cache = cs.make_cache(row_values_eig, codes.data());
  bool ok = cs.allowed_change(row_values_eig, codes.data(), cache, var, new_value, new_code);
  return LogicalVector::create(ok);
}
