#include <Rcpp.h>
#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

#include "constraint_set.h"

using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector skpr_constraints_allowed(const Eigen::MatrixXd &points,
                                       const IntegerMatrix &level_pos,
                                       const List &constraints_ir) {
  ConstraintSet constraints(constraints_ir);
  const int n = points.rows();
  const int q = points.cols();
  if (q != constraints.q() || level_pos.nrow() != n ||
      level_pos.ncol() != q) {
    stop("skpr: constraint point/code dimensions do not match the IR.");
  }
  if (!points.allFinite()) {
    stop("skpr: constraint points must contain finite values.");
  }

  LogicalVector allowed(n);
  std::vector<int> codes(q);
  for (int row = 0; row < n; ++row) {
    for (int var = 0; var < q; ++var)
      codes[var] = level_pos(row, var);
    allowed[row] = constraints.allowed_row(codes.data());
  }
  return allowed;
}
