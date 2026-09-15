// R-facing batch adapter for the C++ constraint evaluator.
// This lets R callers check the same complete-row feasibility rules used inside
// coordinate exchange without running an optimization search.
//
// Data flow: points + zero-based level-position matrix + compiled constraint IR
//   -> construct and validate ConstraintSet -> check input shapes/finiteness
//   -> evaluate each code row -> logical vector of allowed/disallowed results.
// The point matrix supplies shape and finiteness checks; feasibility is determined
// by level_pos and the IR's level values, not by reading values from points.

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
  // Validate the batch against the factor schema before extracting row codes.
  const int n = points.rows();
  const int q = points.cols();
  if (q != constraints.q() || level_pos.nrow() != n ||
      level_pos.ncol() != q) {
    stop("skpr: constraint point/code dimensions do not match the IR.");
  }
  if (!points.allFinite()) {
    stop("skpr: constraint points must contain finite values.");
  }

  // Copy one row from R's column-major matrix into the contiguous code buffer
  // expected by ConstraintSet, reusing that buffer for the whole batch.
  LogicalVector allowed(n);
  std::vector<int> codes(q);
  for (int row = 0; row < n; ++row) {
    for (int var = 0; var < q; ++var)
      codes[var] = level_pos(row, var);
    allowed[row] = constraints.allowed_row(codes.data());
  }
  return allowed;
}
