// Runtime representation of compiled coordinate-exchange constraints.
// R/constraints_compile.R emits a versioned list of arrays; the constructor in
// constraint_set.cpp validates and copies them into this object's owned storage.
// The CE generator then supplies zero-based level codes and, for partial rows,
// an assigned mask. No R expression evaluation is needed during these checks.
//
// Data flow: R constraint IR -> validated tables -> allowed_row() for complete
// points / can_complete() for pruning partial assignments -> CE candidate rows.
// constraints_api.cpp also exposes complete-row evaluation to R callers.
// Numeric level values come from the IR, so constraints can use original units
// while the CE model matrix uses a normalized representation of the same codes.

#ifndef SKPR_CONSTRAINT_SET_H
#define SKPR_CONSTRAINT_SET_H

#include <Rcpp.h>
#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

#include <vector>

class ConstraintSet {
public:
  // Each tuple specifies codes only for the factors in idx. A hit forbids all
  // full rows with that tuple, independent of the remaining factor values.
  struct ForbidTable {
    std::vector<int> idx;
    std::vector<std::vector<int>> tuples;
  };

  // Construction performs schema and bounds validation once per constraint set.
  explicit ConstraintSet(const Rcpp::List &ir);

  int q() const { return q_; }
  int levels(int var) const { return L_[var]; }
  double level_value(int var, int code) const;

  // Complete rows must satisfy an OR of AND clauses and avoid every forbidden
  // table. All entries in row_codes must be valid level positions.
  void validate_codes(const int *row_codes) const;
  bool allowed_row(const int *row_codes) const;

  // Conservative feasibility check for a partial assignment. An unassigned
  // factor may contain any valid level code. False is therefore definitive;
  // true means that at least one completion may remain feasible.
  bool can_complete(const int *row_codes,
                    const unsigned char *assigned) const;

private:
  // Factor schema and flattened value tables. A factor's code maps to
  // level_value_[level_ptr_[factor] + code]; all stored offsets are zero-based.
  int q_ = 0;
  double comparison_tol_ = 0.0;
  std::vector<int> factor_kind_; // 0 discrete, 1 numeric
  std::vector<int> L_;
  std::vector<int> level_ptr_;
  std::vector<double> level_value_;

  // DNF (OR of AND) clauses reference atom IDs through flattened offset slices.
  std::vector<int> clause_ptr_;
  std::vector<int> clause_atom_;

  // Atom dispatch: 1 comparison, 2 membership, 3 linear, 4 forbidden tuple.
  std::vector<int> atom_type_;
  std::vector<int> atom_payload_idx_;

  // Single-factor comparisons against an exact level code.
  std::vector<int> cmp_var_;
  std::vector<int> cmp_op_;
  std::vector<int> cmp_code_;

  // Membership predicates; the compiler emits sorted code lists for lookup.
  std::vector<int> in_var_;
  std::vector<int> in_neg_;
  std::vector<int> in_ptr_;
  std::vector<int> in_code_;

  // Linear predicates: constant + sum(coefficient * numeric level value) is
  // compared with rhs. Partial evaluation bounds contributions of open factors.
  std::vector<int> lin_op_;
  std::vector<double> lin_rhs_;
  std::vector<double> lin_const_;
  std::vector<int> lin_ptr_;
  std::vector<int> lin_idx_;
  std::vector<double> lin_coef_;

  // Forbidden-tuple atoms embedded inside DNF clauses, unlike the separate
  // tables below, which must be avoided regardless of the satisfied clause.
  std::vector<int> forb_ptr_;
  std::vector<int> forb_idx_;
  std::vector<int> forb_code_;

  // Forbidden tuple tables are conjoined with the DNF expression.
  std::vector<ForbidTable> forb_tables_;

  // Predicate evaluation shared by complete-row checks and conservative bounds
  // on partial rows. Operator codes match the R compiler's op_code table.
  bool relop(double lhs, int op, double rhs) const;
  static bool code_relop(int lhs, int op, int rhs);
  bool atom_true(int atom_id, const int *row_codes) const;
  bool atom_possible(int atom_id, const int *row_codes,
                     const unsigned char *assigned) const;
  bool membership(int payload, int code) const;
  bool forbidden_table_hit(const ForbidTable &table,
                           const int *row_codes) const;
  bool forbidden_table_covers_completions(
      const ForbidTable &table, const int *row_codes,
      const unsigned char *assigned) const;
};

#endif
