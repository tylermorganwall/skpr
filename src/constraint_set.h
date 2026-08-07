#ifndef SKPR_CONSTRAINT_SET_H
#define SKPR_CONSTRAINT_SET_H

#include <Rcpp.h>
#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

#include <vector>

class ConstraintSet {
public:
  struct ForbidTable {
    std::vector<int> idx;
    std::vector<std::vector<int>> tuples;
  };

  explicit ConstraintSet(const Rcpp::List &ir);

  int q() const { return q_; }
  int levels(int var) const { return L_[var]; }
  double level_value(int var, int code) const;

  void validate_codes(const int *row_codes) const;
  bool allowed_row(const int *row_codes) const;

  // Conservative feasibility check for a partial assignment. An unassigned
  // factor may contain any valid level code. False is therefore definitive;
  // true means that at least one completion may remain feasible.
  bool can_complete(const int *row_codes,
                    const unsigned char *assigned) const;

private:
  int q_ = 0;
  double comparison_tol_ = 0.0;
  std::vector<int> factor_kind_; // 0 discrete, 1 numeric
  std::vector<int> L_;
  std::vector<int> level_ptr_;
  std::vector<double> level_value_;

  // DNF clauses.
  std::vector<int> clause_ptr_;
  std::vector<int> clause_atom_;

  // Atom dispatch: 1 comparison, 2 membership, 3 linear, 4 forbidden tuple.
  std::vector<int> atom_type_;
  std::vector<int> atom_payload_idx_;

  std::vector<int> cmp_var_;
  std::vector<int> cmp_op_;
  std::vector<int> cmp_code_;

  std::vector<int> in_var_;
  std::vector<int> in_neg_;
  std::vector<int> in_ptr_;
  std::vector<int> in_code_;

  std::vector<int> lin_op_;
  std::vector<double> lin_rhs_;
  std::vector<double> lin_const_;
  std::vector<int> lin_ptr_;
  std::vector<int> lin_idx_;
  std::vector<double> lin_coef_;

  std::vector<int> forb_ptr_;
  std::vector<int> forb_idx_;
  std::vector<int> forb_code_;

  // Forbidden tuple tables are conjoined with the DNF expression.
  std::vector<ForbidTable> forb_tables_;

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
