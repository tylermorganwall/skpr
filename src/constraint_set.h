#ifndef SKPR_CONSTRAINT_SET_H
#define SKPR_CONSTRAINT_SET_H

#include <Rcpp.h>
#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <unordered_set>
#include <vector>

class ConstraintSet {
public:
  struct ForbidTable {
    std::vector<int> idx;              // factor indices (0-based)
    std::vector<std::uint64_t> stride; // mixed radix strides
    bool packed_ok = false;
    std::unordered_set<std::uint64_t> keys;
    std::vector<std::vector<int>> tuples; // fallback (codes)

    int m() const { return static_cast<int>(idx.size()); }
  };

  struct RowCache {
    std::vector<unsigned char> atom_truth; // 0/1
    std::vector<int> clause_unsat;         // unsatisfied atom count per clause
    int satisfied_count = 0;               // number of satisfied clauses
    std::vector<double> lin_lhs;           // lhs per LIN payload

    // Forbidden tuple tables
    std::vector<unsigned char> forb_hit; // per table: 1 if forbidden
    std::vector<std::uint64_t>
        forb_key; // per table: packed key if packed_ok else 0
  };

  explicit ConstraintSet(const Rcpp::List &ir);

  int q() const { return q_; }

  RowCache make_cache(const Eigen::RowVectorXd &row_values,
                      const int *row_codes) const;

  bool allowed_row(const Eigen::RowVectorXd &row_values,
                   const int *row_codes) const;

  bool allowed_change(const Eigen::RowVectorXd &row_values,
                      const int *row_codes, const RowCache &cache, int var,
                      double new_value, int new_code) const;

  void apply_change(const Eigen::RowVectorXd &row_values, const int *row_codes,
                    RowCache &cache, int var, double old_value, int old_code,
                    double new_value, int new_code) const;

private:
  int q_ = 0;
  std::vector<int> factor_kind_; // 0 discrete, 1 numeric
  std::vector<int> L_;
  std::vector<double> value_offset_; // value in constraint units = offset + scale * search_value
  std::vector<double> value_scale_;

  // DNF: clauses
  std::vector<int> clause_ptr_;
  std::vector<int> clause_atom_;

  // atoms
  std::vector<int> atom_type_; // 1 cmp, 2 in, 3 lin, 4 forbid
  std::vector<int> atom_payload_idx_;

  // cmp payload
  std::vector<int> cmp_var_;
  std::vector<int> cmp_op_;
  std::vector<double> cmp_value_;

  // in payload
  std::vector<int> in_var_;
  std::vector<int> in_neg_;
  std::vector<int> in_ptr_;
  std::vector<double> in_values_;
  std::vector<int> in_mask_ptr_;
  std::vector<unsigned char> in_mask_;

  // lin payload
  std::vector<int> lin_op_;
  std::vector<double> lin_rhs_;
  std::vector<double> lin_const_;
  std::vector<int> lin_ptr_;
  std::vector<int> lin_idx_;
  std::vector<double> lin_coef_;

  // forbid payload (multi-var equality pattern)
  std::vector<int> forb_ptr_;
  std::vector<int> forb_idx_;
  std::vector<double> forb_value_;

  // forbidden tuple tables (outside DNF; conjoined)
  std::vector<ForbidTable> forb_tables_;

  // adjacency
  std::vector<std::vector<int>> atoms_by_factor_;
  std::vector<std::vector<int>> clauses_by_atom_;

  static bool relop(double lhs, int op, double rhs);

  double to_constraint_value(int var, double raw_value) const;

  double get_value(const Eigen::RowVectorXd &row_values, int var_changed,
                   double new_value, int var_query) const;

  unsigned char eval_atom_current(const Eigen::RowVectorXd &row_values,
                                  const int *row_codes, const RowCache &cache,
                                  int atom_id) const;

  unsigned char eval_atom_changed(const Eigen::RowVectorXd &row_values,
                                  const int *row_codes, const RowCache &cache,
                                  int atom_id, int var_changed,
                                  double new_value, int new_code) const;

  double lin_lhs_current(const Eigen::RowVectorXd &row_values,
                         int lin_payload) const;

  double lin_lhs_changed(const RowCache &cache, int lin_payload,
                         int var_changed, double old_value,
                         double new_value) const;

  double lin_coef_for_var(int lin_payload, int var) const;

  bool in_membership(int in_payload, double value, int code) const;

  // forbidden tuple tables
  void init_forbid_tables(const Rcpp::List &ir);
  unsigned char forbid_hit_current(const int *row_codes, RowCache &cache) const;
  unsigned char forbid_hit_changed(const int *row_codes, const RowCache &cache,
                                   int var, int old_code, int new_code) const;

  std::uint64_t pack_key(const ForbidTable &tab, const int *row_codes) const;
};

#endif
