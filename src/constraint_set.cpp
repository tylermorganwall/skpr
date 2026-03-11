#include "constraint_set.h"
#include <limits>

using namespace Rcpp;

static std::vector<int> as_int_vec(const IntegerVector &v) {
  std::vector<int> out(v.size());
  for (int i = 0; i < v.size(); ++i)
    out[i] = v[i];
  return out;
}

static std::vector<double> as_dbl_vec(const NumericVector &v) {
  std::vector<double> out(v.size());
  for (int i = 0; i < v.size(); ++i)
    out[i] = v[i];
  return out;
}

static bool would_overflow_mul_u64(std::uint64_t a, std::uint64_t b) {
  if (a == 0 || b == 0)
    return false;
  return a > (std::numeric_limits<std::uint64_t>::max() / b);
}

ConstraintSet::ConstraintSet(const Rcpp::List &ir) {
  if (!ir.containsElementNamed("q"))
    stop("skpr: constraints_ir missing 'q'.");
  q_ = as<int>(ir["q"]);
  factor_kind_ = as_int_vec(as<IntegerVector>(ir["factor_kind"]));
  L_ = as_int_vec(as<IntegerVector>(ir["L"]));

  clause_ptr_ = as_int_vec(as<IntegerVector>(ir["clause_ptr"]));
  clause_atom_ = as_int_vec(as<IntegerVector>(ir["clause_atom"]));

  atom_type_ = as_int_vec(as<IntegerVector>(ir["atom_type"]));
  atom_payload_idx_ = as_int_vec(as<IntegerVector>(ir["atom_payload_idx"]));

  cmp_var_ = as_int_vec(as<IntegerVector>(ir["cmp_var"]));
  cmp_op_ = as_int_vec(as<IntegerVector>(ir["cmp_op"]));
  cmp_value_ = as_dbl_vec(as<NumericVector>(ir["cmp_value"]));

  in_var_ = as_int_vec(as<IntegerVector>(ir["in_var"]));
  in_neg_ = as_int_vec(as<IntegerVector>(ir["in_neg"]));
  in_ptr_ = as_int_vec(as<IntegerVector>(ir["in_ptr"]));
  in_values_ = as_dbl_vec(as<NumericVector>(ir["in_values"]));

  lin_op_ = as_int_vec(as<IntegerVector>(ir["lin_op"]));
  lin_rhs_ = as_dbl_vec(as<NumericVector>(ir["lin_rhs"]));
  lin_const_ = as_dbl_vec(as<NumericVector>(ir["lin_const"]));
  lin_ptr_ = as_int_vec(as<IntegerVector>(ir["lin_ptr"]));
  lin_idx_ = as_int_vec(as<IntegerVector>(ir["lin_idx"]));
  lin_coef_ = as_dbl_vec(as<NumericVector>(ir["lin_coef"]));

  forb_ptr_ = as_int_vec(as<IntegerVector>(ir["forb_ptr"]));
  forb_idx_ = as_int_vec(as<IntegerVector>(ir["forb_idx"]));
  forb_value_ = as_dbl_vec(as<NumericVector>(ir["forb_value"]));

  value_offset_.assign(q_, 0.0);
  value_scale_.assign(q_, 1.0);
  if (ir.containsElementNamed("value_offset")) {
    value_offset_ = as_dbl_vec(as<NumericVector>(ir["value_offset"]));
    if (static_cast<int>(value_offset_.size()) != q_) {
      stop("skpr: constraints_ir value_offset length must equal q.");
    }
  }
  if (ir.containsElementNamed("value_scale")) {
    value_scale_ = as_dbl_vec(as<NumericVector>(ir["value_scale"]));
    if (static_cast<int>(value_scale_.size()) != q_) {
      stop("skpr: constraints_ir value_scale length must equal q.");
    }
  }

  if (static_cast<int>(factor_kind_.size()) != q_ ||
      static_cast<int>(L_.size()) != q_) {
    stop("skpr: constraints_ir factor_kind/L lengths must equal q.");
  }

  const int n_atoms = static_cast<int>(atom_type_.size());
  const int n_clauses = static_cast<int>(clause_ptr_.size()) - 1;
  if (n_clauses < 0)
    stop("skpr: constraints_ir invalid clause_ptr.");

  atoms_by_factor_.assign(q_, std::vector<int>());
  clauses_by_atom_.assign(n_atoms, std::vector<int>());

  for (int c = 0; c < n_clauses; ++c) {
    const int a0 = clause_ptr_[c];
    const int a1 = clause_ptr_[c + 1];
    for (int k = a0; k < a1; ++k) {
      const int atom_id = clause_atom_[k];
      if (atom_id < 0 || atom_id >= n_atoms)
        stop("skpr: constraints_ir clause_atom out of range.");
      clauses_by_atom_[atom_id].push_back(c);
    }
  }

  for (int a = 0; a < n_atoms; ++a) {
    const int t = atom_type_[a];
    const int u = atom_payload_idx_[a];

    if (t == 1) {
      const int v = cmp_var_[u];
      if (v < 0 || v >= q_)
        stop("skpr: constraints_ir cmp var out of range.");
      atoms_by_factor_[v].push_back(a);
    } else if (t == 2) {
      const int v = in_var_[u];
      if (v < 0 || v >= q_)
        stop("skpr: constraints_ir in var out of range.");
      atoms_by_factor_[v].push_back(a);
    } else if (t == 3) {
      const int start = lin_ptr_[u];
      const int end = lin_ptr_[u + 1];
      for (int k = start; k < end; ++k) {
        const int v = lin_idx_[k];
        if (v < 0 || v >= q_)
          stop("skpr: constraints_ir lin idx out of range.");
        atoms_by_factor_[v].push_back(a);
      }
    } else if (t == 4) {
      const int start = forb_ptr_[u];
      const int end = forb_ptr_[u + 1];
      for (int k = start; k < end; ++k) {
        const int v = forb_idx_[k];
        if (v < 0 || v >= q_)
          stop("skpr: constraints_ir forb idx out of range.");
        atoms_by_factor_[v].push_back(a);
      }
    } else {
      stop("skpr: constraints_ir unknown atom_type.");
    }
  }

  const int n_in = static_cast<int>(in_var_.size());
  in_mask_ptr_.assign(n_in + 1, 0);
  in_mask_.clear();

  int offset = 0;
  for (int u = 0; u < n_in; ++u) {
    const int v = in_var_[u];
    const int Lv = (v >= 0 && v < q_) ? L_[v] : 0;

    if (v >= 0 && v < q_ && factor_kind_[v] == 0 && Lv > 0) {
      in_mask_ptr_[u] = offset;
      in_mask_.resize(offset + Lv);
      std::fill(in_mask_.begin() + offset, in_mask_.begin() + offset + Lv, 0);
      const int start = in_ptr_[u];
      const int end = in_ptr_[u + 1];
      for (int k = start; k < end; ++k) {
        const int code = static_cast<int>(std::llround(in_values_[k]));
        if (code >= 0 && code < Lv)
          in_mask_[offset + code] = 1;
      }
      offset += Lv;
    } else {
      in_mask_ptr_[u] = -1;
    }
  }
  in_mask_ptr_[n_in] = offset;

  for (int u = 0; u < n_in; ++u) {
    const int v = in_var_[u];
    if (v >= 0 && v < q_ && factor_kind_[v] == 1) {
      const int start = in_ptr_[u];
      const int end = in_ptr_[u + 1];
      if (end - start > 1) {
        std::sort(in_values_.begin() + start, in_values_.begin() + end);
      }
    }
  }

  init_forbid_tables(ir);
}

void ConstraintSet::init_forbid_tables(const Rcpp::List &ir) {
  forb_tables_.clear();
  if (!ir.containsElementNamed("forbidden_tables"))
    return;

  Rcpp::List tabs = ir["forbidden_tables"];
  if (tabs.size() == 0)
    return;

  forb_tables_.reserve(tabs.size());
  for (int t = 0; t < tabs.size(); ++t) {
    Rcpp::List tabR = tabs[t];
    IntegerVector idxR = tabR["idx"];     // 0-based indices
    IntegerMatrix codesR = tabR["codes"]; // n_forb x m codes as level positions

    ForbidTable tab;
    tab.idx = as_int_vec(idxR);
    const int m = tab.m();
    if (m <= 0)
      stop("skpr: forbidden_tables element has empty idx.");
    if (codesR.ncol() != m)
      stop("skpr: forbidden_tables codes ncol mismatch.");

    tab.stride.assign(m, 0);
    tab.packed_ok = true;
    std::uint64_t s = 1;
    for (int j = 0; j < m; ++j) {
      const int v = tab.idx[j];
      if (v < 0 || v >= q_)
        stop("skpr: forbidden_tables idx out of range.");
      const int Lv = L_[v];
      if (Lv <= 0) {
        tab.packed_ok = false;
        break;
      }
      tab.stride[j] = s;
      if (would_overflow_mul_u64(s, static_cast<std::uint64_t>(Lv))) {
        tab.packed_ok = false;
        break;
      }
      s *= static_cast<std::uint64_t>(Lv);
    }

    const int n_forb = codesR.nrow();
    if (tab.packed_ok) {
      tab.keys.reserve(static_cast<size_t>(n_forb) * 2);
      for (int r = 0; r < n_forb; ++r) {
        std::uint64_t key = 0;
        for (int j = 0; j < m; ++j) {
          const int code = codesR(r, j);
          key += static_cast<std::uint64_t>(code) * tab.stride[j];
        }
        tab.keys.insert(key);
      }
    } else {
      tab.tuples.reserve(n_forb);
      for (int r = 0; r < n_forb; ++r) {
        std::vector<int> tup(m);
        for (int j = 0; j < m; ++j)
          tup[j] = codesR(r, j);
        tab.tuples.push_back(tup);
      }
    }

    forb_tables_.push_back(std::move(tab));
  }
}

bool ConstraintSet::relop(double lhs, int op, double rhs) {
  if (op == 1)
    return lhs == rhs;
  if (op == 2)
    return lhs != rhs;
  if (op == 3)
    return lhs < rhs;
  if (op == 4)
    return lhs <= rhs;
  if (op == 5)
    return lhs > rhs;
  if (op == 6)
    return lhs >= rhs;
  return false;
}

double ConstraintSet::to_constraint_value(int var, double raw_value) const {
  if (var >= 0 && var < q_ && factor_kind_[var] == 1) {
    return value_offset_[var] + value_scale_[var] * raw_value;
  }
  return raw_value;
}

double ConstraintSet::get_value(const Eigen::RowVectorXd &row_values,
                                int var_changed, double new_value,
                                int var_query) const {
  const double raw =
      (var_query == var_changed) ? new_value : row_values(var_query);
  return to_constraint_value(var_query, raw);
}

double ConstraintSet::lin_coef_for_var(int lin_payload, int var) const {
  const int start = lin_ptr_[lin_payload];
  const int end = lin_ptr_[lin_payload + 1];
  for (int k = start; k < end; ++k) {
    if (lin_idx_[k] == var)
      return lin_coef_[k];
  }
  return 0.0;
}

double ConstraintSet::lin_lhs_current(const Eigen::RowVectorXd &row_values,
                                      int lin_payload) const {
  double lhs = lin_const_[lin_payload];
  const int start = lin_ptr_[lin_payload];
  const int end = lin_ptr_[lin_payload + 1];
  for (int k = start; k < end; ++k) {
    const int v = lin_idx_[k];
    lhs += lin_coef_[k] * to_constraint_value(v, row_values(v));
  }
  return lhs;
}

double ConstraintSet::lin_lhs_changed(const RowCache &cache, int lin_payload,
                                      int var_changed, double old_value,
                                      double new_value) const {
  const double coef = lin_coef_for_var(lin_payload, var_changed);
  double delta = (new_value - old_value);
  if (var_changed >= 0 && var_changed < q_ && factor_kind_[var_changed] == 1) {
    delta *= value_scale_[var_changed];
  }
  return cache.lin_lhs[lin_payload] + coef * delta;
}

bool ConstraintSet::in_membership(int in_payload, double value,
                                  int code) const {
  const int v = in_var_[in_payload];
  if (v >= 0 && v < q_ && factor_kind_[v] == 0 &&
      in_mask_ptr_[in_payload] >= 0) {
    const int Lv = L_[v];
    if (code < 0 || code >= Lv)
      return false;
    return in_mask_[in_mask_ptr_[in_payload] + code] != 0;
  }

  const int start = in_ptr_[in_payload];
  const int end = in_ptr_[in_payload + 1];
  if (end <= start)
    return false;
  const double *b = &in_values_[start];
  const double *e = &in_values_[end];
  return std::binary_search(b, e, value);
}

unsigned char
ConstraintSet::eval_atom_current(const Eigen::RowVectorXd &row_values,
                                 const int *row_codes, const RowCache &cache,
                                 int atom_id) const {
  const int t = atom_type_[atom_id];
  const int u = atom_payload_idx_[atom_id];

  if (t == 1) {
    const int v = cmp_var_[u];
    if (factor_kind_[v] == 0) {
      const int x = row_codes[v];
      const int c = static_cast<int>(std::llround(cmp_value_[u]));
      return relop(static_cast<double>(x), cmp_op_[u], static_cast<double>(c))
                 ? 1
                 : 0;
    }
    return relop(to_constraint_value(v, row_values(v)), cmp_op_[u], cmp_value_[u])
               ? 1
               : 0;
  }

  if (t == 2) {
    const int v = in_var_[u];
    const int code = (factor_kind_[v] == 0) ? row_codes[v] : -1;
    const bool in = in_membership(u, to_constraint_value(v, row_values(v)), code);
    const bool neg = (in_neg_[u] != 0);
    return (neg ? !in : in) ? 1 : 0;
  }

  if (t == 3) {
    const double lhs = cache.lin_lhs[u];
    return relop(lhs, lin_op_[u], lin_rhs_[u]) ? 1 : 0;
  }

  if (t == 4) {
    const int start = forb_ptr_[u];
    const int end = forb_ptr_[u + 1];
    for (int k = start; k < end; ++k) {
      const int v = forb_idx_[k];
      if (factor_kind_[v] == 0) {
        const int x = row_codes[v];
        const int c = static_cast<int>(std::llround(forb_value_[k]));
        if (x != c)
          return 1;
      } else {
        if (to_constraint_value(v, row_values(v)) != forb_value_[k])
          return 1;
      }
    }
    return 0;
  }

  return 0;
}

unsigned char
ConstraintSet::eval_atom_changed(const Eigen::RowVectorXd &row_values,
                                 const int *row_codes, const RowCache &cache,
                                 int atom_id, int var_changed, double new_value,
                                 int new_code) const {
  const int t = atom_type_[atom_id];
  const int u = atom_payload_idx_[atom_id];

  if (t == 1) {
    const int v = cmp_var_[u];
    if (factor_kind_[v] == 0) {
      const int x = (v == var_changed) ? new_code : row_codes[v];
      const int c = static_cast<int>(std::llround(cmp_value_[u]));
      return relop(static_cast<double>(x), cmp_op_[u], static_cast<double>(c))
                 ? 1
                 : 0;
    }
    const double x = get_value(row_values, var_changed, new_value, v);
    return relop(x, cmp_op_[u], cmp_value_[u]) ? 1 : 0;
  }

  if (t == 2) {
    const int v = in_var_[u];
    const int code = (factor_kind_[v] == 0)
                         ? ((v == var_changed) ? new_code : row_codes[v])
                         : -1;
    const double x = get_value(row_values, var_changed, new_value, v);
    const bool in = in_membership(u, x, code);
    const bool neg = (in_neg_[u] != 0);
    return (neg ? !in : in) ? 1 : 0;
  }

  if (t == 3) {
    const double old_value = row_values(var_changed);
    const double lhs =
        lin_lhs_changed(cache, u, var_changed, old_value, new_value);
    return relop(lhs, lin_op_[u], lin_rhs_[u]) ? 1 : 0;
  }

  if (t == 4) {
    const int start = forb_ptr_[u];
    const int end = forb_ptr_[u + 1];
    for (int k = start; k < end; ++k) {
      const int v = forb_idx_[k];
      if (factor_kind_[v] == 0) {
        const int x = (v == var_changed) ? new_code : row_codes[v];
        const int c = static_cast<int>(std::llround(forb_value_[k]));
        if (x != c)
          return 1;
      } else {
        const double x = get_value(row_values, var_changed, new_value, v);
        if (x != forb_value_[k])
          return 1;
      }
    }
    return 0;
  }

  return 0;
}

std::uint64_t ConstraintSet::pack_key(const ForbidTable &tab,
                                      const int *row_codes) const {
  std::uint64_t key = 0;
  const int m = tab.m();
  for (int j = 0; j < m; ++j) {
    const int v = tab.idx[j];
    key += static_cast<std::uint64_t>(row_codes[v]) * tab.stride[j];
  }
  return key;
}

unsigned char ConstraintSet::forbid_hit_current(const int *row_codes,
                                                RowCache &cache) const {
  if (forb_tables_.empty())
    return 0;

  unsigned char any_hit = 0;
  for (size_t t = 0; t < forb_tables_.size(); ++t) {
    const ForbidTable &tab = forb_tables_[t];
    unsigned char hit = 0;
    if (tab.packed_ok) {
      std::uint64_t key = pack_key(tab, row_codes);
      cache.forb_key[t] = key;
      hit = (tab.keys.find(key) != tab.keys.end()) ? 1 : 0;
    } else {
      const int m = tab.m();
      for (size_t r = 0; r < tab.tuples.size(); ++r) {
        bool match = true;
        for (int j = 0; j < m; ++j) {
          const int v = tab.idx[j];
          if (row_codes[v] != tab.tuples[r][j]) {
            match = false;
            break;
          }
        }
        if (match) {
          hit = 1;
          break;
        }
      }
    }
    cache.forb_hit[t] = hit;
    if (hit)
      any_hit = 1;
  }
  return any_hit;
}

unsigned char ConstraintSet::forbid_hit_changed(const int *row_codes,
                                                const RowCache &cache, int var,
                                                int old_code,
                                                int new_code) const {
  if (forb_tables_.empty())
    return 0;

  for (size_t t = 0; t < forb_tables_.size(); ++t) {
    const ForbidTable &tab = forb_tables_[t];

    int pos = -1;
    for (int j = 0; j < tab.m(); ++j) {
      if (tab.idx[j] == var) {
        pos = j;
        break;
      }
    }

    unsigned char hit = cache.forb_hit[t];
    if (pos >= 0) {
      if (tab.packed_ok) {
        const std::uint64_t key_old = cache.forb_key[t];
        const std::int64_t delta =
            static_cast<std::int64_t>(new_code) -
            static_cast<std::int64_t>(old_code);
        std::uint64_t key_new = key_old;
        if (delta >= 0) {
          key_new += static_cast<std::uint64_t>(delta) * tab.stride[pos];
        } else {
          key_new -= static_cast<std::uint64_t>(-delta) * tab.stride[pos];
        }
        hit = (tab.keys.find(key_new) != tab.keys.end()) ? 1 : 0;
      } else {
        hit = 0;
        const int m = tab.m();
        for (size_t r = 0; r < tab.tuples.size(); ++r) {
          bool match = true;
          for (int j = 0; j < m; ++j) {
            const int v = tab.idx[j];
            const int code = (v == var) ? new_code : row_codes[v];
            if (code != tab.tuples[r][j]) {
              match = false;
              break;
            }
          }
          if (match) {
            hit = 1;
            break;
          }
        }
      }
    }

    if (hit)
      return 1;
  }

  return 0;
}

ConstraintSet::RowCache
ConstraintSet::make_cache(const Eigen::RowVectorXd &row_values,
                          const int *row_codes) const {
  RowCache cache;
  const int n_atoms = static_cast<int>(atom_type_.size());
  const int n_clauses = static_cast<int>(clause_ptr_.size()) - 1;

  cache.atom_truth.assign(n_atoms, 0);
  cache.clause_unsat.assign(std::max(0, n_clauses), 0);

  cache.lin_lhs.assign(lin_op_.size(), 0.0);
  for (int u = 0; u < static_cast<int>(lin_op_.size()); ++u) {
    cache.lin_lhs[u] = lin_lhs_current(row_values, u);
  }

  cache.forb_hit.assign(forb_tables_.size(), 0);
  cache.forb_key.assign(forb_tables_.size(), 0);
  forbid_hit_current(row_codes, cache);

  for (int a = 0; a < n_atoms; ++a) {
    cache.atom_truth[a] = eval_atom_current(row_values, row_codes, cache, a);
  }

  cache.satisfied_count = 0;
  for (int c = 0; c < n_clauses; ++c) {
    int unsat = 0;
    const int a0 = clause_ptr_[c];
    const int a1 = clause_ptr_[c + 1];
    for (int k = a0; k < a1; ++k) {
      const int atom_id = clause_atom_[k];
      if (cache.atom_truth[atom_id] == 0)
        ++unsat;
    }
    cache.clause_unsat[c] = unsat;
    if (unsat == 0)
      ++cache.satisfied_count;
  }

  return cache;
}

bool ConstraintSet::allowed_row(const Eigen::RowVectorXd &row_values,
                                const int *row_codes) const {
  RowCache cache = make_cache(row_values, row_codes);
  if (!forb_tables_.empty()) {
    for (size_t t = 0; t < cache.forb_hit.size(); ++t) {
      if (cache.forb_hit[t])
        return false;
    }
  }
  return cache.satisfied_count > 0;
}

bool ConstraintSet::allowed_change(const Eigen::RowVectorXd &row_values,
                                   const int *row_codes, const RowCache &cache,
                                   int var, double new_value,
                                   int new_code) const {
  const int n_clauses = static_cast<int>(clause_ptr_.size()) - 1;
  if (n_clauses <= 0)
    return false;

  if (!forb_tables_.empty()) {
    const int old_code = row_codes[var];
    if (forbid_hit_changed(row_codes, cache, var, old_code, new_code))
      return false;
  }

  const std::vector<int> &affected_atoms = atoms_by_factor_[var];
  if (affected_atoms.empty()) {
    return cache.satisfied_count > 0;
  }

  std::vector<int> touched;
  std::vector<int> delta;
  touched.reserve(16);
  delta.reserve(16);

  auto touch_clause = [&](int c) -> int {
    for (int i = 0; i < static_cast<int>(touched.size()); ++i) {
      if (touched[i] == c)
        return i;
    }
    touched.push_back(c);
    delta.push_back(0);
    return static_cast<int>(touched.size()) - 1;
  };

  int sat_new = cache.satisfied_count;

  for (int idx = 0; idx < static_cast<int>(affected_atoms.size()); ++idx) {
    const int a = affected_atoms[idx];
    const unsigned char old_t = cache.atom_truth[a];
    const unsigned char new_t = eval_atom_changed(row_values, row_codes, cache,
                                                  a, var, new_value, new_code);
    if (old_t == new_t)
      continue;

    const int d_unsat = (old_t == 0 && new_t == 1) ? -1 : +1;

    const std::vector<int> &cls = clauses_by_atom_[a];
    for (int ci = 0; ci < static_cast<int>(cls.size()); ++ci) {
      const int c = cls[ci];
      const int pos = touch_clause(c);

      const int old_unsat = cache.clause_unsat[c] + delta[pos];
      const int new_unsat = old_unsat + d_unsat;

      if (old_unsat == 0 && new_unsat > 0)
        --sat_new;
      else if (old_unsat > 0 && new_unsat == 0)
        ++sat_new;

      delta[pos] += d_unsat;
    }
  }

  return sat_new > 0;
}

void ConstraintSet::apply_change(const Eigen::RowVectorXd &row_values,
                                 const int *row_codes, RowCache &cache, int var,
                                 double old_value, int old_code,
                                 double new_value, int new_code) const {
  const int n_clauses = static_cast<int>(clause_ptr_.size()) - 1;
  if (n_clauses <= 0) {
    cache.satisfied_count = 0;
    return;
  }

  if (!forb_tables_.empty()) {
    for (size_t t = 0; t < forb_tables_.size(); ++t) {
      const ForbidTable &tab = forb_tables_[t];
      int pos = -1;
      for (int j = 0; j < tab.m(); ++j) {
        if (tab.idx[j] == var) {
          pos = j;
          break;
        }
      }
      if (pos >= 0) {
        if (tab.packed_ok) {
          const std::int64_t d = static_cast<std::int64_t>(new_code) -
                                 static_cast<std::int64_t>(old_code);
          if (d >= 0) {
            cache.forb_key[t] += static_cast<std::uint64_t>(d) * tab.stride[pos];
          } else {
            cache.forb_key[t] -= static_cast<std::uint64_t>(-d) * tab.stride[pos];
          }
          cache.forb_hit[t] =
              (tab.keys.find(cache.forb_key[t]) != tab.keys.end()) ? 1 : 0;
        } else {
          unsigned char hit = 0;
          const int m = tab.m();
          for (size_t r = 0; r < tab.tuples.size(); ++r) {
            bool match = true;
            for (int jj = 0; jj < m; ++jj) {
              const int v = tab.idx[jj];
              const int code = (v == var) ? new_code : row_codes[v];
              if (code != tab.tuples[r][jj]) {
                match = false;
                break;
              }
            }
            if (match) {
              hit = 1;
              break;
            }
          }
          cache.forb_hit[t] = hit;
        }
      }
    }
  }

  const std::vector<int> &affected_atoms = atoms_by_factor_[var];
  if (affected_atoms.empty())
    return;

  for (int idx = 0; idx < static_cast<int>(affected_atoms.size()); ++idx) {
    const int a = affected_atoms[idx];
    if (atom_type_[a] == 3) {
      const int u = atom_payload_idx_[a];
      const double coef = lin_coef_for_var(u, var);
      double delta = (new_value - old_value);
      if (var >= 0 && var < q_ && factor_kind_[var] == 1) {
        delta *= value_scale_[var];
      }
      cache.lin_lhs[u] += coef * delta;
    }
  }

  for (int idx = 0; idx < static_cast<int>(affected_atoms.size()); ++idx) {
    const int a = affected_atoms[idx];
    const unsigned char old_t = cache.atom_truth[a];
    const unsigned char new_t =
        eval_atom_current(row_values, row_codes, cache, a);
    if (old_t == new_t)
      continue;

    const int d_unsat = (old_t == 0 && new_t == 1) ? -1 : +1;

    const std::vector<int> &cls = clauses_by_atom_[a];
    for (int ci = 0; ci < static_cast<int>(cls.size()); ++ci) {
      const int c = cls[ci];
      const int old_unsat = cache.clause_unsat[c];
      const int new_unsat = old_unsat + d_unsat;

      if (old_unsat == 0 && new_unsat > 0)
        --cache.satisfied_count;
      else if (old_unsat > 0 && new_unsat == 0)
        ++cache.satisfied_count;

      cache.clause_unsat[c] = new_unsat;
    }

    cache.atom_truth[a] = new_t;
  }
}
