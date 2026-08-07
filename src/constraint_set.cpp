#include "constraint_set.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <set>
#include <string>

using namespace Rcpp;

namespace {

std::vector<int> as_int_vec(const IntegerVector &x) {
  return std::vector<int>(x.begin(), x.end());
}

std::vector<double> as_double_vec(const NumericVector &x) {
  return std::vector<double>(x.begin(), x.end());
}

void require_field(const List &ir, const char *name) {
  if (!ir.containsElementNamed(name)) {
    stop("skpr: constraints_ir missing '%s'.", name);
  }
}

SEXP get_field(const List &ir, const char *name) {
  require_field(ir, name);
  return ir[name];
}

IntegerVector require_integer_vector(const List &ir, const char *name) {
  SEXP value = get_field(ir, name);
  if (TYPEOF(value) != INTSXP) {
    stop("skpr: constraints_ir %s must be integer.", name);
  }
  IntegerVector result(value);
  for (int item : result) {
    if (item == NA_INTEGER) {
      stop("skpr: constraints_ir %s cannot contain NA.", name);
    }
  }
  return result;
}

int require_integer_scalar(const List &ir, const char *name) {
  IntegerVector value = require_integer_vector(ir, name);
  if (value.size() != 1) {
    stop("skpr: constraints_ir %s must be an integer scalar.", name);
  }
  return value[0];
}

NumericVector require_numeric_vector(const List &ir, const char *name) {
  SEXP value = get_field(ir, name);
  if (TYPEOF(value) != REALSXP && TYPEOF(value) != INTSXP) {
    stop("skpr: constraints_ir %s must be numeric.", name);
  }
  return as<NumericVector>(value);
}

double require_numeric_scalar(const List &ir, const char *name) {
  NumericVector value = require_numeric_vector(ir, name);
  if (value.size() != 1 || !std::isfinite(value[0])) {
    stop("skpr: constraints_ir %s must be a finite numeric scalar.", name);
  }
  return value[0];
}

void validate_pointer(const std::vector<int> &ptr, int payload_size,
                      const char *name) {
  if (ptr.empty() || ptr.front() != 0) {
    stop("skpr: constraints_ir %s must start at zero.", name);
  }
  for (std::size_t i = 1; i < ptr.size(); ++i) {
    if (ptr[i] < ptr[i - 1]) {
      stop("skpr: constraints_ir %s must be monotone.", name);
    }
  }
  if (ptr.back() != payload_size) {
    stop("skpr: constraints_ir %s terminal value is invalid.", name);
  }
}

void validate_finite(const std::vector<double> &x, const char *name) {
  for (double value : x) {
    if (!std::isfinite(value)) {
      stop("skpr: constraints_ir %s must contain finite values.", name);
    }
  }
}

} // namespace

ConstraintSet::ConstraintSet(const List &ir) {
  const char *required[] = {
      "version",          "q",          "comparison_tol", "factor_kind",
      "L",                "level_ptr",  "level_value",    "clause_ptr",
      "clause_atom",      "atom_type",  "atom_payload_idx", "cmp_var",
      "cmp_op",           "cmp_code",   "in_var",         "in_neg",
      "in_ptr",           "in_code",    "lin_op",         "lin_rhs",
      "lin_const",        "lin_ptr",    "lin_idx",        "lin_coef",
      "forb_ptr",         "forb_idx",   "forb_code",      "forbidden_tables"};
  for (const char *name : required) {
    require_field(ir, name);
  }

  const int version = require_integer_scalar(ir, "version");
  if (version != 1) {
    stop("skpr: unsupported constraints_ir version; expected version 1.");
  }

  q_ = require_integer_scalar(ir, "q");
  if (q_ <= 0) {
    stop("skpr: constraints_ir q must be positive.");
  }
  comparison_tol_ = require_numeric_scalar(ir, "comparison_tol");
  if (comparison_tol_ < 0.0) {
    stop("skpr: constraints_ir comparison_tol must be finite and nonnegative.");
  }

  factor_kind_ = as_int_vec(require_integer_vector(ir, "factor_kind"));
  L_ = as_int_vec(require_integer_vector(ir, "L"));
  level_ptr_ = as_int_vec(require_integer_vector(ir, "level_ptr"));
  level_value_ = as_double_vec(require_numeric_vector(ir, "level_value"));

  clause_ptr_ = as_int_vec(require_integer_vector(ir, "clause_ptr"));
  clause_atom_ = as_int_vec(require_integer_vector(ir, "clause_atom"));
  atom_type_ = as_int_vec(require_integer_vector(ir, "atom_type"));
  atom_payload_idx_ = as_int_vec(
      require_integer_vector(ir, "atom_payload_idx"));

  cmp_var_ = as_int_vec(require_integer_vector(ir, "cmp_var"));
  cmp_op_ = as_int_vec(require_integer_vector(ir, "cmp_op"));
  cmp_code_ = as_int_vec(require_integer_vector(ir, "cmp_code"));
  in_var_ = as_int_vec(require_integer_vector(ir, "in_var"));
  in_neg_ = as_int_vec(require_integer_vector(ir, "in_neg"));
  in_ptr_ = as_int_vec(require_integer_vector(ir, "in_ptr"));
  in_code_ = as_int_vec(require_integer_vector(ir, "in_code"));

  lin_op_ = as_int_vec(require_integer_vector(ir, "lin_op"));
  lin_rhs_ = as_double_vec(require_numeric_vector(ir, "lin_rhs"));
  lin_const_ = as_double_vec(require_numeric_vector(ir, "lin_const"));
  lin_ptr_ = as_int_vec(require_integer_vector(ir, "lin_ptr"));
  lin_idx_ = as_int_vec(require_integer_vector(ir, "lin_idx"));
  lin_coef_ = as_double_vec(require_numeric_vector(ir, "lin_coef"));

  forb_ptr_ = as_int_vec(require_integer_vector(ir, "forb_ptr"));
  forb_idx_ = as_int_vec(require_integer_vector(ir, "forb_idx"));
  forb_code_ = as_int_vec(require_integer_vector(ir, "forb_code"));

  if (static_cast<int>(factor_kind_.size()) != q_ ||
      static_cast<int>(L_.size()) != q_) {
    stop("skpr: constraints_ir factor_kind/L lengths must equal q.");
  }
  if (static_cast<int>(level_ptr_.size()) != q_ + 1) {
    stop("skpr: constraints_ir level_ptr length must equal q + 1.");
  }
  validate_pointer(level_ptr_, static_cast<int>(level_value_.size()),
                   "level_ptr");
  validate_finite(level_value_, "level_value");
  for (int j = 0; j < q_; ++j) {
    if (factor_kind_[j] != 0 && factor_kind_[j] != 1) {
      stop("skpr: constraints_ir factor_kind entries must be zero or one.");
    }
    if (L_[j] <= 0 || level_ptr_[j + 1] - level_ptr_[j] != L_[j]) {
      stop("skpr: constraints_ir level table length must match L.");
    }
    if (factor_kind_[j] == 1) {
      for (int code = 1; code < L_[j]; ++code) {
        if (!(level_value(j, code) > level_value(j, code - 1))) {
          stop("skpr: numeric constraint levels must be strictly increasing.");
        }
      }
    }
  }

  validate_pointer(clause_ptr_, static_cast<int>(clause_atom_.size()),
                   "clause_ptr");
  if (atom_type_.size() != atom_payload_idx_.size()) {
    stop("skpr: constraints_ir atom arrays must have equal lengths.");
  }
  for (int atom : clause_atom_) {
    if (atom < 0 || atom >= static_cast<int>(atom_type_.size())) {
      stop("skpr: constraints_ir clause_atom out of range.");
    }
  }

  if (cmp_var_.size() != cmp_op_.size() ||
      cmp_var_.size() != cmp_code_.size()) {
    stop("skpr: constraints_ir comparison arrays must have equal lengths.");
  }
  for (std::size_t i = 0; i < cmp_var_.size(); ++i) {
    const int var = cmp_var_[i];
    if (var < 0 || var >= q_ || cmp_op_[i] < 1 || cmp_op_[i] > 6 ||
        cmp_code_[i] < 0 || cmp_code_[i] >= L_[var]) {
      stop("skpr: constraints_ir comparison payload is invalid.");
    }
    if (factor_kind_[var] == 0 && cmp_op_[i] > 2) {
      stop("skpr: ordering comparisons are invalid for discrete factors.");
    }
  }

  if (in_var_.size() != in_neg_.size() ||
      in_ptr_.size() != in_var_.size() + 1) {
    stop("skpr: constraints_ir membership arrays have invalid lengths.");
  }
  validate_pointer(in_ptr_, static_cast<int>(in_code_.size()), "in_ptr");
  for (std::size_t i = 0; i < in_var_.size(); ++i) {
    const int var = in_var_[i];
    if (var < 0 || var >= q_ || (in_neg_[i] != 0 && in_neg_[i] != 1) ||
        in_ptr_[i] == in_ptr_[i + 1]) {
      stop("skpr: constraints_ir membership payload is invalid.");
    }
    for (int k = in_ptr_[i]; k < in_ptr_[i + 1]; ++k) {
      if (in_code_[k] < 0 || in_code_[k] >= L_[var]) {
        stop("skpr: constraints_ir membership code out of range.");
      }
    }
  }

  if (lin_op_.size() != lin_rhs_.size() ||
      lin_op_.size() != lin_const_.size() ||
      lin_ptr_.size() != lin_op_.size() + 1 ||
      lin_idx_.size() != lin_coef_.size()) {
    stop("skpr: constraints_ir linear arrays have invalid lengths.");
  }
  validate_pointer(lin_ptr_, static_cast<int>(lin_idx_.size()), "lin_ptr");
  validate_finite(lin_rhs_, "lin_rhs");
  validate_finite(lin_const_, "lin_const");
  validate_finite(lin_coef_, "lin_coef");
  for (std::size_t i = 0; i < lin_op_.size(); ++i) {
    if (lin_op_[i] < 1 || lin_op_[i] > 6 ||
        lin_ptr_[i] == lin_ptr_[i + 1]) {
      stop("skpr: constraints_ir linear payload is invalid.");
    }
    std::set<int> seen;
    for (int k = lin_ptr_[i]; k < lin_ptr_[i + 1]; ++k) {
      const int var = lin_idx_[k];
      if (var < 0 || var >= q_ || factor_kind_[var] != 1 ||
          !seen.insert(var).second) {
        stop("skpr: constraints_ir linear factor index is invalid.");
      }
    }
  }

  if (forb_ptr_.empty()) {
    stop("skpr: constraints_ir forb_ptr cannot be empty.");
  }
  const int forbidden_atom_count =
      static_cast<int>(std::count(atom_type_.begin(), atom_type_.end(), 4));
  if (static_cast<int>(forb_ptr_.size()) != forbidden_atom_count + 1) {
    stop("skpr: constraints_ir forb_ptr has the wrong length.");
  }
  if (forb_idx_.size() != forb_code_.size()) {
    stop("skpr: constraints_ir forbidden atom arrays must have equal lengths.");
  }
  validate_pointer(forb_ptr_, static_cast<int>(forb_idx_.size()), "forb_ptr");
  for (std::size_t i = 0; i + 1 < forb_ptr_.size(); ++i) {
    if (forb_ptr_[i] == forb_ptr_[i + 1]) {
      stop("skpr: constraints_ir forbidden atoms cannot be empty.");
    }
    std::set<int> seen;
    for (int k = forb_ptr_[i]; k < forb_ptr_[i + 1]; ++k) {
      const int var = forb_idx_[k];
      if (var < 0 || var >= q_ || forb_code_[k] < 0 ||
          forb_code_[k] >= L_[var] || !seen.insert(var).second) {
        stop("skpr: constraints_ir forbidden atom payload is invalid.");
      }
    }
  }

  for (std::size_t atom = 0; atom < atom_type_.size(); ++atom) {
    const int type = atom_type_[atom];
    const int payload = atom_payload_idx_[atom];
    int payload_count = 0;
    if (type == 1) {
      payload_count = static_cast<int>(cmp_var_.size());
    } else if (type == 2) {
      payload_count = static_cast<int>(in_var_.size());
    } else if (type == 3) {
      payload_count = static_cast<int>(lin_op_.size());
    } else if (type == 4) {
      payload_count = static_cast<int>(forb_ptr_.size()) - 1;
    } else {
      stop("skpr: constraints_ir atom_type is invalid.");
    }
    if (payload < 0 || payload >= payload_count) {
      stop("skpr: constraints_ir atom_payload_idx out of range.");
    }
  }

  SEXP tables_sexp = get_field(ir, "forbidden_tables");
  if (TYPEOF(tables_sexp) != VECSXP) {
    stop("skpr: constraints_ir forbidden_tables must be a list.");
  }
  List tables(tables_sexp);
  forb_tables_.reserve(tables.size());
  for (int table_index = 0; table_index < tables.size(); ++table_index) {
    if (TYPEOF(tables[table_index]) != VECSXP) {
      stop("skpr: constraints_ir forbidden table must be a list.");
    }
    List table_r = tables[table_index];
    require_field(table_r, "idx");
    require_field(table_r, "codes");
    IntegerVector idx_r = require_integer_vector(table_r, "idx");
    SEXP codes_sexp = get_field(table_r, "codes");
    if (TYPEOF(codes_sexp) != INTSXP || !Rf_isMatrix(codes_sexp)) {
      stop("skpr: constraints_ir forbidden table codes must be an integer matrix.");
    }
    IntegerMatrix codes_r(codes_sexp);
    if (idx_r.size() <= 0 || codes_r.ncol() != idx_r.size() ||
        codes_r.nrow() <= 0) {
      stop("skpr: constraints_ir forbidden table dimensions are invalid.");
    }

    ForbidTable table;
    table.idx = as_int_vec(idx_r);
    std::set<int> seen;
    for (int var : table.idx) {
      if (var < 0 || var >= q_ || !seen.insert(var).second) {
        stop("skpr: constraints_ir forbidden table index is invalid.");
      }
    }
    std::set<std::vector<int>> unique_rows;
    for (int row = 0; row < codes_r.nrow(); ++row) {
      std::vector<int> tuple(table.idx.size());
      for (int col = 0; col < codes_r.ncol(); ++col) {
        const int code = codes_r(row, col);
        if (code == NA_INTEGER || code < 0 || code >= L_[table.idx[col]]) {
          stop("skpr: constraints_ir forbidden table code is invalid.");
        }
        tuple[col] = code;
      }
      unique_rows.insert(tuple);
    }
    table.tuples.assign(unique_rows.begin(), unique_rows.end());
    forb_tables_.push_back(std::move(table));
  }
}

double ConstraintSet::level_value(int var, int code) const {
  return level_value_[level_ptr_[var] + code];
}

void ConstraintSet::validate_codes(const int *row_codes) const {
  for (int var = 0; var < q_; ++var) {
    if (row_codes[var] == NA_INTEGER || row_codes[var] < 0 ||
        row_codes[var] >= L_[var]) {
      stop("skpr: constraint row code is out of range.");
    }
  }
}

bool ConstraintSet::code_relop(int lhs, int op, int rhs) {
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
  return lhs >= rhs;
}

bool ConstraintSet::relop(double lhs, int op, double rhs) const {
  const double band =
      comparison_tol_ * std::max(1.0, std::max(std::fabs(lhs), std::fabs(rhs)));
  const double difference = lhs - rhs;
  if (op == 1)
    return std::fabs(difference) <= band;
  if (op == 2)
    return std::fabs(difference) > band;
  if (op == 3)
    return difference < -band;
  if (op == 4)
    return difference <= band;
  if (op == 5)
    return difference > band;
  return difference >= -band;
}

bool ConstraintSet::membership(int payload, int code) const {
  return std::binary_search(in_code_.begin() + in_ptr_[payload],
                            in_code_.begin() + in_ptr_[payload + 1], code);
}

bool ConstraintSet::atom_true(int atom_id, const int *row_codes) const {
  const int type = atom_type_[atom_id];
  const int payload = atom_payload_idx_[atom_id];
  if (type == 1) {
    return code_relop(row_codes[cmp_var_[payload]], cmp_op_[payload],
                      cmp_code_[payload]);
  }
  if (type == 2) {
    const bool found = membership(payload, row_codes[in_var_[payload]]);
    return in_neg_[payload] ? !found : found;
  }
  if (type == 3) {
    double lhs = lin_const_[payload];
    for (int k = lin_ptr_[payload]; k < lin_ptr_[payload + 1]; ++k) {
      lhs += lin_coef_[k] * level_value(lin_idx_[k], row_codes[lin_idx_[k]]);
    }
    return relop(lhs, lin_op_[payload], lin_rhs_[payload]);
  }

  for (int k = forb_ptr_[payload]; k < forb_ptr_[payload + 1]; ++k) {
    if (row_codes[forb_idx_[k]] != forb_code_[k]) {
      return true;
    }
  }
  return false;
}

bool ConstraintSet::atom_possible(int atom_id, const int *row_codes,
                                  const unsigned char *assigned) const {
  const int type = atom_type_[atom_id];
  const int payload = atom_payload_idx_[atom_id];
  if (type == 1) {
    const int var = cmp_var_[payload];
    if (assigned[var]) {
      return code_relop(row_codes[var], cmp_op_[payload], cmp_code_[payload]);
    }
    for (int code = 0; code < L_[var]; ++code) {
      if (code_relop(code, cmp_op_[payload], cmp_code_[payload])) {
        return true;
      }
    }
    return false;
  }
  if (type == 2) {
    const int var = in_var_[payload];
    if (assigned[var]) {
      const bool found = membership(payload, row_codes[var]);
      return in_neg_[payload] ? !found : found;
    }
    for (int code = 0; code < L_[var]; ++code) {
      const bool found = membership(payload, code);
      if (in_neg_[payload] ? !found : found) {
        return true;
      }
    }
    return false;
  }
  if (type == 3) {
    double minimum = lin_const_[payload];
    double maximum = lin_const_[payload];
    for (int k = lin_ptr_[payload]; k < lin_ptr_[payload + 1]; ++k) {
      const int var = lin_idx_[k];
      const double coefficient = lin_coef_[k];
      if (assigned[var]) {
        const double contribution = coefficient * level_value(var, row_codes[var]);
        minimum += contribution;
        maximum += contribution;
      } else {
        const double low = coefficient * level_value(var, 0);
        const double high = coefficient * level_value(var, L_[var] - 1);
        minimum += std::min(low, high);
        maximum += std::max(low, high);
      }
    }
    const double rhs = lin_rhs_[payload];
    const int op = lin_op_[payload];
    if (op == 1)
      return relop(minimum, 4, rhs) && relop(maximum, 6, rhs);
    if (op == 2)
      return !(relop(minimum, 1, rhs) && relop(maximum, 1, rhs));
    if (op == 3)
      return relop(minimum, 3, rhs);
    if (op == 4)
      return relop(minimum, 4, rhs);
    if (op == 5)
      return relop(maximum, 5, rhs);
    return relop(maximum, 6, rhs);
  }

  for (int k = forb_ptr_[payload]; k < forb_ptr_[payload + 1]; ++k) {
    const int var = forb_idx_[k];
    if (assigned[var]) {
      if (row_codes[var] != forb_code_[k]) {
        return true;
      }
    } else if (L_[var] > 1) {
      return true;
    } else if (forb_code_[k] != 0) {
      return true;
    }
  }
  return false;
}

bool ConstraintSet::forbidden_table_hit(const ForbidTable &table,
                                        const int *row_codes) const {
  for (const std::vector<int> &tuple : table.tuples) {
    bool match = true;
    for (std::size_t col = 0; col < table.idx.size(); ++col) {
      if (row_codes[table.idx[col]] != tuple[col]) {
        match = false;
        break;
      }
    }
    if (match)
      return true;
  }
  return false;
}

bool ConstraintSet::forbidden_table_covers_completions(
    const ForbidTable &table, const int *row_codes,
    const unsigned char *assigned) const {
  std::vector<int> open_columns;
  std::uint64_t combinations = 1;
  for (std::size_t col = 0; col < table.idx.size(); ++col) {
    const int var = table.idx[col];
    if (!assigned[var]) {
      open_columns.push_back(static_cast<int>(col));
      const std::uint64_t levels = static_cast<std::uint64_t>(L_[var]);
      if (combinations > static_cast<std::uint64_t>(table.tuples.size()) /
                             std::max<std::uint64_t>(1, levels)) {
        return false;
      }
      combinations *= levels;
    }
  }

  std::set<std::vector<int>> forbidden_suffixes;
  for (const std::vector<int> &tuple : table.tuples) {
    bool prefix_match = true;
    for (std::size_t col = 0; col < table.idx.size(); ++col) {
      const int var = table.idx[col];
      if (assigned[var] && row_codes[var] != tuple[col]) {
        prefix_match = false;
        break;
      }
    }
    if (!prefix_match)
      continue;
    std::vector<int> suffix;
    suffix.reserve(open_columns.size());
    for (int col : open_columns)
      suffix.push_back(tuple[col]);
    forbidden_suffixes.insert(std::move(suffix));
  }
  return forbidden_suffixes.size() == combinations;
}

bool ConstraintSet::allowed_row(const int *row_codes) const {
  validate_codes(row_codes);

  bool dnf_satisfied = false;
  for (std::size_t clause = 0; clause + 1 < clause_ptr_.size(); ++clause) {
    bool clause_satisfied = true;
    for (int k = clause_ptr_[clause]; k < clause_ptr_[clause + 1]; ++k) {
      if (!atom_true(clause_atom_[k], row_codes)) {
        clause_satisfied = false;
        break;
      }
    }
    if (clause_satisfied) {
      dnf_satisfied = true;
      break;
    }
  }
  if (!dnf_satisfied)
    return false;

  for (const ForbidTable &table : forb_tables_) {
    if (forbidden_table_hit(table, row_codes))
      return false;
  }
  return true;
}

bool ConstraintSet::can_complete(const int *row_codes,
                                 const unsigned char *assigned) const {
  for (int var = 0; var < q_; ++var) {
    if (assigned[var] &&
        (row_codes[var] < 0 || row_codes[var] >= L_[var])) {
      stop("skpr: partial constraint row code is out of range.");
    }
  }

  bool dnf_possible = false;
  for (std::size_t clause = 0; clause + 1 < clause_ptr_.size(); ++clause) {
    bool clause_possible = true;
    for (int k = clause_ptr_[clause]; k < clause_ptr_[clause + 1]; ++k) {
      if (!atom_possible(clause_atom_[k], row_codes, assigned)) {
        clause_possible = false;
        break;
      }
    }
    if (clause_possible) {
      dnf_possible = true;
      break;
    }
  }
  if (!dnf_possible)
    return false;

  for (const ForbidTable &table : forb_tables_) {
    if (forbidden_table_covers_completions(table, row_codes, assigned))
      return false;
  }
  return true;
}
