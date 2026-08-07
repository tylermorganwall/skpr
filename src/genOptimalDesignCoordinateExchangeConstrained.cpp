#include <Rcpp.h>
#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

#include "constraint_set.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <string>
#include <utility>
#include <vector>

using namespace Rcpp;

namespace {

struct SearchDiagnostics {
  long long proposals_generated = 0;
  long long proposals_scored = 0;
  long long partial_nodes = 0;
  long long partial_pruned = 0;
  int capped_groups = 0;
  int model_matrix_calls = 0;
  int information_recomputes = 0;
  int accepted_moves = 0;
  int iterations = 0;
  int initialization_restarts = 0;
  int initialization_pool_size = 0;
  std::string initialization_mode = "provided";
  std::string stop_reason = "max_iter";
  std::vector<double> objective_history;
};

struct InformationState {
  Eigen::MatrixXd matrix;
  Eigen::MatrixXd inverse;
  double logdet = -std::numeric_limits<double>::infinity();
  double reciprocal_condition = 0.0;
  int rank = 0;
  bool valid = false;
};

struct GeneratedRows {
  Eigen::MatrixXd points;
  std::vector<std::vector<int>> codes;
  bool complete = true;
  bool capped = false;
  long long nodes = 0;
  long long pruned = 0;
  double raw_product = 1.0;
};

struct GeneratorState {
  const std::vector<std::vector<double>> &levels;
  const ConstraintSet *constraints;
  std::vector<int> variables;
  std::vector<std::vector<int>> level_order;
  std::vector<int> codes;
  std::vector<int> base_codes;
  std::vector<unsigned char> assigned;
  std::vector<std::vector<int>> rows;
  int limit;
  long long node_budget;
  bool exclude_base;
  bool stopped = false;
  long long nodes = 0;
  long long pruned = 0;

  void visit(int depth) {
    if (stopped)
      return;
    ++nodes;
    if (nodes > node_budget || static_cast<int>(rows.size()) >= limit) {
      stopped = true;
      return;
    }
    if (depth == static_cast<int>(variables.size())) {
      if (constraints != NULL && !constraints->allowed_row(codes.data())) {
        ++pruned;
        return;
      }
      if (exclude_base && codes == base_codes)
        return;
      rows.push_back(codes);
      return;
    }

    const int var = variables[depth];
    for (int code : level_order[depth]) {
      codes[var] = code;
      assigned[var] = 1;
      if (constraints == NULL ||
          constraints->can_complete(codes.data(), assigned.data())) {
        visit(depth + 1);
      } else {
        ++pruned;
      }
      assigned[var] = 0;
      if (stopped)
        return;
    }
  }
};

int random_index(int n) {
  if (n <= 1)
    return 0;
  int value = static_cast<int>(std::floor(R::runif(0.0, n)));
  return std::max(0, std::min(n - 1, value));
}

template <typename T> void shuffle_vector(std::vector<T> &x) {
  for (int i = static_cast<int>(x.size()) - 1; i > 0; --i) {
    const int j = random_index(i + 1);
    std::swap(x[i], x[j]);
  }
}

Eigen::MatrixXd call_model_matrix(Function model_matrix_fn,
                                  const Eigen::MatrixXd &points,
                                  SearchDiagnostics &diagnostics) {
  ++diagnostics.model_matrix_calls;
  NumericMatrix result = model_matrix_fn(wrap(points));
  Eigen::MatrixXd matrix = as<Eigen::MatrixXd>(result);
  if (matrix.rows() != points.rows() || !matrix.allFinite()) {
    stop("skpr: modelmatrix_fn returned invalid dimensions or values.");
  }
  return matrix;
}

InformationState information_from_matrix(const Eigen::MatrixXd &matrix,
                                         int design_rows) {
  InformationState state;
  state.matrix = 0.5 * (matrix + matrix.transpose());
  if (!state.matrix.allFinite() || state.matrix.rows() == 0 ||
      state.matrix.rows() != state.matrix.cols()) {
    return state;
  }

  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> solver(state.matrix);
  if (solver.info() != Eigen::Success)
    return state;
  const Eigen::VectorXd eigenvalues = solver.eigenvalues();
  const double largest = eigenvalues.maxCoeff();
  if (!std::isfinite(largest) || largest <= 0.0)
    return state;
  const double rank_tolerance =
      std::max(design_rows, static_cast<int>(matrix.rows())) *
      std::numeric_limits<double>::epsilon() * largest;
  state.rank = static_cast<int>((eigenvalues.array() > rank_tolerance).count());
  if (state.rank != matrix.rows() || eigenvalues.minCoeff() <= 0.0)
    return state;

  state.logdet = eigenvalues.array().log().sum();
  state.reciprocal_condition = eigenvalues.minCoeff() / largest;
  state.inverse = solver.eigenvectors() * eigenvalues.cwiseInverse().asDiagonal() *
                  solver.eigenvectors().transpose();
  state.valid = std::isfinite(state.logdet) && state.inverse.allFinite();
  return state;
}

InformationState information_from_design(const Eigen::MatrixXd &design) {
  return information_from_matrix(design.transpose() * design, design.rows());
}

int matrix_rank(const Eigen::MatrixXd &matrix) {
  if (matrix.rows() == 0 || matrix.cols() == 0)
    return 0;
  Eigen::JacobiSVD<Eigen::MatrixXd> svd(matrix);
  const Eigen::VectorXd singular = svd.singularValues();
  if (singular.size() == 0)
    return 0;
  const double tolerance = std::max(matrix.rows(), matrix.cols()) *
                           std::numeric_limits<double>::epsilon() * singular(0);
  return static_cast<int>((singular.array() > tolerance).count());
}

struct InitializationScore {
  int rank = -1;
  double log_pseudodeterminant =
      -std::numeric_limits<double>::infinity();
  double reciprocal_condition = 0.0;
  bool valid = false;
};

InitializationScore initialization_score(const Eigen::MatrixXd &information,
                                          int design_rows) {
  InitializationScore score;
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> solver(
      0.5 * (information + information.transpose()));
  if (solver.info() != Eigen::Success)
    return score;
  const Eigen::VectorXd eigenvalues = solver.eigenvalues();
  const double largest = eigenvalues.maxCoeff();
  if (!std::isfinite(largest) || largest <= 0.0)
    return score;
  const double tolerance =
      std::max(design_rows, static_cast<int>(information.rows())) *
      std::numeric_limits<double>::epsilon() * largest;
  score.rank = static_cast<int>((eigenvalues.array() > tolerance).count());
  if (score.rank <= 0)
    return score;
  score.log_pseudodeterminant = 0.0;
  double smallest_positive = largest;
  for (int index = 0; index < eigenvalues.size(); ++index) {
    if (eigenvalues(index) > tolerance) {
      score.log_pseudodeterminant += std::log(eigenvalues(index));
      smallest_positive = std::min(smallest_positive, eigenvalues(index));
    }
  }
  score.reciprocal_condition = smallest_positive / largest;
  score.valid = std::isfinite(score.log_pseudodeterminant) &&
                std::isfinite(score.reciprocal_condition);
  return score;
}

bool better_initialization_score(const InitializationScore &candidate,
                                 const InitializationScore &best) {
  if (!candidate.valid)
    return false;
  if (!best.valid || candidate.rank != best.rank)
    return !best.valid || candidate.rank > best.rank;
  const double log_tolerance =
      64.0 * std::numeric_limits<double>::epsilon() *
      std::max(1.0, std::abs(best.log_pseudodeterminant));
  if (candidate.log_pseudodeterminant >
      best.log_pseudodeterminant + log_tolerance)
    return true;
  if (std::abs(candidate.log_pseudodeterminant -
               best.log_pseudodeterminant) <= log_tolerance) {
    return candidate.reciprocal_condition > best.reciprocal_condition;
  }
  return false;
}

int match_level(double value, const std::vector<double> &levels) {
  int best = -1;
  double best_distance = std::numeric_limits<double>::infinity();
  for (int code = 0; code < static_cast<int>(levels.size()); ++code) {
    const double distance = std::fabs(value - levels[code]);
    if (distance < best_distance) {
      best = code;
      best_distance = distance;
    }
  }
  const double scale = std::max(1.0, std::fabs(value));
  return best_distance <= 1e-10 * scale ? best : -1;
}

std::vector<std::vector<double>> read_factor_levels(const List &factor_levels,
                                                    int q) {
  if (factor_levels.size() != q)
    stop("skpr: factor_levels must have length equal to ncol(points).");
  std::vector<std::vector<double>> levels(q);
  for (int var = 0; var < q; ++var) {
    NumericVector values = factor_levels[var];
    if (values.size() <= 0)
      stop("skpr: factor_levels contains an empty level set.");
    levels[var].assign(values.begin(), values.end());
    for (double value : levels[var]) {
      if (!std::isfinite(value))
        stop("skpr: factor_levels must contain finite values.");
    }
    std::sort(levels[var].begin(), levels[var].end());
    if (std::adjacent_find(levels[var].begin(), levels[var].end()) !=
        levels[var].end()) {
      stop("skpr: factor_levels must contain unique values.");
    }
  }
  return levels;
}

std::vector<std::vector<int>> validate_groups(const List &coordinate_groups,
                                              int q) {
  if (coordinate_groups.size() <= 0)
    stop("skpr: coordinate_groups must contain at least one group.");
  std::vector<std::vector<int>> groups(coordinate_groups.size());
  std::vector<int> seen(q, 0);
  for (int group_index = 0; group_index < coordinate_groups.size();
       ++group_index) {
    IntegerVector group_r = coordinate_groups[group_index];
    if (group_r.size() <= 0)
      stop("skpr: coordinate_groups cannot contain empty groups.");
    for (int value : group_r) {
      const int var = value - 1;
      if (var < 0 || var >= q || seen[var]) {
        stop("skpr: coordinate_groups must partition factors exactly once.");
      }
      seen[var] = 1;
      groups[group_index].push_back(var);
    }
  }
  if (std::find(seen.begin(), seen.end(), 0) != seen.end())
    stop("skpr: coordinate_groups must partition factors exactly once.");
  return groups;
}

GeneratedRows generate_rows(
    const std::vector<int> &base_codes, const std::vector<int> &variables,
    const std::vector<std::vector<double>> &levels,
    const ConstraintSet *constraints, int candidate_limit, long long node_budget,
    bool exclude_base) {
  GeneratedRows result;
  result.raw_product = 1.0;
  for (int var : variables) {
    result.raw_product *= static_cast<double>(levels[var].size());
    if (!std::isfinite(result.raw_product)) {
      result.raw_product = std::numeric_limits<double>::infinity();
      break;
    }
  }

  const bool bounded = result.raw_product > candidate_limit;
  GeneratorState state{levels,
                       constraints,
                       variables,
                       {},
                       base_codes,
                       base_codes,
                       std::vector<unsigned char>(levels.size(), 1),
                       {},
                       candidate_limit + 1,
                       std::max<long long>(node_budget, candidate_limit + 1LL),
                       exclude_base};

  for (int var : variables)
    state.assigned[var] = 0;
  if (bounded)
    shuffle_vector(state.variables);
  state.level_order.reserve(state.variables.size());
  for (int var : state.variables) {
    std::vector<int> order(levels[var].size());
    std::iota(order.begin(), order.end(), 0);
    if (bounded)
      shuffle_vector(order);
    if (exclude_base) {
      const auto position = std::find(order.begin(), order.end(), base_codes[var]);
      if (position != order.end())
        std::rotate(order.begin(), position, position + 1);
    }
    state.level_order.push_back(std::move(order));
  }
  if (constraints == NULL ||
      constraints->can_complete(state.codes.data(), state.assigned.data())) {
    state.visit(0);
  }

  result.nodes = state.nodes;
  result.pruned = state.pruned;
  result.complete = !state.stopped;
  result.capped = static_cast<int>(state.rows.size()) > candidate_limit ||
                  (state.stopped && result.raw_product > candidate_limit);
  if (static_cast<int>(state.rows.size()) > candidate_limit)
    state.rows.resize(candidate_limit);
  result.codes = std::move(state.rows);
  result.points.resize(result.codes.size(), levels.size());
  for (int row = 0; row < static_cast<int>(result.codes.size()); ++row) {
    for (int var = 0; var < static_cast<int>(levels.size()); ++var)
      result.points(row, var) = levels[var][result.codes[row][var]];
  }
  return result;
}

std::vector<int> select_rows_by_leverage(
    const Eigen::MatrixXd &design, const Eigen::MatrixXd &inverse,
    int kexchange, int augmented_rows) {
  const int n = design.rows();
  const int first = std::max(0, std::min(augmented_rows, n));
  if (kexchange < 1)
    stop("skpr: kexchange must be at least 1.");
  const int target = std::min(kexchange, n - first);
  std::vector<std::pair<double, int>> ranked;
  for (int row = first; row < n; ++row) {
    const Eigen::VectorXd value = design.row(row).transpose();
    ranked.emplace_back(value.dot(inverse * value), row);
  }
  std::sort(ranked.begin(), ranked.end(),
            [](const std::pair<double, int> &left,
               const std::pair<double, int> &right) {
              if (left.first == right.first)
                return left.second < right.second;
              return left.first < right.first;
            });
  std::vector<int> selected;
  for (int index = 0; index < target; ++index)
    selected.push_back(ranked[index].second);
  return selected;
}

bool all_rows_feasible(const std::vector<std::vector<int>> &codes,
                       const ConstraintSet *constraints) {
  if (constraints == NULL)
    return true;
  for (const std::vector<int> &row : codes) {
    if (!constraints->allowed_row(row.data()))
      return false;
  }
  return true;
}

bool initialize_from_pool(
    Eigen::MatrixXd &points, std::vector<std::vector<int>> &codes,
    Eigen::MatrixXd &design, const GeneratedRows &pool,
    Function model_matrix_fn, int augmented_rows, SearchDiagnostics &diagnostics,
    bool &support_definitively_singular) {
  support_definitively_singular = false;
  if (pool.points.rows() == 0)
    return false;
  Eigen::MatrixXd pool_design =
      call_model_matrix(model_matrix_fn, pool.points, diagnostics);
  if (pool_design.cols() != design.cols())
    stop("skpr: modelmatrix_fn changed its number of columns.");

  const int n = points.rows();
  const int p = design.cols();
  const int fixed = std::max(0, std::min(augmented_rows, n));
  Eigen::MatrixXd support(fixed + pool_design.rows(), p);
  if (fixed > 0)
    support.topRows(fixed) = design.topRows(fixed);
  support.bottomRows(pool_design.rows()) = pool_design;
  if (matrix_rank(support) < p) {
    support_definitively_singular = pool.complete;
    return false;
  }

  Eigen::MatrixXd selected(n, p);
  if (fixed > 0)
    selected.topRows(fixed) = design.topRows(fixed);
  int selected_count = fixed;
  int current_rank = matrix_rank(selected.topRows(selected_count));
  Eigen::MatrixXd selected_information = Eigen::MatrixXd::Zero(p, p);
  if (fixed > 0) {
    selected_information =
        selected.topRows(fixed).transpose() * selected.topRows(fixed);
  }
  for (int row = fixed; row < n; ++row) {
    int chosen = -1;
    InitializationScore best_score;
    for (int candidate = 0; candidate < pool_design.rows(); ++candidate) {
      const Eigen::VectorXd candidate_row = pool_design.row(candidate);
      const Eigen::MatrixXd trial_information =
          selected_information + candidate_row * candidate_row.transpose();
      const InitializationScore candidate_score =
          initialization_score(trial_information, selected_count + 1);
      if (current_rank < p && candidate_score.rank <= current_rank)
        continue;
      if (better_initialization_score(candidate_score, best_score)) {
        chosen = candidate;
        best_score = candidate_score;
      }
    }
    if (chosen < 0)
      return false;
    points.row(row) = pool.points.row(chosen);
    codes[row] = pool.codes[chosen];
    selected.row(selected_count) = pool_design.row(chosen);
    const Eigen::VectorXd chosen_row = pool_design.row(chosen);
    selected_information += chosen_row * chosen_row.transpose();
    ++selected_count;
    current_rank = best_score.rank;
  }
  design = call_model_matrix(model_matrix_fn, points, diagnostics);
  return information_from_design(design).valid;
}

void ensure_feasible_full_rank_initialization(
    Eigen::MatrixXd &points, std::vector<std::vector<int>> &codes,
    Eigen::MatrixXd &design, const std::vector<std::vector<double>> &levels,
    Function model_matrix_fn, const ConstraintSet *constraints,
    int augmented_rows, int restart_limit, int generation_budget,
    SearchDiagnostics &diagnostics) {
  const int n = points.rows();
  const int fixed = std::max(0, std::min(augmented_rows, n));
  if (constraints != NULL) {
    for (int row = 0; row < fixed; ++row) {
      if (!constraints->allowed_row(codes[row].data())) {
        stop("skpr: Coordinate exchange augmented row violates the provided constraints.");
      }
    }
  }
  if (all_rows_feasible(codes, constraints) &&
      information_from_design(design).valid) {
    diagnostics.initialization_mode = "provided";
    return;
  }
  if (restart_limit <= 0 || generation_budget <= 0) {
    stop("skpr: Coordinate exchange needs positive repair limits for an infeasible or singular start.");
  }

  std::vector<int> variables(levels.size());
  std::iota(variables.begin(), variables.end(), 0);
  std::vector<int> base_codes(levels.size(), 0);
  const int pool_limit = generation_budget;
  const long long node_budget =
      std::max<long long>(pool_limit + 1LL,
                          static_cast<long long>(generation_budget) * 50LL);
  for (int restart = 0; restart < restart_limit; ++restart) {
    ++diagnostics.initialization_restarts;
    GeneratedRows pool = generate_rows(base_codes, variables, levels, constraints,
                                       pool_limit, node_budget, restart > 0);
    diagnostics.partial_nodes += pool.nodes;
    diagnostics.partial_pruned += pool.pruned;
    diagnostics.initialization_pool_size =
        std::max(diagnostics.initialization_pool_size,
                 static_cast<int>(pool.codes.size()));
    bool definitively_singular = false;
    if (initialize_from_pool(points, codes, design, pool, model_matrix_fn,
                             fixed, diagnostics, definitively_singular)) {
      diagnostics.initialization_mode = pool.capped ? "bounded_pool" : "exact_pool";
      return;
    }
    if (definitively_singular) {
      stop("skpr: the complete feasible coordinate grid cannot support a full-rank design for the model.");
    }
  }
  stop("skpr: Coordinate exchange could not construct a full-rank feasible initial design within the configured repair limits.");
}

double determinant_delta(const Eigen::VectorXd &old_row,
                         const Eigen::VectorXd &new_row,
                         const Eigen::MatrixXd &inverse) {
  const double old_leverage = old_row.dot(inverse * old_row);
  const double new_leverage = new_row.dot(inverse * new_row);
  const double cross = new_row.dot(inverse * old_row);
  return 1.0 + new_leverage - old_leverage +
         cross * cross - new_leverage * old_leverage;
}

IntegerMatrix wrap_codes(const std::vector<std::vector<int>> &codes) {
  if (codes.empty())
    return IntegerMatrix(0, 0);
  IntegerMatrix result(codes.size(), codes.front().size());
  for (int row = 0; row < result.nrow(); ++row)
    for (int var = 0; var < result.ncol(); ++var)
      result(row, var) = codes[row][var];
  return result;
}

List diagnostics_list(const SearchDiagnostics &diagnostics,
                      const InformationState &final_information,
                      const std::vector<double> &group_products,
                      double maintained_logdet, double agreement) {
  return List::create(
      _["schema_version"] = 1L,
      _["initialization_mode"] = diagnostics.initialization_mode,
      _["initialization_restarts"] = diagnostics.initialization_restarts,
      _["initialization_pool_size"] = diagnostics.initialization_pool_size,
      _["iterations"] = diagnostics.iterations,
      _["accepted_moves"] = diagnostics.accepted_moves,
      _["proposals_generated"] = static_cast<double>(diagnostics.proposals_generated),
      _["proposals_scored"] = static_cast<double>(diagnostics.proposals_scored),
      _["partial_nodes"] = static_cast<double>(diagnostics.partial_nodes),
      _["partial_pruned"] = static_cast<double>(diagnostics.partial_pruned),
      _["proposals_pruned"] = static_cast<double>(diagnostics.partial_pruned),
      _["capped_groups"] = diagnostics.capped_groups,
      _["model_matrix_calls"] = diagnostics.model_matrix_calls,
      _["information_recomputes"] = diagnostics.information_recomputes,
      _["stop_reason"] = diagnostics.stop_reason,
      _["group_raw_products"] = wrap(group_products),
      _["objective_history"] = wrap(diagnostics.objective_history),
      _["maintained_logdet"] = maintained_logdet,
      _["final_logdet"] = final_information.logdet,
      _["logdet_agreement"] = agreement,
      _["final_rank"] = final_information.rank,
      _["reciprocal_condition"] = final_information.reciprocal_condition);
}

} // namespace

// [[Rcpp::export]]
IntegerVector skpr_ce_select_rows_by_leverage(Eigen::MatrixXd design,
                                              Eigen::MatrixXd inverse,
                                              int kexchange,
                                              int augmentedrows = 0) {
  std::vector<int> rows =
      select_rows_by_leverage(design, inverse, kexchange, augmentedrows);
  IntegerVector result(rows.size());
  for (int index = 0; index < static_cast<int>(rows.size()); ++index)
    result[index] = rows[index] + 1;
  return result;
}

// [[Rcpp::export]]
List genOptimalDesignCoordinateExchangeConstrained(
    Eigen::MatrixXd points, List factor_levels, Function modelmatrix_fn,
    List coordinate_groups, Nullable<List> constraints_ir = R_NilValue,
    double tolerance = 1e-4,
    IntegerVector kexchange = IntegerVector::create(NA_INTEGER),
    int augmentedrows = 0, int max_iter = 200, int recompute_every = 10,
    int repair_stuck_limit = 5, int repair_max_tries = 2000,
    int coordinate_group_max_candidates = 10000) {
  RNGScope rng_scope;
  if (!points.allFinite() || points.rows() <= 0 || points.cols() <= 0)
    stop("skpr: coordinate-exchange points must be a finite non-empty matrix.");
  if (!std::isfinite(tolerance) || tolerance < 0.0 || max_iter < 0 ||
      recompute_every < 0 || repair_stuck_limit < 0 || repair_max_tries < 0 ||
      coordinate_group_max_candidates < 1) {
    stop("skpr: coordinate-exchange controls are invalid.");
  }

  const int n = points.rows();
  const int q = points.cols();
  const int fixed = std::max(0, std::min(augmentedrows, n));
  const std::vector<std::vector<double>> levels =
      read_factor_levels(factor_levels, q);
  const std::vector<std::vector<int>> groups =
      validate_groups(coordinate_groups, q);

  std::unique_ptr<ConstraintSet> constraints;
  if (!constraints_ir.isNull()) {
    constraints.reset(new ConstraintSet(as<List>(constraints_ir)));
    if (constraints->q() != q)
      stop("skpr: constraints_ir q must match ncol(points).");
    for (int var = 0; var < q; ++var) {
      if (constraints->levels(var) != static_cast<int>(levels[var].size()))
        stop("skpr: constraints_ir level counts must match factor_levels.");
    }
  }

  std::vector<std::vector<int>> codes(n, std::vector<int>(q));
  for (int row = 0; row < n; ++row) {
    for (int var = 0; var < q; ++var) {
      codes[row][var] = match_level(points(row, var), levels[var]);
      if (codes[row][var] < 0)
        stop("skpr: initial point is not present in factor_levels.");
    }
  }

  SearchDiagnostics diagnostics;
  Eigen::MatrixXd design = call_model_matrix(modelmatrix_fn, points, diagnostics);
  const int p = design.cols();
  if (p <= 0 || n < p)
    stop("skpr: Too few runs for the coordinate-exchange model matrix.");

  ensure_feasible_full_rank_initialization(
      points, codes, design, levels, modelmatrix_fn, constraints.get(), fixed,
      repair_stuck_limit, repair_max_tries, diagnostics);

  InformationState information = information_from_design(design);
  ++diagnostics.information_recomputes;
  if (!information.valid)
    stop("skpr: Coordinate exchange initialization is numerically singular.");
  diagnostics.objective_history.push_back(information.logdet);

  int k_use = n - fixed;
  if (kexchange.size() > 0 && kexchange[0] != NA_INTEGER)
    k_use = kexchange[0];
  if (k_use < 1)
    stop("skpr: kexchange must be at least 1.");
  k_use = std::min(k_use, n - fixed);

  std::vector<double> group_products(groups.size(), 1.0);
  for (int group_index = 0; group_index < static_cast<int>(groups.size());
       ++group_index) {
    for (int var : groups[group_index])
      group_products[group_index] *= levels[var].size();
  }

  int accepted_since_recompute = 0;
  const double objective_epsilon = 1e-12;
  for (int iteration = 0; iteration < max_iter; ++iteration) {
    diagnostics.iterations = iteration + 1;
    const double sweep_start = information.logdet;
    bool accepted_in_sweep = false;
    const std::vector<int> selected =
        select_rows_by_leverage(design, information.inverse, k_use, fixed);

    for (int row : selected) {
      checkUserInterrupt();
      std::vector<Eigen::RowVectorXd> proposal_points;
      std::vector<std::vector<int>> proposal_codes;
      for (const std::vector<int> &group : groups) {
        bool movable = false;
        for (int var : group)
          movable = movable || levels[var].size() > 1;
        if (!movable)
          continue;
        const long long node_budget =
            std::max<long long>(1000LL,
                                50LL * coordinate_group_max_candidates);
        GeneratedRows generated = generate_rows(
            codes[row], group, levels, constraints.get(),
            coordinate_group_max_candidates, node_budget, true);
        diagnostics.partial_nodes += generated.nodes;
        diagnostics.partial_pruned += generated.pruned;
        diagnostics.proposals_generated += generated.codes.size();
        if (generated.capped)
          ++diagnostics.capped_groups;
        for (int candidate = 0; candidate < generated.points.rows(); ++candidate) {
          proposal_points.push_back(generated.points.row(candidate));
          proposal_codes.push_back(generated.codes[candidate]);
        }
      }
      if (proposal_points.empty())
        continue;

      Eigen::MatrixXd proposal_matrix(proposal_points.size(), q);
      for (int candidate = 0;
           candidate < static_cast<int>(proposal_points.size()); ++candidate)
        proposal_matrix.row(candidate) = proposal_points[candidate];
      Eigen::MatrixXd proposal_design =
          call_model_matrix(modelmatrix_fn, proposal_matrix, diagnostics);
      if (proposal_design.cols() != p)
        stop("skpr: modelmatrix_fn changed its number of columns.");
      diagnostics.proposals_scored += proposal_design.rows();

      const Eigen::VectorXd old_row = design.row(row).transpose();
      double best_delta = 1.0;
      int best = -1;
      for (int candidate = 0; candidate < proposal_design.rows(); ++candidate) {
        const Eigen::VectorXd new_row = proposal_design.row(candidate).transpose();
        const double delta =
            determinant_delta(old_row, new_row, information.inverse);
        if (std::isfinite(delta) && delta > best_delta + objective_epsilon) {
          best_delta = delta;
          best = candidate;
        }
      }
      if (best < 0)
        continue;

      const Eigen::VectorXd new_row = proposal_design.row(best).transpose();
      Eigen::MatrixXd trial_design = design;
      trial_design.row(row) = proposal_design.row(best);
      InformationState trial = information_from_design(trial_design);
      ++diagnostics.information_recomputes;
      if (!trial.valid ||
          trial.logdet <= information.logdet + objective_epsilon)
        continue;

      points.row(row) = proposal_matrix.row(best);
      codes[row] = proposal_codes[best];
      design.swap(trial_design);
      information = std::move(trial);
      ++diagnostics.accepted_moves;
      ++accepted_since_recompute;
      accepted_in_sweep = true;
      diagnostics.objective_history.push_back(information.logdet);

      if (recompute_every > 0 && accepted_since_recompute >= recompute_every) {
        information = information_from_design(design);
        ++diagnostics.information_recomputes;
        accepted_since_recompute = 0;
        if (!information.valid)
          stop("skpr: coordinate-exchange information recomputation became singular.");
        diagnostics.objective_history.back() = information.logdet;
      }
    }

    if (!accepted_in_sweep) {
      diagnostics.stop_reason = "no_improving_move";
      break;
    }
    const double relative_gain =
        std::expm1((information.logdet - sweep_start) / p);
    if (relative_gain <= tolerance) {
      diagnostics.stop_reason = "tolerance";
      break;
    }
  }
  if (max_iter == 0)
    diagnostics.stop_reason = "max_iter";

  const double maintained_logdet = information.logdet;
  InformationState final_information = information_from_design(design);
  ++diagnostics.information_recomputes;
  if (!final_information.valid)
    stop("skpr: final coordinate-exchange design is numerically singular.");
  const double agreement =
      std::fabs(final_information.logdet - maintained_logdet);
  if (final_information.reciprocal_condition <
      std::sqrt(std::numeric_limits<double>::epsilon())) {
    warning("skpr: final coordinate-exchange design is full rank but ill-conditioned.");
  }
  const double criterion =
      std::exp(final_information.logdet / p) / static_cast<double>(n);

  return List::create(
      _["points"] = points, _["level_codes"] = wrap_codes(codes),
      _["model_matrix"] = design, _["logdet"] = final_information.logdet,
      _["criterion"] = criterion,
      _["diagnostics"] = diagnostics_list(
          diagnostics, final_information, group_products, maintained_logdet,
          agreement));
}
