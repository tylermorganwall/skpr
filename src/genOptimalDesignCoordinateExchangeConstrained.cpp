// Constrained coordinate exchange for one fully randomized D-optimal search.
// R/gen_design.R owns normalization, repeat scheduling, and final design
// attributes. This file owns feasible initialization and exchanges within one
// repeat; constraint_set.* evaluates the compiled constraints. R precomputes
// model values in small tables; a callback handles models exceeding table limits.
//
// Data flow:
//   R points + factor levels + coordinate groups + model tables + constraint IR
//     -> validate inputs and map every factor value to a zero-based level code
//     -> evaluate the starting model matrix and repair feasibility/rank if needed
//     -> select mutable rows and visit their coordinate groups in order
//     -> generate one group's feasible alternatives with other factors fixed,
//        then look up affected model values using proposed and fixed level codes
//     -> score row replacements by determinant ratio and immediately commit the
//        best improvement before generating the next group's alternatives
//     -> update a scaled triangular factor, with periodic exact rebuilds
//     -> return points, exact level codes, model matrix, objective, and diagnostics
//        to R for decoding and comparison with other repeats.
//
// Representations: n = runs, q = factors, p = model columns. `points` is n x q
// (numeric search-scale values or discrete codes); `codes` indexes each factor's
// level table; `design` is the n x p model matrix X. The constraint IR has its own
// numeric level values, in original units when supplied by gen_design(), but must
// use the same level-code ordering. `information` holds the triangular factor
// of equilibrated X'X, column scales, and the original-basis log(det(X'X)).
// The first `augmentedrows` runs remain fixed. R validates supported pointwise
// formulas. Native reconstruction checks cached row values; R independently
// verifies tabulated results before comparing repeats.

#include <Rcpp.h>
#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

#include "constraint_set.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <set>
#include <string>
#include <utility>
#include <vector>

using namespace Rcpp;

namespace {

// Search state and traversal workspace ---------------------------------------
// Diagnostics describe one repeat. Objective history records initialization and
// accepted moves; proposal counters also expose pruning and bounded generation.
struct SearchDiagnostics {
  long long proposals_generated = 0;
  long long proposals_scored = 0;
  long long partial_nodes = 0;
  long long partial_pruned = 0;
  int capped_groups = 0;
  int model_matrix_calls = 0;
  int model_table_evaluations = 0;
  double model_table_bytes = 0;
  std::string model_evaluator = "r";
  int information_recomputes = 0;
  int factor_updates = 0;
  int numerical_fallbacks = 0;
  int accepted_moves = 0;
  int iterations = 0;
  int initialization_restarts = 0;
  int initialization_pool_size = 0;
  std::string initialization_mode = "provided";
  std::string stop_reason = "max_iter";
  std::vector<double> objective_history;
};

// A valid state holds a lower triangular factor of the equilibrated information
// matrix. Triangular solves replace inverse products when scoring exchanges.
struct InformationState {
  Eigen::MatrixXd factor;
  Eigen::VectorXd scales;
  double logdet = -std::numeric_limits<double>::infinity();
  double reciprocal_condition = 0.0;
  int rank = 0;
  bool valid = false;
};

// A generated batch keeps values and exact level positions together. `complete`
// describes exhaustion of this traversal (with any requested base exclusion);
// `capped` records truncation, and raw_product ignores constraint pruning.
struct GeneratedRows {
  Eigen::MatrixXd points;
  std::vector<std::vector<int>> codes;
  bool complete = true;
  bool capped = false;
  long long nodes = 0;
  long long pruned = 0;
  double raw_product = 1.0;
};

int random_index(int n);

// Depth-first assignment of the factors in `variables`. Other factors retain
// their base codes. The assigned mask tells ConstraintSet which entries are
// fixed along the current branch and which may still take any allowed level.
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
  bool randomized = false;
  bool stopped = false;
  long long nodes = 0;
  long long pruned = 0;

  void visit(int depth) {
    if (stopped)
      return;
    ++nodes;
    if ((nodes & 1023LL) == 0)
      checkUserInterrupt();
    if (nodes > node_budget || static_cast<int>(rows.size()) >= limit) {
      stopped = true;
      return;
    }
    // Partial checks only reject impossible branches; a leaf still receives a
    // complete constraint check before it becomes a candidate row.
    if (depth == static_cast<int>(variables.size())) {
      if (constraints != NULL && !constraints->allowed_row(codes.data())) {
        ++pruned;
        return;
      }
      if (exclude_base && codes == base_codes)
        return;
      rows.push_back(codes);
      if (static_cast<int>(rows.size()) >= limit)
        stopped = true;
      return;
    }

    const int var = variables[depth];
    const auto &order = level_order[depth];
    const int offset = randomized ? random_index(order.size()) : 0;
    for (int index = 0; index < static_cast<int>(order.size()); ++index) {
      const int code = order[(index + offset) % order.size()];
      codes[var] = code;
      assigned[var] = 1;
      if (constraints == NULL ||
          constraints->can_complete(codes.data(), assigned.data())) {
        visit(depth + 1);
      } else {
        ++pruned;
        // Failed partial branches consume the budget too, even without descent.
        ++nodes;
        if ((nodes & 1023LL) == 0)
          checkUserInterrupt();
        if (nodes >= node_budget)
          stopped = true;
      }
      assigned[var] = 0;
      if (stopped)
        return;
    }
  }
};

// Use R's RNG for traversal shuffles so set.seed() and repeat-level seeds control
// the bounded search. Exact enumeration below does not shuffle its traversal.
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

// Model expansion and numerical summaries -----------------------------------
// Cross the R/C++ boundary once per batch. Callers separately check that the
// number of model columns remains p across starting points and proposal batches.
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

// R precomputes exact model values by term dependency. Native evaluation only
// indexes these immutable tables; interactions use all their current level codes.
// The callback remains available for models exceeding preparation limits and for
// direct internal callers. Both evaluators run through the same search algorithm.
class ModelEvaluator {
  struct Block {
    std::vector<int> factors;
    std::vector<int> columns;
    std::vector<int> strides;
    Eigen::MatrixXd values;
  };
  Function callback;
  bool tabulated = false;
  int p = 0;
  std::vector<Block> blocks;
  std::vector<int> all_blocks;
  std::vector<std::vector<int>> group_blocks;

public:
  ModelEvaluator(Function fn, Nullable<List> tables,
                 const std::vector<std::vector<double>> &levels,
                 const std::vector<std::vector<int>> &groups,
                 SearchDiagnostics &diagnostics) : callback(fn) {
    if (tables.isNull())
      return;
    List plan(tables);
    if (as<int>(plan["version"]) != 1)
      stop("skpr: unsupported model table version.");
    List grids = plan["factor_levels"];
    if (grids.size() != static_cast<int>(levels.size()))
      stop("skpr: model table factor grids do not match factor_levels.");
    for (int j = 0; j < grids.size(); ++j)
      if (as<std::vector<double>>(grids[j]) != levels[j])
        stop("skpr: model table factor grids do not match factor_levels.");
    CharacterVector names = plan["columns"];
    p = names.size();
    if (p <= 0)
      stop("skpr: model tables must contain model columns.");
    std::vector<bool> covered(p, false);
    List source = plan["blocks"];
    group_blocks.resize(groups.size());
    for (int b = 0; b < source.size(); ++b) {
      List item = source[b];
      Block block;
      block.factors = as<std::vector<int>>(item["factors"]);
      block.columns = as<std::vector<int>>(item["columns"]);
      block.values = as<Eigen::MatrixXd>(item["values"]);
      long long count = 1;
      std::set<int> used;
      for (int factor : block.factors) {
        if (factor < 0 || factor >= static_cast<int>(levels.size()) ||
            !used.insert(factor).second)
          stop("skpr: invalid model table factor index.");
        block.strides.push_back(static_cast<int>(count));
        if (count > std::numeric_limits<int>::max() /
                        static_cast<long long>(levels[factor].size()))
          stop("skpr: model table dimensions overflow.");
        count *= levels[factor].size();
      }
      if (block.columns.empty() || block.values.rows() != count ||
          block.values.cols() != static_cast<int>(block.columns.size()))
        stop("skpr: invalid model table dimensions.");
      for (int col : block.columns) {
        if (col < 0 || col >= p || covered[col])
          stop("skpr: model table columns must partition the model matrix.");
        covered[col] = true;
      }
      for (int g = 0; g < static_cast<int>(groups.size()); ++g)
        for (int factor : groups[g])
          if (used.count(factor)) {
            group_blocks[g].push_back(b);
            break;
          }
      diagnostics.model_table_bytes += 8.0 * block.values.size();
      blocks.push_back(std::move(block));
      all_blocks.push_back(b);
    }
    if (std::find(covered.begin(), covered.end(), false) != covered.end())
      stop("skpr: model tables omit model columns.");
    tabulated = true;
    diagnostics.model_evaluator = "tables";
  }

  Eigen::MatrixXd evaluate(const Eigen::MatrixXd &points,
                           const std::vector<std::vector<int>> &codes,
                           SearchDiagnostics &diagnostics,
                           const Eigen::VectorXd *base = NULL, int group = -1) const {
    if (!tabulated)
      return call_model_matrix(callback, points, diagnostics);
    ++diagnostics.model_table_evaluations;
    Eigen::MatrixXd result(codes.size(), p);
    if (base != NULL)
      result.rowwise() = base->transpose();
    const std::vector<int> &active = base == NULL ? all_blocks : group_blocks.at(group);
    for (int b : active) {
      const Block &block = blocks[b];
      for (int row = 0; row < static_cast<int>(codes.size()); ++row) {
        int index = 0;
        for (int j = 0; j < static_cast<int>(block.factors.size()); ++j)
          index += codes[row][block.factors[j]] * block.strides[j];
        for (int col = 0; col < static_cast<int>(block.columns.size()); ++col)
          result(row, block.columns[col]) = block.values(index, col);
      }
    }
    if (!result.allFinite())
      stop("skpr: precomputed model table produced non-finite entries (NA/NaN/Inf).");
    return result;
  }
};

// Column equilibration keeps numerical rank independent of model-column units.
// All rank decisions use singular values of X itself, not squared values in X'X.
Eigen::VectorXd column_scales(const Eigen::MatrixXd &design) {
  Eigen::VectorXd scales = design.cwiseAbs().colwise().maxCoeff().transpose();
  for (int col = 0; col < scales.size(); ++col)
    if (scales(col) == 0.0)
      scales(col) = 1.0;
  return scales;
}

int singular_rank(const Eigen::VectorXd &singular, int rows, int cols) {
  if (singular.size() == 0 || singular(0) <= 0.0)
    return 0;
  const double tolerance = std::max(rows, cols) *
      std::numeric_limits<double>::epsilon() * singular(0);
  return static_cast<int>((singular.array() > tolerance).count());
}

int matrix_rank(const Eigen::MatrixXd &matrix) {
  if (matrix.rows() == 0 || matrix.cols() == 0)
    return 0;
  const Eigen::MatrixXd normalized =
      matrix.array().rowwise() / column_scales(matrix).transpose().array();
  Eigen::JacobiSVD<Eigen::MatrixXd> svd(normalized);
  return singular_rank(svd.singularValues(), matrix.rows(), matrix.cols());
}

// Exact rebuilds compute a triangular factor directly from the equilibrated
// design using QR. SVD supplies rank and conditioning; no explicit inverse is
// needed. The column scaling is included in the returned, original-basis logdet.
InformationState information_from_design(const Eigen::MatrixXd &design) {
  InformationState state;
  const int p = design.cols();
  if (p == 0 || design.rows() < p || !design.allFinite())
    return state;
  state.scales = column_scales(design);
  const Eigen::MatrixXd normalized =
      design.array().rowwise() / state.scales.transpose().array();
  Eigen::JacobiSVD<Eigen::MatrixXd> svd(normalized);
  state.rank = singular_rank(svd.singularValues(), design.rows(), p);
  if (state.rank != p)
    return state;
  Eigen::HouseholderQR<Eigen::MatrixXd> qr(normalized);
  Eigen::MatrixXd upper = qr.matrixQR().topLeftCorner(p, p)
      .triangularView<Eigen::Upper>();
  for (int row = 0; row < p; ++row)
    if (upper(row, row) < 0.0)
      upper.row(row) *= -1.0;
  state.factor = upper.transpose();
  state.logdet = 2.0 * (upper.diagonal().array().log().sum() +
                       state.scales.array().log().sum());
  const double ratio = svd.singularValues()(p - 1) / svd.singularValues()(0);
  state.reciprocal_condition = ratio * ratio;
  state.valid = std::isfinite(state.logdet) && state.factor.allFinite() &&
                (state.factor.diagonal().array() > 0.0).all();
  return state;
}

Eigen::VectorXd whiten_row(const Eigen::VectorXd &row,
                          const InformationState &state) {
  return state.factor.triangularView<Eigen::Lower>().solve(
      row.cwiseQuotient(state.scales));
}

// Update/downdate a lower Cholesky factor in O(p^2). Add the new row before
// removing the old one so a saturated design never passes through a singular
// intermediate matrix. Numerically unsafe downdates fall back to an exact QR.
bool update_factor(Eigen::MatrixXd &factor, Eigen::VectorXd value, int sign) {
  const double guard = 64.0 * std::numeric_limits<double>::epsilon();
  for (int col = 0; col < factor.cols(); ++col) {
    const double diagonal = factor(col, col);
    const double s = value(col) / diagonal;
    const double squared_c = 1.0 + sign * s * s;
    if (!std::isfinite(squared_c) || squared_c <= guard)
      return false;
    const double c = std::sqrt(squared_c);
    factor(col, col) = diagonal * c;
    for (int row = col + 1; row < factor.rows(); ++row) {
      factor(row, col) = (factor(row, col) + sign * s * value(row)) / c;
      value(row) = c * value(row) - s * factor(row, col);
    }
  }
  return factor.allFinite();
}

bool replace_information_row(InformationState &state,
                             const Eigen::VectorXd &old_row,
                             const Eigen::VectorXd &new_row) {
  const Eigen::VectorXd old_scaled = old_row.cwiseQuotient(state.scales);
  const Eigen::VectorXd new_scaled = new_row.cwiseQuotient(state.scales);
  if (!update_factor(state.factor, new_scaled, 1) ||
      !update_factor(state.factor, old_scaled, -1))
    return false;
  state.logdet = 2.0 * (state.factor.diagonal().array().log().sum() +
                       state.scales.array().log().sum());
  return std::isfinite(state.logdet);
}

// The final callback evaluation must agree column by column with the cached
// rows. Relative column scales make this check independent of coefficient units.
bool model_matrices_agree(const Eigen::MatrixXd &left,
                          const Eigen::MatrixXd &right) {
  if (left.rows() != right.rows() || left.cols() != right.cols())
    return false;
  for (int col = 0; col < left.cols(); ++col) {
    const double scale = std::max(left.col(col).cwiseAbs().maxCoeff(),
                                  right.col(col).cwiseAbs().maxCoeff());
    if ((left.col(col) - right.col(col)).cwiseAbs().maxCoeff() > 1e-10 * scale)
      return false;
  }
  return true;
}

// Factor-space validation ----------------------------------------------------
// Initial numeric points are matched to the closest grid entry within a scaled
// tolerance. Subsequent proposals use exact table lookups through their codes.
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

// Canonicalize each search grid into increasing, finite, unique values. Its
// resulting positions must agree with the constraint IR's level-code ordering.
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

// R supplies one-based factor indices; C++ uses zero-based indices. A group is
// a set of coordinates changed jointly, and groups must partition all factors.
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

// Feasible row generation ----------------------------------------------------
// The same generator supplies initialization pools and coordinate alternatives.
// Small grids are enumerated; larger ones use independent randomized descents
// under a shared node budget, retaining distinct feasible rows.
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

  // Exact searches enumerate the grid. Bounded searches repeatedly find a single
  // feasible row, randomizing branch starts on each descent. This spreads the
  // pool across all coordinates instead of taking a rank-deficient DFS prefix.
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
    if (exclude_base && !bounded) {
      const auto position = std::find(order.begin(), order.end(), base_codes[var]);
      if (position != order.end())
        std::rotate(order.begin(), position, position + 1);
    }
    state.level_order.push_back(std::move(order));
  }
  if (bounded) {
    state.randomized = true;
    state.limit = 1;
    result.complete = false;
    std::set<std::vector<int>> seen;
    while (result.nodes < node_budget &&
           static_cast<int>(result.codes.size()) < candidate_limit) {
      checkUserInterrupt();
      state.nodes = state.pruned = 0;
      state.stopped = false;
      state.rows.clear();
      state.node_budget = node_budget - result.nodes;
      state.visit(0);
      result.nodes += state.nodes;
      result.pruned += state.pruned;
      if (state.rows.empty() && !state.stopped) {
        result.complete = true;
        break;
      }
      if (!state.rows.empty() && seen.insert(state.rows.front()).second)
        result.codes.push_back(state.rows.front());
    }
    result.capped = !result.complete;
  } else {
    state.visit(0);
    result.nodes = state.nodes;
    result.pruned = state.pruned;
    result.complete = !state.stopped;
    result.capped = state.stopped;
    result.codes = std::move(state.rows);
    if (static_cast<int>(result.codes.size()) > candidate_limit)
      result.codes.resize(candidate_limit);
  }
  result.points.resize(result.codes.size(), levels.size());
  for (int row = 0; row < static_cast<int>(result.codes.size()); ++row) {
    for (int var = 0; var < static_cast<int>(levels.size()); ++var)
      result.points(row, var) = levels[var][result.codes[row][var]];
  }
  return result;
}

// Mutable-row selection ------------------------------------------------------
// Rank eligible rows by leverage x' (X'X)^-1 x, taking the k lowest values.
// Augmented rows are excluded before counting k; row index breaks equal scores.
template <typename Leverage>
std::vector<int> select_rows(int n, int kexchange, int augmented_rows,
                             Leverage leverage) {
  const int first = std::max(0, std::min(augmented_rows, n));
  if (kexchange < 1)
    stop("skpr: kexchange must be at least 1.");
  const int target = std::min(kexchange, n - first);
  std::vector<std::pair<double, int>> ranked;
  for (int row = first; row < n; ++row) {
    ranked.emplace_back(leverage(row), row);
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

std::vector<int> select_rows_by_leverage(
    const Eigen::MatrixXd &design, const Eigen::MatrixXd &inverse,
    int kexchange, int augmented_rows) {
  return select_rows(design.rows(), kexchange, augmented_rows, [&](int row) {
    const Eigen::VectorXd value = design.row(row).transpose();
    return value.dot(inverse * value);
  });
}

// Feasible, full-rank initialization -----------------------------------------
// A supplied start can be used directly only if every row is feasible and the
// information matrix is numerically invertible.
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

// Preserve fixed rows, then scan a randomized pool ordering for independent
// model rows. Reorthogonalization identifies missing rank directions in O(M p^2)
// without an eigendecomposition for every candidate. Remaining runs are sampled
// with replacement, providing independent starts even for exact feasible pools.
bool initialize_from_pool(
    Eigen::MatrixXd &points, std::vector<std::vector<int>> &codes,
    Eigen::MatrixXd &design, const GeneratedRows &pool,
    const ModelEvaluator &model, int augmented_rows, SearchDiagnostics &diagnostics,
    bool &support_definitively_singular) {
  support_definitively_singular = pool.complete && pool.points.rows() == 0;
  if (pool.points.rows() == 0)
    return false;
  const Eigen::MatrixXd pool_design =
      model.evaluate(pool.points, pool.codes, diagnostics);
  if (pool_design.cols() != design.cols())
    stop("skpr: modelmatrix_fn changed its number of columns.");
  const int n = points.rows();
  const int p = design.cols();
  const int fixed = augmented_rows;
  Eigen::MatrixXd support(fixed + pool_design.rows(), p);
  if (fixed > 0)
    support.topRows(fixed) = design.topRows(fixed);
  support.bottomRows(pool_design.rows()) = pool_design;
  const Eigen::VectorXd scales = column_scales(support);
  support.array().rowwise() /= scales.transpose().array();
  Eigen::JacobiSVD<Eigen::MatrixXd> svd(support);
  if (singular_rank(svd.singularValues(), support.rows(), p) < p) {
    support_definitively_singular = pool.complete;
    return false;
  }
  const double tolerance = std::max(static_cast<int>(support.rows()), p) *
      std::numeric_limits<double>::epsilon() * svd.singularValues()(0);
  Eigen::MatrixXd basis = Eigen::MatrixXd::Zero(p, p);
  int rank = 0;
  auto add_direction = [&](Eigen::VectorXd value) {
    if (rank == p)
      return false;
    for (int pass = 0; pass < 2; ++pass)
      if (rank > 0)
        value -= basis.leftCols(rank) *
                 (basis.leftCols(rank).transpose() * value).eval();
    const double norm = value.norm();
    if (norm <= tolerance)
      return false;
    basis.col(rank++) = value / norm;
    return true;
  };
  for (int row = 0; row < fixed; ++row)
    add_direction(support.row(row).transpose());
  if (n - fixed < p - rank)
    return false;

  std::vector<int> order(pool_design.rows());
  std::iota(order.begin(), order.end(), 0);
  shuffle_vector(order);
  std::vector<int> selected;
  int cursor = 0;
  for (int row = fixed; row < n; ++row) {
    int chosen = -1;
    if (rank < p) {
      while (cursor < static_cast<int>(order.size())) {
        const int candidate = order[cursor++];
        if (add_direction(support.row(fixed + candidate).transpose())) {
          chosen = candidate;
          break;
        }
      }
    } else {
      chosen = random_index(pool_design.rows());
    }
    if (chosen < 0)
      return false;
    selected.push_back(chosen);
  }

  Eigen::MatrixXd selected_design = design;
  for (int row = fixed; row < n; ++row) {
    const int chosen = selected[row - fixed];
    points.row(row) = pool.points.row(chosen);
    codes[row] = pool.codes[chosen];
    selected_design.row(row) = pool_design.row(chosen);
  }
  design = model.evaluate(points, codes, diagnostics);
  if (!model_matrices_agree(design, selected_design))
    stop("skpr: modelmatrix_fn must use a fixed pointwise model; its rows depend on the evaluation batch.");
  return matrix_rank(design) == p;
}

// Reject infeasible fixed rows and reuse valid starts. Otherwise accumulate
// distinct feasible support across restart pools, so rank directions discovered
// in an earlier bounded sample remain available in subsequent attempts.
void ensure_feasible_full_rank_initialization(
    Eigen::MatrixXd &points, std::vector<std::vector<int>> &codes,
    Eigen::MatrixXd &design, const std::vector<std::vector<double>> &levels,
    const ModelEvaluator &model, const ConstraintSet *constraints,
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
      matrix_rank(design) == design.cols()) {
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
                          static_cast<long long>(generation_budget) *
                          std::max(50LL, 2LL * static_cast<long long>(levels.size() + 1)));
  GeneratedRows accumulated;
  accumulated.complete = false;
  std::set<std::vector<int>> seen;
  for (int restart = 0; restart < restart_limit; ++restart) {
    ++diagnostics.initialization_restarts;
    GeneratedRows pool = generate_rows(base_codes, variables, levels, constraints,
                                       pool_limit, node_budget, false);
    diagnostics.partial_nodes += pool.nodes;
    diagnostics.partial_pruned += pool.pruned;
    for (const auto &row : pool.codes)
      if (seen.insert(row).second)
        accumulated.codes.push_back(row);
    accumulated.complete = accumulated.complete || pool.complete;
    accumulated.capped = !accumulated.complete;
    accumulated.points.resize(accumulated.codes.size(), levels.size());
    for (int row = 0; row < static_cast<int>(accumulated.codes.size()); ++row)
      for (int var = 0; var < static_cast<int>(levels.size()); ++var)
        accumulated.points(row, var) = levels[var][accumulated.codes[row][var]];
    diagnostics.initialization_pool_size =
        std::max(diagnostics.initialization_pool_size,
                 static_cast<int>(accumulated.codes.size()));
    bool definitively_singular = false;
    if (initialize_from_pool(points, codes, design, accumulated, model,
                             fixed, diagnostics, definitively_singular)) {
      diagnostics.initialization_mode = accumulated.capped ? "bounded_pool" : "exact_pool";
      return;
    }
    if (definitively_singular) {
      stop("skpr: the complete feasible coordinate grid cannot support a full-rank design for the model.");
    }
  }
  stop("skpr: Coordinate exchange could not construct a full-rank feasible initial design within the configured repair limits.");
}

// Objective scoring and R result conversion ---------------------------------
// For M = X'X, replacing old_row by new_row gives
//   M_new = M - old_row * old_row' + new_row * new_row'.
// The rank-two determinant lemma gives det(M_new) / det(M) below. A ratio above
// one predicts an improvement without factoring M_new for every candidate.
double determinant_delta(double old_leverage,
                         const Eigen::VectorXd &old_white,
                         const Eigen::VectorXd &new_white) {
  const double new_leverage = new_white.squaredNorm();
  const double cross = new_white.dot(old_white);
  return (1.0 - old_leverage) * (1.0 + new_leverage) + cross * cross;
}

// Keep level codes zero-based on return; the R decoder adds one for indexing.
IntegerMatrix wrap_codes(const std::vector<std::vector<int>> &codes) {
  if (codes.empty())
    return IntegerMatrix(0, 0);
  IntegerMatrix result(codes.size(), codes.front().size());
  for (int row = 0; row < result.nrow(); ++row)
    for (int var = 0; var < result.ncol(); ++var)
      result(row, var) = codes[row][var];
  return result;
}

// Flatten one repeat's counters and final numerical state into the schema that
// gen_design() later enriches with repeat status and selection information.
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
      _["model_evaluator"] = diagnostics.model_evaluator,
      _["model_table_evaluations"] = diagnostics.model_table_evaluations,
      _["model_table_bytes"] = diagnostics.model_table_bytes,
      _["information_recomputes"] = diagnostics.information_recomputes,
      _["factor_updates"] = diagnostics.factor_updates,
      _["numerical_fallbacks"] = diagnostics.numerical_fallbacks,
      _["stop_reason"] = diagnostics.stop_reason,
      _["group_raw_products"] = wrap(group_products),
      _["objective_history"] = wrap(diagnostics.objective_history),
      _["maintained_logdet"] = maintained_logdet,
      _["final_logdet"] = final_information.logdet,
      _["logdet_agreement"] = agreement,
      _["final_rank"] = final_information.rank,
      _["conditioning_basis"] = "column_equilibrated",
      _["reciprocal_condition"] = final_information.reciprocal_condition);
}

} // namespace

// R-facing entry points ------------------------------------------------------
// Expose the row-selection policy independently, returning one-based row indices.
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

// Run one search repeat. R is responsible for constructing the initial points
// and invoking this function again when multiple starts are requested.
// [[Rcpp::export]]
List genOptimalDesignCoordinateExchangeConstrained(
    Eigen::MatrixXd points, List factor_levels, Function modelmatrix_fn,
    List coordinate_groups, Nullable<List> constraints_ir = R_NilValue,
    double tolerance = 1e-4,
    Nullable<IntegerVector> kexchange = R_NilValue,
    int augmentedrows = 0, int max_iter = 200, int recompute_every = 10,
    int repair_stuck_limit = 5, int repair_max_tries = 2000,
    int coordinate_group_max_candidates = 10000,
    Nullable<List> model_tables = R_NilValue) {
  // 1. Validate controls, grids, groups, and the optional compiled constraints.
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

  // 2. Establish parallel point/code/model representations, then repair the
  // starting design if necessary before factoring it to score exchanges.
  std::vector<std::vector<int>> codes(n, std::vector<int>(q));
  for (int row = 0; row < n; ++row) {
    for (int var = 0; var < q; ++var) {
      codes[row][var] = match_level(points(row, var), levels[var]);
      if (codes[row][var] < 0)
        stop("skpr: initial point is not present in factor_levels.");
    }
  }

  SearchDiagnostics diagnostics;
  const ModelEvaluator model(modelmatrix_fn, model_tables, levels, groups, diagnostics);
  Eigen::MatrixXd design = model.evaluate(points, codes, diagnostics);
  const int p = design.cols();
  if (p <= 0 || n < p)
    stop("skpr: Too few runs for the coordinate-exchange model matrix.");

  ensure_feasible_full_rank_initialization(
      points, codes, design, levels, model, constraints.get(), fixed,
      repair_stuck_limit, repair_max_tries, diagnostics);

  InformationState information = information_from_design(design);
  ++diagnostics.information_recomputes;
  if (!information.valid)
    stop("skpr: Coordinate exchange initialization is numerically singular.");
  diagnostics.objective_history.push_back(information.logdet);

  // 3. Resolve the mutable-row budget and record each group's unpruned size.
  int k_use = n - fixed;
  if (kexchange.isNotNull()) {
    IntegerVector requested(kexchange);
    if (requested.size() > 0 && requested[0] != NA_INTEGER)
      k_use = requested[0];
  }
  if (k_use < 1)
    stop("skpr: kexchange must be at least 1.");
  k_use = std::min(k_use, n - fixed);

  std::vector<double> group_products(groups.size(), 1.0);
  for (int group_index = 0; group_index < static_cast<int>(groups.size());
       ++group_index) {
    for (int var : groups[group_index])
      group_products[group_index] *= levels[var].size();
  }

  // 4. Sweep the selected rows and their coordinate groups in order. Leverage is
  // recomputed at each sweep boundary; the selected row order remains fixed.
  // Each group uses the current row and information state, including accepted
  // changes from earlier groups in the same row visit.
  int accepted_since_recompute = 0;
  const double objective_epsilon = 1e-12;
  for (int iteration = 0; iteration < max_iter; ++iteration) {
    diagnostics.iterations = iteration + 1;
    const double sweep_start = information.logdet;
    bool accepted_in_sweep = false;
    bool capped_in_sweep = false;
    const std::vector<int> selected =
        select_rows(n, k_use, fixed, [&](int row) {
          return whiten_row(design.row(row).transpose(), information).squaredNorm();
        });

    for (int row : selected) {
      for (const std::vector<int> &group : groups) {
        checkUserInterrupt();
        // Change only this group. All other factor values stay fixed at their
        // latest accepted values, including changes made earlier in this row.
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
        if (generated.capped) {
          ++diagnostics.capped_groups;
          capped_in_sweep = true;
        }
        if (generated.codes.empty())
          continue;

        // Look up this group's affected model columns (or use the R fallback).
        // Recompute scores from the current information state;
        // a prior group's accepted change invalidates both the old row and its score.
        const Eigen::MatrixXd &proposal_matrix = generated.points;
        const Eigen::VectorXd old_row = design.row(row).transpose();
        Eigen::MatrixXd proposal_design =
            model.evaluate(proposal_matrix, generated.codes, diagnostics,
                           &old_row, static_cast<int>(&group - groups.data()));
        if (proposal_design.cols() != p)
          stop("skpr: modelmatrix_fn changed its number of columns.");
        diagnostics.proposals_scored += proposal_design.rows();

        const Eigen::VectorXd old_white = whiten_row(old_row, information);
        const double old_leverage = old_white.squaredNorm();
        double best_delta = 1.0;
        int best = -1;
        for (int candidate = 0; candidate < proposal_design.rows(); ++candidate) {
          const Eigen::VectorXd new_row = proposal_design.row(candidate).transpose();
          const double delta =
              determinant_delta(old_leverage, old_white, whiten_row(new_row, information));
          if (std::isfinite(delta) && delta > best_delta + objective_epsilon) {
            best_delta = delta;
            best = candidate;
          }
        }
        if (best < 0)
          continue;

        // Apply the winning replacement through two O(p^2) factor updates. Check
        // its log gain against the determinant ratio; an unsafe update triggers
        // an exact rebuild before any point or model row is committed.
        const Eigen::VectorXd new_row = proposal_design.row(best).transpose();
        InformationState trial = information;
        bool updated = replace_information_row(trial, old_row, new_row);
        const double predicted_gain = std::log(best_delta);
        if (!updated || std::fabs(trial.logdet - information.logdet - predicted_gain) >
                            1e-8 * std::max(1.0, std::fabs(predicted_gain))) {
          Eigen::MatrixXd trial_design = design;
          trial_design.row(row) = new_row.transpose();
          trial = information_from_design(trial_design);
          ++diagnostics.information_recomputes;
          ++diagnostics.numerical_fallbacks;
          updated = false;
        }
        if (!trial.valid || trial.logdet <= information.logdet + objective_epsilon)
          continue;

        points.row(row) = proposal_matrix.row(best);
        codes[row] = generated.codes[best];
        design.row(row) = new_row.transpose();
        information = std::move(trial);
        if (updated)
          ++diagnostics.factor_updates;
        ++diagnostics.accepted_moves;
        accepted_since_recompute = updated ? accepted_since_recompute + 1 : 0;
        accepted_in_sweep = true;
        diagnostics.objective_history.push_back(information.logdet);

        // Refresh the column scales and triangular factor at the requested
        // interval; frequency zero leaves only fallback and final exact rebuilds.
        if (recompute_every > 0 && accepted_since_recompute >= recompute_every) {
          information = information_from_design(design);
          ++diagnostics.information_recomputes;
          accepted_since_recompute = 0;
          if (!information.valid)
            stop("skpr: coordinate-exchange information recomputation became singular.");
          diagnostics.objective_history.back() = information.logdet;
        }
      }
    }

    // 5. Stop after a sweep with no accepted change, or a sufficiently small
    // relative gain in det(X'X)^(1/p). A capped traversal may leave alternatives
    // unexamined even when this sweep finds no improving move.
    if (!accepted_in_sweep) {
      diagnostics.stop_reason = capped_in_sweep ? "sampled_no_improvement" :
                                                 "no_improving_move";
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

  // 6. Re-evaluate the final points before comparing the cached and exact
  // information states. A batch-dependent callback must not return a design
  // whose model matrix or objective describes different runs.
  Eigen::MatrixXd final_design = model.evaluate(points, codes, diagnostics);
  if (!model_matrices_agree(design, final_design))
    stop("skpr: modelmatrix_fn must use a fixed pointwise model; its rows depend on the evaluation batch.");
  design.swap(final_design);
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
