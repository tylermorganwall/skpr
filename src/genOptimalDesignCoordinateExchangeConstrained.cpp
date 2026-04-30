#include <Rcpp.h>
#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

#include <algorithm>
#include <cmath>
#include <limits>
#include <memory>
#include <vector>

#include "constraint_set.h"
#include "optimalityfunctions.h"

using namespace Rcpp;

namespace {

double logdet_xtx(const Eigen::MatrixXd &X) {
  Eigen::MatrixXd XtX = X.transpose() * X;
  Eigen::LLT<Eigen::MatrixXd> llt(XtX);
  if (llt.info() == Eigen::Success) {
    Eigen::MatrixXd L = llt.matrixL();
    return 2.0 * L.diagonal().array().log().sum();
  }
  Eigen::LDLT<Eigen::MatrixXd> ldlt(XtX);
  if (ldlt.info() != Eigen::Success)
    return R_NegInf;
  Eigen::VectorXd d = ldlt.vectorD();
  return d.array().abs().log().sum();
}

int matrix_rank(const Eigen::MatrixXd &X) {
  if (X.rows() == 0 || X.cols() == 0)
    return 0;
  Eigen::ColPivHouseholderQR<Eigen::MatrixXd> qr(X);
  return qr.rank();
}

bool has_full_column_rank(const Eigen::MatrixXd &X) {
  return X.rows() >= X.cols() && matrix_rank(X) == X.cols();
}

Eigen::MatrixXd information_inverse(const Eigen::MatrixXd &X) {
  if (!has_full_column_rank(X)) {
    Rcpp::stop("skpr: Coordinate exchange attempted to invert a singular "
               "information matrix.");
  }
  Eigen::MatrixXd XtX = X.transpose() * X;
  Eigen::LLT<Eigen::MatrixXd> llt(XtX);
  if (llt.info() != Eigen::Success) {
    Rcpp::stop("skpr: Coordinate exchange information matrix was not "
               "positive definite after full-rank initialization.");
  }
  Eigen::MatrixXd identity =
      Eigen::MatrixXd::Identity(XtX.rows(), XtX.cols());
  Eigen::MatrixXd V = llt.solve(identity);
  if (!V.allFinite()) {
    Rcpp::stop("skpr: Coordinate exchange information matrix inverse "
               "contained non-finite values.");
  }
  return V;
}

Eigen::MatrixXd call_modelmatrix(Function modelmatrix_fn,
                                 const Eigen::MatrixXd &pts) {
  NumericMatrix mm = modelmatrix_fn(Rcpp::wrap(pts));
  Eigen::Map<Eigen::MatrixXd> mapped(REAL(mm), mm.nrow(), mm.ncol());
  return Eigen::MatrixXd(mapped);
}

int match_level_index(double x, const NumericVector &lev, double tol) {
  int best = -1;
  double best_abs = std::numeric_limits<double>::infinity();
  for (int i = 0; i < lev.size(); ++i) {
    double d = std::fabs(x - static_cast<double>(lev[i]));
    if (d < best_abs) {
      best_abs = d;
      best = i;
    }
  }
  if (best < 0)
    return -1;
  if (best_abs <= tol)
    return best;
  return -1;
}

bool enumerate_feasible_candidates(
    const Rcpp::List &factor_levels, const ConstraintSet *constraints,
    int max_grid_size, Eigen::MatrixXd &candidate_points,
    std::vector<std::vector<int>> &candidate_codes) {
  const int q = static_cast<int>(factor_levels.size());
  std::vector<Rcpp::NumericVector> levels(q);
  std::vector<int> Lq(q);
  long double grid_size = 1.0;

  for (int j = 0; j < q; ++j) {
    levels[j] = factor_levels[j];
    Lq[j] = levels[j].size();
    if (Lq[j] <= 0)
      return false;
    grid_size *= static_cast<long double>(Lq[j]);
    if (grid_size > static_cast<long double>(max_grid_size))
      return false;
  }

  const int total = static_cast<int>(grid_size);
  std::vector<int> code(q, 0);
  std::vector<Eigen::RowVectorXd> rows;
  rows.reserve(total);
  candidate_codes.clear();
  candidate_codes.reserve(total);

  for (int iter = 0; iter < total; ++iter) {
    Eigen::RowVectorXd row(q);
    for (int j = 0; j < q; ++j)
      row(j) = levels[j][code[j]];

    if (constraints == NULL || constraints->allowed_row(row, code.data())) {
      rows.push_back(row);
      candidate_codes.push_back(code);
    }

    for (int j = q - 1; j >= 0; --j) {
      ++code[j];
      if (code[j] < Lq[j])
        break;
      code[j] = 0;
    }
  }

  candidate_points.resize(static_cast<int>(rows.size()), q);
  for (int i = 0; i < static_cast<int>(rows.size()); ++i)
    candidate_points.row(i) = rows[i];

  return true;
}

struct GroupCandidates {
  Eigen::MatrixXd points;
  std::vector<std::vector<int>> codes;
};

GroupCandidates enumerate_group_candidates(
    const Eigen::RowVectorXd &base_row, const std::vector<int> &base_codes,
    const std::vector<int> &group, const Rcpp::List &factor_levels,
    const ConstraintSet *constraints, int max_candidates) {
  if (max_candidates < 1) {
    Rcpp::stop("skpr: advancedoptions$coordinate_group_max_candidates must be "
               "at least 1.");
  }

  const int q = static_cast<int>(factor_levels.size());
  std::vector<Rcpp::NumericVector> levels(q);
  std::vector<int> Lg(group.size());
  long double product = 1.0;

  for (int g = 0; g < static_cast<int>(group.size()); ++g) {
    const int j = group[g];
    levels[j] = factor_levels[j];
    Lg[g] = levels[j].size();
    if (Lg[g] <= 0)
      Rcpp::stop("skpr: coordinate group contains a factor with no levels.");
    product *= static_cast<long double>(Lg[g]);
    if (product > static_cast<long double>(max_candidates)) {
      Rcpp::stop("skpr: coordinate group candidate count exceeds "
                 "advancedoptions$coordinate_group_max_candidates.");
    }
  }

  const int total = static_cast<int>(product);
  std::vector<int> combo(group.size(), 0);
  std::vector<Eigen::RowVectorXd> rows;
  std::vector<std::vector<int>> codes;
  rows.reserve(total);
  codes.reserve(total);

  for (int iter = 0; iter < total; ++iter) {
    bool same = true;
    Eigen::RowVectorXd cand = base_row;
    std::vector<int> cand_codes = base_codes;

    for (int g = 0; g < static_cast<int>(group.size()); ++g) {
      const int j = group[g];
      const int code = combo[g];
      cand(j) = levels[j][code];
      cand_codes[j] = code;
      if (code != base_codes[j])
        same = false;
    }

    if (!same &&
        (constraints == NULL || constraints->allowed_row(cand, cand_codes.data()))) {
      rows.push_back(cand);
      codes.push_back(cand_codes);
    }

    for (int g = static_cast<int>(group.size()) - 1; g >= 0; --g) {
      ++combo[g];
      if (combo[g] < Lg[g])
        break;
      combo[g] = 0;
    }
  }

  GroupCandidates out;
  out.points.resize(static_cast<int>(rows.size()), q);
  for (int i = 0; i < static_cast<int>(rows.size()); ++i)
    out.points.row(i) = rows[i];
  out.codes = std::move(codes);
  return out;
}

double d_delta(const Eigen::VectorXd &f_old, const Eigen::VectorXd &f_new,
               const Eigen::MatrixXd &V) {
  const double v_old = f_old.dot(V * f_old);
  const double v_new = f_new.dot(V * f_new);
  const double v_cross = f_new.dot(V * f_old);
  return 1.0 + (v_new - v_old) + (v_cross * v_cross - v_new * v_old);
}

bool sample_feasible_row(Eigen::RowVectorXd &row_values,
                         std::vector<int> &row_codes,
                         const Rcpp::List &factor_levels,
                         const ConstraintSet *constraints, int max_tries) {
  const int q = static_cast<int>(factor_levels.size());
  row_codes.assign(q, 0);
  row_values.resize(q);

  for (int attempt = 0; attempt < max_tries; ++attempt) {
    for (int j = 0; j < q; ++j) {
      NumericVector lev = factor_levels[j];
      const int Lj = lev.size();
      const int code =
          static_cast<int>(std::floor(R::runif(0.0, static_cast<double>(Lj))));
      row_codes[j] = code;
      row_values(j) = lev[code];
    }

    if (constraints == NULL)
      return true;
    if (constraints->allowed_row(row_values, row_codes.data()))
      return true;
  }

  return false;
}

int random_index(int n) {
  if (n <= 1)
    return 0;
  int idx = static_cast<int>(std::floor(R::runif(0.0, static_cast<double>(n))));
  if (idx < 0)
    idx = 0;
  if (idx >= n)
    idx = n - 1;
  return idx;
}

bool greedy_full_rank_initialization(
    Eigen::MatrixXd &points, std::vector<std::vector<int>> &level_pos,
    Eigen::MatrixXd &X, const Rcpp::List &factor_levels,
    Rcpp::Function modelmatrix_fn, const ConstraintSet *constraints,
    int augmentedrows, int max_grid_size) {
  Eigen::MatrixXd candidate_points;
  std::vector<std::vector<int>> candidate_codes;
  const bool enumerated = enumerate_feasible_candidates(
      factor_levels, constraints, max_grid_size, candidate_points, candidate_codes);
  if (!enumerated)
    return false;
  if (candidate_points.rows() == 0) {
    Rcpp::stop("skpr: Coordinate exchange constraints leave no feasible "
               "factor-level combinations.");
  }

  Eigen::MatrixXd candidate_X = call_modelmatrix(modelmatrix_fn, candidate_points);
  if (candidate_X.cols() != X.cols()) {
    Rcpp::stop("skpr: modelmatrix_fn returned wrong number of columns during "
               "coordinate-exchange initialization.");
  }

  const int n = points.rows();
  const int p = X.cols();
  const int n_aug = std::max(0, std::min(augmentedrows, n));
  Eigen::MatrixXd support(n_aug + candidate_X.rows(), p);
  if (n_aug > 0)
    support.topRows(n_aug) = X.topRows(n_aug);
  support.bottomRows(candidate_X.rows()) = candidate_X;
  if (matrix_rank(support) < p) {
    Rcpp::stop("skpr: Coordinate exchange could not construct a full-rank "
               "initial design because the feasible coordinate grid does not "
               "support the specified model.");
  }

  Eigen::MatrixXd selected_X(n, p);
  int selected_count = 0;
  for (int i = 0; i < n_aug; ++i) {
    selected_X.row(selected_count) = X.row(i);
    ++selected_count;
  }
  int current_rank = matrix_rank(selected_X.topRows(selected_count));

  for (int i = n_aug; i < n; ++i) {
    int chosen = -1;
    int chosen_rank = current_rank;

    if (current_rank < p) {
      for (int r = 0; r < candidate_X.rows(); ++r) {
        Eigen::MatrixXd trial(selected_count + 1, p);
        if (selected_count > 0)
          trial.topRows(selected_count) = selected_X.topRows(selected_count);
        trial.row(selected_count) = candidate_X.row(r);
        const int trial_rank = matrix_rank(trial);
        if (trial_rank > chosen_rank) {
          chosen = r;
          chosen_rank = trial_rank;
          break;
        }
      }
      if (chosen < 0)
        return false;
    } else {
      chosen = random_index(candidate_X.rows());
    }

    points.row(i) = candidate_points.row(chosen);
    level_pos[i] = candidate_codes[chosen];
    selected_X.row(selected_count) = candidate_X.row(chosen);
    ++selected_count;
    current_rank = chosen_rank;
  }

  X = call_modelmatrix(modelmatrix_fn, points);
  return has_full_column_rank(X);
}

void ensure_full_rank_feasible_initialization(
    Eigen::MatrixXd &points, std::vector<std::vector<int>> &level_pos,
    Eigen::MatrixXd &X, const Rcpp::List &factor_levels,
    Rcpp::Function modelmatrix_fn, const ConstraintSet *constraints,
    int augmentedrows, int repair_max_tries) {
  const int n = points.rows();
  const int n_aug = std::max(0, std::min(augmentedrows, n));

  bool all_feasible = true;
  if (constraints != NULL) {
    for (int i = 0; i < n; ++i) {
      const bool ok = constraints->allowed_row(points.row(i), level_pos[i].data());
      if (!ok && i < n_aug) {
        Rcpp::stop("skpr: Coordinate exchange augmented row violates the "
                   "provided constraints.");
      }
      all_feasible = all_feasible && ok;
    }
  }

  if (all_feasible && has_full_column_rank(X))
    return;

  if (repair_max_tries <= 0) {
    Rcpp::stop("skpr: Coordinate exchange initial design is singular or "
               "infeasible and no repair attempts are configured.");
  }

  const int max_grid_size = std::max(1000, repair_max_tries * 50);
  if (greedy_full_rank_initialization(points, level_pos, X, factor_levels,
                                      modelmatrix_fn, constraints, n_aug,
                                      max_grid_size)) {
    return;
  }

  for (int attempt = 0; attempt < repair_max_tries; ++attempt) {
    bool sampled_all_rows = true;
    for (int i = n_aug; i < n; ++i) {
      Eigen::RowVectorXd row;
      std::vector<int> codes;
      const bool ok =
          sample_feasible_row(row, codes, factor_levels, constraints,
                              std::max(1, repair_max_tries));
      if (!ok) {
        sampled_all_rows = false;
        break;
      }
      points.row(i) = row;
      level_pos[i] = codes;
    }
    if (!sampled_all_rows)
      continue;

    X = call_modelmatrix(modelmatrix_fn, points);
    if (has_full_column_rank(X))
      return;
  }

  Rcpp::stop("skpr: Coordinate exchange could not find a full-rank feasible "
             "initial design after the configured repair attempts; increase "
             "trials, simplify the model, relax constraints, or increase "
             "advancedoptions$ce_repair_max_tries.");
}

std::vector<int>
select_ce_rows_by_leverage_impl(const Eigen::MatrixXd &X,
                                const Eigen::MatrixXd &V, int kexchange,
                                int augmentedrows,
                                const std::vector<unsigned char> *mustchange) {
  const int n = X.rows();
  const int n_aug = std::max(0, std::min(augmentedrows, n));
  const int available = n - n_aug;
  std::vector<int> selected;
  if (available <= 0)
    return selected;

  if (kexchange < 1)
    Rcpp::stop("skpr: kexchange must be at least 1.");

  const int target = std::min(kexchange, available);

  std::vector<std::pair<double, int>> ranked;
  ranked.reserve(available);

  for (int i = n_aug; i < n; ++i) {
    if (mustchange != NULL && (*mustchange)[i] != 0) {
      selected.push_back(i);
      continue;
    }
    Eigen::VectorXd fi = X.row(i).transpose();
    double leverage = fi.dot(V * fi);
    ranked.emplace_back(leverage, i);
  }

  std::sort(ranked.begin(), ranked.end(),
            [](const std::pair<double, int> &a,
               const std::pair<double, int> &b) {
              if (a.first == b.first)
                return a.second < b.second;
              return a.first < b.first;
            });

  int remaining = target - static_cast<int>(selected.size());
  if (remaining <= 0)
    return selected;

  if (remaining >= static_cast<int>(ranked.size())) {
    for (size_t i = 0; i < ranked.size(); ++i)
      selected.push_back(ranked[i].second);
    return selected;
  }

  const double cutoff = ranked[remaining - 1].first;
  const double tie_tol = 1e-12 * std::max(1.0, std::fabs(cutoff));
  for (size_t i = 0; i < ranked.size(); ++i) {
    if (static_cast<int>(i) < remaining || ranked[i].first <= cutoff + tie_tol) {
      selected.push_back(ranked[i].second);
    } else {
      break;
    }
  }

  return selected;
}

} // namespace

// [[Rcpp::export]]
Rcpp::IntegerVector skpr_ce_select_rows_by_leverage(Eigen::MatrixXd X,
                                                    Eigen::MatrixXd V,
                                                    int kexchange,
                                                    int augmentedrows = 0) {
  std::vector<int> rows =
      select_ce_rows_by_leverage_impl(X, V, kexchange, augmentedrows, NULL);
  Rcpp::IntegerVector out(rows.size());
  for (int i = 0; i < static_cast<int>(rows.size()); ++i)
    out[i] = rows[i] + 1;
  return out;
}

// [[Rcpp::export]]
Rcpp::List genOptimalDesignCoordinateExchangeConstrained(
    Eigen::MatrixXd points, Rcpp::List factor_levels,
    Rcpp::Function modelmatrix_fn, Rcpp::List coordinate_groups,
    Rcpp::List group_columns,
    Rcpp::Nullable<Rcpp::List> constraints_ir = R_NilValue,
    double tolerance = 1e-4,
    Rcpp::IntegerVector kexchange = Rcpp::IntegerVector::create(NA_INTEGER),
    int augmentedrows = 0, int max_iter = 200, int recompute_every = 10,
    int repair_stuck_limit = 5, int repair_max_tries = 2000,
    int coordinate_group_max_candidates = 10000) {
  RNGScope rngScope;

  const int n = points.rows();
  const int q = points.cols();
  const int n_aug = std::max(0, std::min(augmentedrows, n));
  if (factor_levels.size() != q) {
    stop("skpr: factor_levels must have length equal to ncol(points).");
  }
  if (coordinate_groups.size() != group_columns.size()) {
    stop("skpr: coordinate_groups and group_columns must have the same length.");
  }
  if (coordinate_groups.size() <= 0) {
    stop("skpr: coordinate_groups must contain at least one group.");
  }

  std::vector<int> Lq(q);
  for (int j = 0; j < q; ++j) {
    NumericVector lev = factor_levels[j];
    Lq[j] = lev.size();
    if (Lq[j] <= 0)
      stop("skpr: factor_levels contains empty level set.");
  }

  std::vector<std::vector<int>> level_pos(n, std::vector<int>(q, 0));
  const double match_tol = 1e-12;
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < q; ++j) {
      NumericVector lev = factor_levels[j];
      int idx = match_level_index(points(i, j), lev, match_tol);
      if (idx < 0) {
        stop("skpr: initial points contain value not in factor_levels (within "
             "tolerance).");
      }
      level_pos[i][j] = idx;
    }
  }

  std::vector<std::vector<int>> group_list(coordinate_groups.size());
  std::vector<int> factor_seen(q, 0);
  for (int g = 0; g < coordinate_groups.size(); ++g) {
    IntegerVector group = coordinate_groups[g];
    if (group.size() <= 0)
      stop("skpr: coordinate_groups cannot contain empty groups.");
    group_list[g].reserve(group.size());
    for (int k = 0; k < group.size(); ++k) {
      const int j = group[k] - 1;
      if (j < 0 || j >= q)
        stop("skpr: coordinate_groups contains a factor index out of range.");
      if (factor_seen[j] != 0)
        stop("skpr: each factor must appear exactly once across coordinate_groups.");
      factor_seen[j] = 1;
      group_list[g].push_back(j);
    }
    std::sort(group_list[g].begin(), group_list[g].end());
  }
  for (int j = 0; j < q; ++j) {
    if (factor_seen[j] == 0)
      stop("skpr: each factor must appear exactly once across coordinate_groups.");
  }

  bool have_constraints = !constraints_ir.isNull();
  std::unique_ptr<ConstraintSet> constraints;
  std::vector<ConstraintSet::RowCache> caches;
  std::vector<unsigned char> mustchange(n, 0);
  std::vector<int> stuck(n, 0);

  if (have_constraints) {
    constraints.reset(new ConstraintSet(Rcpp::List(constraints_ir)));
    if (constraints->q() != q)
      stop("skpr: constraints_ir$q must match ncol(points).");
  }

  Eigen::MatrixXd X = call_modelmatrix(modelmatrix_fn, points);
  const int p = X.cols();
  if (n < p)
    stop("skpr: Too few runs: nrow(design) < ncol(model_matrix).");

  ensure_full_rank_feasible_initialization(
      points, level_pos, X, factor_levels, modelmatrix_fn,
      have_constraints ? constraints.get() : NULL, augmentedrows,
      repair_max_tries);

  if (have_constraints) {
    caches.resize(n);
    for (int i = 0; i < n; ++i) {
      caches[i] = constraints->make_cache(points.row(i), level_pos[i].data());
      mustchange[i] = (caches[i].satisfied_count <= 0) ? 1 : 0;
    }
  }

  std::vector<std::vector<int>> group_column_list(group_columns.size());
  for (int g = 0; g < group_columns.size(); ++g) {
    IntegerVector cols = group_columns[g];
    group_column_list[g].reserve(cols.size());
    for (int k = 0; k < cols.size(); ++k) {
      const int col0 = cols[k] - 1;
      if (col0 < 0 || col0 >= p) {
        stop("skpr: group_columns contains a model-matrix column index out of "
             "range.");
      }
      group_column_list[g].push_back(col0);
    }
    std::sort(group_column_list[g].begin(), group_column_list[g].end());
    group_column_list[g].erase(
        std::unique(group_column_list[g].begin(), group_column_list[g].end()),
        group_column_list[g].end());
  }

  Eigen::MatrixXd V = information_inverse(X);

  Eigen::MatrixXd identity(2, 2);
  identity.setIdentity(2, 2);
  Eigen::MatrixXd ru_f1(p, 2);
  Eigen::MatrixXd ru_f2(p, 2);
  Eigen::MatrixXd ru_f2vinv(2, p);

  double logdet = logdet_xtx(X);
  double prior_logdet = logdet - 1.0;

  int k_use = NA_INTEGER;
  if (kexchange.size() > 0)
    k_use = kexchange[0];
  if (k_use == NA_INTEGER)
    k_use = n;
  if (k_use < 1)
    stop("skpr: kexchange must be at least 1.");
  if (k_use > n)
    k_use = n;

  const double eps_improve = 1e-12;

  for (int iter = 0; iter < max_iter; ++iter) {
    prior_logdet = logdet;
    bool any_accepted = false;

    std::vector<int> selected_rows = select_ce_rows_by_leverage_impl(
        X, V, k_use, augmentedrows, have_constraints ? &mustchange : NULL);

    for (int sel = 0; sel < static_cast<int>(selected_rows.size()); ++sel) {
      Rcpp::checkUserInterrupt();
      int irow = selected_rows[sel];

      for (int g = 0; g < static_cast<int>(group_list.size()); ++g) {
        Rcpp::checkUserInterrupt();

        const std::vector<int> &group = group_list[g];
        const std::vector<int> &affected_cols = group_column_list[g];
        if (affected_cols.empty())
          continue;

        bool has_alternative = false;
        for (int t = 0; t < static_cast<int>(group.size()); ++t) {
          if (Lq[group[t]] > 1) {
            has_alternative = true;
            break;
          }
        }
        if (!has_alternative)
          continue;

        GroupCandidates candidates = enumerate_group_candidates(
            points.row(irow), level_pos[irow], group, factor_levels,
            have_constraints ? constraints.get() : NULL,
            coordinate_group_max_candidates);
        if (candidates.points.rows() == 0)
          continue;

        Eigen::MatrixXd candX = call_modelmatrix(modelmatrix_fn, candidates.points);
        if (candX.cols() != p)
          stop("skpr: modelmatrix_fn returned wrong number of columns.");

        Eigen::VectorXd f_old = X.row(irow).transpose();
        const bool row_must_fix = (have_constraints && mustchange[irow]);

        double bestDelta = row_must_fix ? -INFINITY : 1.0;
        int bestPos = -1;

        for (int rr = 0; rr < candX.rows(); ++rr) {
          Eigen::VectorXd f_new = candX.row(rr).transpose();
          const double delta = d_delta(f_old, f_new, V);
          if (std::isfinite(delta) && delta > 0.0 && delta > bestDelta) {
            bestDelta = delta;
            bestPos = rr;
          }
        }

        if (bestPos == -1)
          continue;
        if (!row_must_fix && bestDelta <= 1.0 + eps_improve)
          continue;

        Eigen::VectorXd f_new = candX.row(bestPos).transpose();

        rankUpdate(V, f_old, f_new, identity, ru_f1, ru_f2, ru_f2vinv);
        X.row(irow) = f_new.transpose();

        points.row(irow) = candidates.points.row(bestPos);
        level_pos[irow] = candidates.codes[bestPos];

        if (bestDelta > 0 && std::isfinite(bestDelta)) {
          logdet += std::log(bestDelta);
        } else {
          logdet = logdet_xtx(X);
        }

        if (have_constraints) {
          caches[irow] =
              constraints->make_cache(points.row(irow), level_pos[irow].data());
          mustchange[irow] = (caches[irow].satisfied_count <= 0) ? 1 : 0;
          if (!mustchange[irow])
            stuck[irow] = 0;
        }

        any_accepted = true;
      }

      if (have_constraints) {
        if (mustchange[irow]) {
          stuck[irow] += 1;
        } else {
          stuck[irow] = 0;
        }
      }
    }

    if (have_constraints) {
      for (int i = n_aug; i < n; ++i) {
        if (!mustchange[i])
          continue;
        if (stuck[i] < repair_stuck_limit)
          continue;

        Eigen::RowVectorXd new_row;
        std::vector<int> new_codes;
        bool ok = sample_feasible_row(new_row, new_codes, factor_levels,
                                      constraints.get(), repair_max_tries);
        if (!ok) {
          return Rcpp::List::create(
              _["points"] = points, _["model_matrix"] = X, _["logdet"] = logdet,
              _["criterion"] = std::exp(logdet / static_cast<double>(p)) /
                               static_cast<double>(n),
              _["any_infeasible_remaining"] = true);
        }

        Eigen::MatrixXd onePt(1, q);
        onePt.row(0) = new_row;
        Eigen::MatrixXd oneX = call_modelmatrix(modelmatrix_fn, onePt);
        if (oneX.cols() != p)
          stop("skpr: modelmatrix_fn returned wrong number of columns.");

        Eigen::VectorXd f_old = X.row(i).transpose();
        Eigen::VectorXd f_new = oneX.row(0).transpose();
        rankUpdate(V, f_old, f_new, identity, ru_f1, ru_f2, ru_f2vinv);

        X.row(i) = f_new.transpose();
        points.row(i) = new_row;
        level_pos[i] = new_codes;

        caches[i] = constraints->make_cache(points.row(i), level_pos[i].data());
        mustchange[i] = (caches[i].satisfied_count <= 0) ? 1 : 0;
        stuck[i] = 0;
        any_accepted = true;
      }
    }

    if (recompute_every > 0 && ((iter + 1) % recompute_every == 0)) {
      V = information_inverse(X);
      logdet = logdet_xtx(X);
    }

    double rel = 0.0;
    if (std::isfinite(prior_logdet) && std::fabs(prior_logdet) > 0.0) {
      rel = (logdet - prior_logdet) / std::fabs(prior_logdet);
    }

    if (!any_accepted)
      break;

    if (rel < tolerance) {
      if (!(have_constraints &&
            std::any_of(mustchange.begin(), mustchange.end(),
                        [](unsigned char x) { return x != 0; }))) {
        break;
      }
    }
  }

  bool any_infeasible =
      have_constraints && std::any_of(mustchange.begin(), mustchange.end(),
                                      [](unsigned char x) { return x != 0; });

  double deff =
      std::exp(logdet / static_cast<double>(p)) / static_cast<double>(n);

  return Rcpp::List::create(_["points"] = points, _["model_matrix"] = X,
                            _["logdet"] = logdet, _["criterion"] = deff,
                            _["any_infeasible_remaining"] = any_infeasible);
}
