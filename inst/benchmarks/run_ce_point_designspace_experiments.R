suppressPackageStartupMessages({
  if (requireNamespace("data.table", quietly = TRUE)) {
    library(data.table)
  }
})

load_skpr <- function() {
  loaded = FALSE
  if (requireNamespace("devtools", quietly = TRUE)) {
    loaded = tryCatch({
      devtools::load_all(quiet = TRUE)
      TRUE
    }, error = function(e) {
      message("devtools::load_all() failed, falling back to installed skpr: ", conditionMessage(e))
      FALSE
    })
  }
  if (!loaded) {
    suppressPackageStartupMessages(library(skpr))
  }
}

load_skpr()

dir.create("inst/benchmarks/results", recursive = TRUE, showWarnings = FALSE)
results_dir = "inst/benchmarks/results"
power_scan_path = file.path(results_dir, "designspace_power_scan.csv")
powered_design_path = file.path(results_dir, "designspace_powered_meta_design.csv")
base_scenarios_path = file.path(results_dir, "designspace_base_scenarios.csv")
raw_results_path = file.path(results_dir, "designspace_experiment_raw.csv")
paired_results_path = file.path(results_dir, "designspace_experiment_paired.csv")
summary_results_path = file.path(results_dir, "designspace_experiment_summary_by_scenario.csv")
stats_results_path = file.path(results_dir, "designspace_experiment_stats.txt")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

extract_d_opt = function(design_obj) {
  d_val = suppressWarnings(as.numeric(attr(design_obj, "D")))
  if (length(d_val) == 0 || !is.finite(d_val[[1]])) {
    d_val = tryCatch(
      as.numeric(get_optimality(design_obj, "D")[1, 1]),
      error = function(e) NA_real_
    )
  }
  if (length(d_val) == 0) {
    return(NA_real_)
  }
  d_val[[1]]
}

make_candidates = function(density, candidate_size, nominal_levels) {
  num_grid = if (density == "low") seq(-1, 1, by = 1) else seq(-1, 1, by = 0.1)
  l = as.integer(nominal_levels)

  if (candidate_size == "small") {
    lev3 = LETTERS[seq_len(l)]
    cand = expand.grid(
      x1 = num_grid,
      x2 = num_grid,
      x3 = lev3,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    cand$x3 = factor(cand$x3, levels = lev3)
  } else {
    lev3 = LETTERS[seq_len(l)]
    lev4 = letters[seq_len(l)]
    cand = expand.grid(
      x1 = num_grid,
      x2 = num_grid,
      x3 = lev3,
      x4 = lev4,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    cand$x3 = factor(cand$x3, levels = lev3)
    cand$x4 = factor(cand$x4, levels = lev4)
  }

  cand
}

build_model = function(candidate_size, interactions, quadratic) {
  vars = c("x1", "x2", "x3")
  if (candidate_size == "large") {
    vars = c(vars, "x4")
  }

  terms = vars

  if (interactions == "some") {
    some_terms = c("x1:x2", "x1:x3")
    if (candidate_size == "large") {
      some_terms = c(some_terms, "x2:x4", "x3:x4")
    }
    terms = c(terms, some_terms)
  } else if (interactions == "many") {
    pair_terms = combn(vars, 2, FUN = function(v) paste(v, collapse = ":"))
    terms = c(terms, pair_terms)
  }

  if (quadratic == "some") {
    terms = c(terms, "I(x1^2)")
  } else if (quadratic == "many") {
    terms = c(terms, "I(x1^2)", "I(x2^2)")
  }

  terms = unique(terms)
  as.formula(paste("~", paste(terms, collapse = " + ")))
}

compute_trials = function(trial_size, p_cols, candidate_rows) {
  target = if (trial_size == "small") {
    max(as.integer(p_cols + 2L), as.integer(ceiling(1.25 * p_cols)))
  } else {
    max(as.integer(p_cols + 8L), as.integer(ceiling(1.90 * p_cols)))
  }

  target = min(target, as.integer(candidate_rows - 1L))
  if (!is.finite(target) || target <= p_cols) {
    return(NA_integer_)
  }
  as.integer(target)
}

compute_search_repeats = function(trial_size, candidate_rows) {
  if (candidate_rows <= 300) {
    return(if (trial_size == "small") 18L else 12L)
  }
  if (candidate_rows <= 1500) {
    return(if (trial_size == "small") 12L else 8L)
  }
  if (candidate_rows <= 6000) {
    return(if (trial_size == "small") 8L else 5L)
  }
  if (candidate_rows <= 12000) {
    return(if (trial_size == "small") 6L else 4L)
  }
  if (trial_size == "small") 5L else 3L
}

run_one_method = function(candidateset, model, trials, repeats, method, seed) {
  adv = if (method == "point_exchange") {
    list(search_method = "fedorov")
  } else {
    list(search_method = "coordinate_exchange")
  }

  out = tryCatch(
    {
      set.seed(seed)
      t0 = proc.time()[["elapsed"]]
      des = gen_design(
        candidateset = candidateset,
        model = model,
        trials = trials,
        repeats = repeats,
        optimality = "D",
        progress = FALSE,
        advancedoptions = adv
      )
      t1 = proc.time()[["elapsed"]]
      list(
        ok = TRUE,
        d_opt = extract_d_opt(des),
        runtime_ms = 1000 * (t1 - t0),
        error = NA_character_
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        d_opt = NA_real_,
        runtime_ms = NA_real_,
        error = conditionMessage(e)
      )
    }
  )

  out
}

# ---------------------------------------------------------------------------
# 1) Power-driven meta-design for method-effect detection
# ---------------------------------------------------------------------------
meta_space = expand.grid(
  method = factor(c("point_exchange", "coordinate_exchange")),
  trial_size = factor(c("small", "large"), levels = c("small", "large")),
  candidate_size = factor(c("small", "large"), levels = c("small", "large")),
  interactions = factor(c("none", "some", "many"), levels = c("none", "some", "many")),
  quadratic = factor(c("none", "some", "many"), levels = c("none", "some", "many")),
  nominal_levels = factor(c("3", "4", "5"), levels = c("3", "4", "5")),
  density = factor(c("low", "high"), levels = c("low", "high")),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = TRUE
)

power_model = ~ method + trial_size + candidate_size + interactions + quadratic + nominal_levels + density
alpha_target = 0.05
effectsize_target = 0.5
power_target = 0.95
power_scan_repeats = as.integer(Sys.getenv("SKPR_POWER_SCAN_REPEATS", "120"))
power_final_repeats = as.integer(Sys.getenv("SKPR_POWER_FINAL_REPEATS", "200"))

min_trials_power = ncol(model.matrix(power_model, meta_space)) + 1L
max_trials_power = nrow(meta_space)

evaluate_method_power = function(n_trials) {
  message(
    sprintf(
      "power scan evaluating trials=%d (scan repeats=%d)",
      as.integer(n_trials),
      power_scan_repeats
    )
  )
  one = tryCatch(
    {
      set.seed(81000 + n_trials)
      design_n = gen_design(
        candidateset = meta_space,
        model = power_model,
        trials = as.integer(n_trials),
        repeats = power_scan_repeats,
        optimality = "D",
        progress = FALSE
      )
      pwr = eval_design(
        design = design_n,
        model = power_model,
        alpha = alpha_target,
        effectsize = effectsize_target,
        detailedoutput = FALSE,
        conservative = FALSE
      )
      method_power = pwr$power[
        pwr$parameter == "method" & pwr$type == "effect.power"
      ][[1]]
      list(ok = TRUE, method_power = as.numeric(method_power), error = NA_character_)
    },
    error = function(e) {
      list(ok = FALSE, method_power = NA_real_, error = conditionMessage(e))
    }
  )

  data.frame(
    trials = as.integer(n_trials),
    method_power = one$method_power,
    ok = isTRUE(one$ok),
    error = one$error,
    stringsAsFactors = FALSE
  )
}

coarse_grid = sort(unique(c(seq(min_trials_power, max_trials_power, by = 8L), max_trials_power)))
power_scan_rows = lapply(coarse_grid, evaluate_method_power)
power_scan = do.call(rbind, power_scan_rows)

first_hit_idx = which(power_scan$ok & power_scan$method_power >= power_target)
if (length(first_hit_idx) == 0) {
  stop("No powered meta-design found up to full factorial for requested alpha/effectsize/power.")
}

coarse_hit = power_scan$trials[min(first_hit_idx)]
coarse_prev = max(min_trials_power, coarse_hit - 7L)

refine_grid = seq(coarse_prev, coarse_hit, by = 1L)
refine_rows = lapply(refine_grid, evaluate_method_power)
refine_scan = do.call(rbind, refine_rows)

power_scan = unique(rbind(power_scan, refine_scan))
power_scan = power_scan[order(power_scan$trials), ]
write.csv(power_scan, power_scan_path, row.names = FALSE)

final_hit_idx = which(power_scan$ok & power_scan$method_power >= power_target)
powered_trials = power_scan$trials[min(final_hit_idx)]
powered_method_power = power_scan$method_power[min(final_hit_idx)]

set.seed(91000 + powered_trials)
powered_design = gen_design(
  candidateset = meta_space,
  model = power_model,
  trials = as.integer(powered_trials),
  repeats = power_final_repeats,
  optimality = "D",
  progress = FALSE
)

powered_design_df = as.data.frame(powered_design, stringsAsFactors = TRUE)

message(
  "Powered meta-design found: trials=", powered_trials,
  " method_power=", sprintf("%.4f", powered_method_power),
  " at alpha=", alpha_target,
  " effectsize=", effectsize_target,
  " power_target=", power_target,
  " final_repeats=", power_final_repeats
)

# ---------------------------------------------------------------------------
# 2) Scenario execution with multiple random iterations
# ---------------------------------------------------------------------------
iterations_per_scenario = 2L
base_scenarios = unique(powered_design_df[, setdiff(names(powered_design_df), "method"), drop = FALSE])
base_scenarios = base_scenarios[order(
  base_scenarios$trial_size,
  base_scenarios$candidate_size,
  base_scenarios$interactions,
  base_scenarios$quadratic,
  base_scenarios$nominal_levels,
  base_scenarios$density
), , drop = FALSE]

results = list()
row_counter = 1L

for (i in seq_len(nrow(base_scenarios))) {
  sc = base_scenarios[i, , drop = FALSE]
  density = as.character(sc$density)
  candidate_size = as.character(sc$candidate_size)
  nominal_levels = as.integer(as.character(sc$nominal_levels))
  interactions = as.character(sc$interactions)
  quadratic = as.character(sc$quadratic)
  trial_size = as.character(sc$trial_size)

  cand = make_candidates(
    density = density,
    candidate_size = candidate_size,
    nominal_levels = nominal_levels
  )
  model = build_model(
    candidate_size = candidate_size,
    interactions = interactions,
    quadratic = quadratic
  )

  p_cols = ncol(model.matrix(model, cand))
  candidate_rows = nrow(cand)
  trials = compute_trials(trial_size, p_cols, candidate_rows)

  if (is.na(trials) || trials <= p_cols || trials >= candidate_rows) {
    for (iter in seq_len(iterations_per_scenario)) {
      for (method in c("point_exchange", "coordinate_exchange")) {
        results[[row_counter]] = data.frame(
          scenario_id = i,
          iteration = iter,
          seed = NA_integer_,
          method = method,
          trial_size = trial_size,
          candidate_size = candidate_size,
          interactions = interactions,
          quadratic = quadratic,
          nominal_levels = nominal_levels,
          density = density,
          candidate_rows = candidate_rows,
          p_cols = p_cols,
          trials = trials,
          repeats = NA_integer_,
          runtime_ms = NA_real_,
          d_opt = NA_real_,
          error = "Scenario skipped: invalid trials or saturated model.",
          stringsAsFactors = FALSE
        )
        row_counter = row_counter + 1L
      }
    }
    next
  }

  repeats = compute_search_repeats(trial_size, candidate_rows)

  for (iter in seq_len(iterations_per_scenario)) {
    seed = as.integer(100000 + i * 100 + iter)

    point_out = run_one_method(
      candidateset = cand,
      model = model,
      trials = trials,
      repeats = repeats,
      method = "point_exchange",
      seed = seed
    )
    ce_out = run_one_method(
      candidateset = cand,
      model = model,
      trials = trials,
      repeats = repeats,
      method = "coordinate_exchange",
      seed = seed
    )

    results[[row_counter]] = data.frame(
      scenario_id = i,
      iteration = iter,
      seed = seed,
      method = "point_exchange",
      trial_size = trial_size,
      candidate_size = candidate_size,
      interactions = interactions,
      quadratic = quadratic,
      nominal_levels = nominal_levels,
      density = density,
      candidate_rows = candidate_rows,
      p_cols = p_cols,
      trials = trials,
      repeats = repeats,
      runtime_ms = point_out$runtime_ms,
      d_opt = point_out$d_opt,
      error = point_out$error,
      stringsAsFactors = FALSE
    )
    row_counter = row_counter + 1L

    results[[row_counter]] = data.frame(
      scenario_id = i,
      iteration = iter,
      seed = seed,
      method = "coordinate_exchange",
      trial_size = trial_size,
      candidate_size = candidate_size,
      interactions = interactions,
      quadratic = quadratic,
      nominal_levels = nominal_levels,
      density = density,
      candidate_rows = candidate_rows,
      p_cols = p_cols,
      trials = trials,
      repeats = repeats,
      runtime_ms = ce_out$runtime_ms,
      d_opt = ce_out$d_opt,
      error = ce_out$error,
      stringsAsFactors = FALSE
    )
    row_counter = row_counter + 1L
  }

  message(
    sprintf(
      "completed scenario %d/%d :: trial=%s cand=%s int=%s quad=%s L=%d density=%s",
      i,
      nrow(base_scenarios),
      trial_size,
      candidate_size,
      interactions,
      quadratic,
      nominal_levels,
      density
    )
  )

  if (i %% 8L == 0L || i == nrow(base_scenarios)) {
    raw_checkpoint = do.call(rbind, results)
    write.csv(raw_checkpoint, raw_results_path, row.names = FALSE)
    message(sprintf("checkpoint write: %d rows", nrow(raw_checkpoint)))
  }
}

raw_df = do.call(rbind, results)

point_df = raw_df[raw_df$method == "point_exchange", ]
ce_df = raw_df[raw_df$method == "coordinate_exchange", ]

join_cols = c(
  "scenario_id", "iteration", "seed",
  "trial_size", "candidate_size", "interactions", "quadratic",
  "nominal_levels", "density", "candidate_rows", "p_cols", "trials", "repeats"
)

paired_df = merge(
  point_df,
  ce_df,
  by = join_cols,
  suffixes = c("_point", "_ce"),
  all = TRUE
)

paired_df$d_opt_diff_ce_minus_point = paired_df$d_opt_ce - paired_df$d_opt_point
paired_df$d_opt_ratio_ce_over_point = paired_df$d_opt_ce / paired_df$d_opt_point
paired_df$runtime_ratio_point_over_ce = paired_df$runtime_ms_point / paired_df$runtime_ms_ce
paired_df$both_ok = is.na(paired_df$error_point) & is.na(paired_df$error_ce)

paired_ok = paired_df[paired_df$both_ok & is.finite(paired_df$d_opt_diff_ce_minus_point), ]

tt = NULL
if (nrow(paired_ok) >= 2) {
  tt = t.test(paired_ok$d_opt_ce, paired_ok$d_opt_point, paired = TRUE)
}

lm_fit = NULL
lm_summary = NULL
long_ok = raw_df[is.na(raw_df$error) & is.finite(raw_df$d_opt), ]
if (nrow(long_ok) >= 20) {
  long_ok$method = factor(long_ok$method, levels = c("point_exchange", "coordinate_exchange"))
  lm_fit = lm(
    d_opt ~ method + trial_size + candidate_size + interactions + quadratic + nominal_levels + density,
    data = long_ok
  )
  lm_summary = summary(lm_fit)
}

agg_factor = aggregate(
  d_opt_diff_ce_minus_point ~ trial_size + candidate_size + interactions + quadratic + nominal_levels + density,
  data = paired_ok,
  FUN = function(x) {
    c(
      n = length(x),
      mean = mean(x),
      median = median(x),
      sd = stats::sd(x)
    )
  }
)

agg_factor_expanded = if (nrow(agg_factor) > 0) {
  agg_col = agg_factor$d_opt_diff_ce_minus_point
  agg_stats = if (is.matrix(agg_col)) {
    as.data.frame(agg_col)
  } else if (is.list(agg_col)) {
    as.data.frame(do.call(rbind, agg_col))
  } else {
    as.data.frame(matrix(agg_col, ncol = 1))
  }
  names(agg_stats) = c("n", "mean", "median", "sd")[seq_len(ncol(agg_stats))]
  if (!("n" %in% names(agg_stats))) agg_stats$n = NA_real_
  if (!("mean" %in% names(agg_stats))) agg_stats$mean = NA_real_
  if (!("median" %in% names(agg_stats))) agg_stats$median = NA_real_
  if (!("sd" %in% names(agg_stats))) agg_stats$sd = NA_real_
  agg_stats$n = as.numeric(agg_stats$n)
  agg_stats$mean = as.numeric(agg_stats$mean)
  agg_stats$median = as.numeric(agg_stats$median)
  agg_stats$sd = as.numeric(agg_stats$sd)
  data.frame(
    agg_factor[, c("trial_size", "candidate_size", "interactions", "quadratic", "nominal_levels", "density"), drop = FALSE],
    agg_stats,
    stringsAsFactors = FALSE
  )
} else {
  data.frame()
}

# ---------------------------------------------------------------------------
# 3) Write outputs
# ---------------------------------------------------------------------------
write.csv(power_scan, power_scan_path, row.names = FALSE)
write.csv(powered_design_df, powered_design_path, row.names = FALSE)
write.csv(base_scenarios, base_scenarios_path, row.names = FALSE)
write.csv(raw_df, raw_results_path, row.names = FALSE)
write.csv(paired_df, paired_results_path, row.names = FALSE)
tryCatch(
  write.csv(agg_factor_expanded, summary_results_path, row.names = FALSE),
  error = function(e) {
    warning("Failed writing aggregated summary: ", conditionMessage(e))
  }
)

stats_lines = c(
  sprintf("alpha_target=%.4f", alpha_target),
  sprintf("effectsize_target=%.4f", effectsize_target),
  sprintf("power_target=%.4f", power_target),
  sprintf("powered_trials=%d", powered_trials),
  sprintf("powered_method_power=%.6f", powered_method_power),
  sprintf("base_scenarios=%d", nrow(base_scenarios)),
  sprintf("iterations_per_scenario=%d", iterations_per_scenario),
  sprintf("total_runs=%d", nrow(raw_df)),
  sprintf("paired_runs=%d", nrow(paired_df)),
  sprintf("paired_ok=%d", nrow(paired_ok))
)

if (!is.null(tt)) {
  stats_lines = c(
    stats_lines,
    "",
    "paired_t_test_d_opt_ce_vs_point:",
    sprintf("  t_statistic=%.6f", unname(tt$statistic)),
    sprintf("  df=%.2f", unname(tt$parameter)),
    sprintf("  p_value=%.12f", tt$p.value),
    sprintf("  mean_diff_ce_minus_point=%.6f", unname(tt$estimate)),
    sprintf("  conf_int_low=%.6f", tt$conf.int[[1]]),
    sprintf("  conf_int_high=%.6f", tt$conf.int[[2]])
  )
}

if (!is.null(lm_summary)) {
  coef_tab = lm_summary$coefficients
  method_row = grep("^method", rownames(coef_tab))
  stats_lines = c(stats_lines, "", "lm_d_opt_main_effects:")
  if (length(method_row) > 0) {
    mr = coef_tab[method_row[[1]], ]
    stats_lines = c(
      stats_lines,
      sprintf("  method_coef=%.6f", mr[[1]]),
      sprintf("  method_std_error=%.6f", mr[[2]]),
      sprintf("  method_t=%.6f", mr[[3]]),
      sprintf("  method_p=%.12f", mr[[4]])
    )
  }
  stats_lines = c(
    stats_lines,
    sprintf("  r_squared=%.6f", lm_summary$r.squared),
    sprintf("  adj_r_squared=%.6f", lm_summary$adj.r.squared)
  )
}

writeLines(stats_lines, con = stats_results_path)

message("Wrote designspace experiment outputs to inst/benchmarks/results/")
