suppressPackageStartupMessages({
  library(devtools)
  library(microbenchmark)
})

load_all(quiet = TRUE)

dir.create("inst/benchmarks/results", recursive = TRUE, showWarnings = FALSE)

make_candidates = function(num_levels, fac_levels) {
  cand = expand.grid(
    x1 = seq(-1, 1, length.out = num_levels),
    x2 = seq(-1, 1, length.out = num_levels),
    x3 = letters[seq_len(fac_levels)],
    x4 = LETTERS[seq_len(fac_levels)],
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  cand$x3 = factor(cand$x3, levels = letters[seq_len(fac_levels)])
  cand$x4 = factor(cand$x4, levels = LETTERS[seq_len(fac_levels)])
  cand
}

run_pair_benchmark = function(
  candidateset,
  model,
  trials,
  repeats,
  times = 3L,
  seed = 20260306L,
  ce_constraints = NULL
) {
  ce_adv = list(search_method = "coordinate_exchange")
  if (!is.null(ce_constraints)) {
    ce_adv$constraints = list(filter_expr = ce_constraints)
  }

  mb = microbenchmark(
    point_exchange = {
      set.seed(seed)
      gen_design(
        candidateset = candidateset,
        model = model,
        trials = trials,
        repeats = repeats,
        optimality = "D",
        progress = FALSE,
        advancedoptions = list(search_method = "fedorov")
      )
    },
    coordinate_exchange = {
      set.seed(seed)
      gen_design(
        candidateset = candidateset,
        model = model,
        trials = trials,
        repeats = repeats,
        optimality = "D",
        progress = FALSE,
        advancedoptions = ce_adv
      )
    },
    times = as.integer(times),
    unit = "ms"
  )

  list(
    raw = as.data.frame(mb),
    summary = summary(mb)[, c("expr", "min", "lq", "mean", "median", "uq", "max")]
  )
}

safe_run_pair_benchmark = function(...) {
  tryCatch(
    run_pair_benchmark(...),
    error = function(e) {
      message("benchmark scenario skipped due to error: ", conditionMessage(e))
      NULL
    }
  )
}

add_meta = function(df, meta) {
  for (nm in names(meta)) {
    df[[nm]] = meta[[nm]]
  }
  df
}

collect = function(lst) {
  if (length(lst) == 0) {
    return(data.frame())
  }
  all_cols = unique(unlist(lapply(lst, names), use.names = FALSE))
  normalized = lapply(lst, function(df) {
    missing = setdiff(all_cols, names(df))
    if (length(missing) > 0) {
      for (nm in missing) {
        df[[nm]] = NA
      }
    }
    df[, all_cols, drop = FALSE]
  })
  do.call(rbind, normalized)
}

results_raw = list()
results_summary = list()

append_result = function(section, scenario, raw_df, summary_df, meta = list()) {
  base_meta = c(list(section = section, scenario = scenario), meta)
  results_raw[[length(results_raw) + 1L]] <<- add_meta(raw_df, base_meta)
  results_summary[[length(results_summary) + 1L]] <<- add_meta(summary_df, base_meta)
}

base_model = ~x1 + x2 + x3 + x4 + x1:x2 + x3:x4

# ---------------------------------------------------------------------------
# 1) Prior benchmark matrix (candidate size x trials)
# ---------------------------------------------------------------------------
prior_scenarios = data.frame(
  scenario = c(
    "small_c100_t12",
    "small_c100_t24",
    "medium_c441_t48",
    "large_c1089_t72",
    "xlarge_c10404_t300",
    "xxlarge_c30276_t300"
  ),
  num_levels = c(5L, 5L, 7L, 11L, 34L, 58L),
  fac_levels = c(2L, 2L, 3L, 3L, 3L, 3L),
  trials = c(12L, 24L, 48L, 72L, 300L, 300L),
  repeats = c(30L, 30L, 20L, 20L, 2L, 1L),
  times = c(4L, 4L, 3L, 3L, 1L, 1L),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(prior_scenarios))) {
  s = prior_scenarios[i, ]
  cand = make_candidates(s$num_levels, s$fac_levels)
  out = safe_run_pair_benchmark(
    candidateset = cand,
    model = base_model,
    trials = s$trials,
    repeats = s$repeats,
    times = s$times
  )
  if (is.null(out)) {
    next
  }
  append_result(
    section = "prior",
    scenario = s$scenario,
    raw_df = out$raw,
    summary_df = out$summary,
    meta = list(
      candidate_rows = nrow(cand),
      trials = s$trials,
      repeats = s$repeats,
      benchmark_times = s$times,
      model_label = "base_model",
      constraints_label = "none",
      feasible_rows = nrow(cand),
      complexity_cols = ncol(model.matrix(base_model, cand))
    )
  )
  message("completed prior scenario: ", s$scenario)
}

# ---------------------------------------------------------------------------
# 2) Scaling vs trials
# ---------------------------------------------------------------------------
cand_trials = make_candidates(num_levels = 34L, fac_levels = 3L)
trial_specs = data.frame(
  trials = c(12L, 24L, 36L, 48L, 72L, 96L, 192L, 384L, 1000L, 3000L),
  repeats = c(10L, 10L, 10L, 8L, 8L, 8L, 6L, 4L, 2L, 1L),
  times = c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(trial_specs))) {
  spec = trial_specs[i, ]
  tr = spec$trials
  scenario_name = paste0("trial_", tr)
  out = safe_run_pair_benchmark(
    candidateset = cand_trials,
    model = base_model,
    trials = tr,
    repeats = spec$repeats,
    times = spec$times
  )
  if (is.null(out)) {
    next
  }
  append_result(
    section = "scaling_trials",
    scenario = scenario_name,
    raw_df = out$raw,
    summary_df = out$summary,
    meta = list(
      candidate_rows = nrow(cand_trials),
      trials = tr,
      repeats = spec$repeats,
      benchmark_times = spec$times,
      model_label = "base_model",
      constraints_label = "none",
      feasible_rows = nrow(cand_trials),
      complexity_cols = ncol(model.matrix(base_model, cand_trials))
    )
  )
  message("completed trials scaling scenario: ", scenario_name)
}

# ---------------------------------------------------------------------------
# 3) Scaling vs candidate rows
# ---------------------------------------------------------------------------
candidate_level_grid = c(5L, 7L, 11L, 21L, 34L, 58L)
candidate_trials = 300L

candidate_budget = function(candidate_rows) {
  if (candidate_rows <= 1500L) {
    return(list(repeats = 8L, times = 2L))
  }
  if (candidate_rows <= 5000L) {
    return(list(repeats = 5L, times = 2L))
  }
  if (candidate_rows <= 12000L) {
    return(list(repeats = 3L, times = 1L))
  }
  list(repeats = 2L, times = 1L)
}

for (lv in candidate_level_grid) {
  cand = make_candidates(num_levels = lv, fac_levels = 3L)
  budget = candidate_budget(nrow(cand))
  scenario_name = paste0("cand_rows_", nrow(cand))
  out = safe_run_pair_benchmark(
    candidateset = cand,
    model = base_model,
    trials = candidate_trials,
    repeats = budget$repeats,
    times = budget$times
  )
  if (is.null(out)) {
    next
  }
  append_result(
    section = "scaling_candidate_rows",
    scenario = scenario_name,
    raw_df = out$raw,
    summary_df = out$summary,
    meta = list(
      candidate_rows = nrow(cand),
      trials = candidate_trials,
      repeats = budget$repeats,
      benchmark_times = budget$times,
      model_label = "base_model",
      constraints_label = "none",
      feasible_rows = nrow(cand),
      complexity_cols = ncol(model.matrix(base_model, cand))
    )
  )
  message("completed candidate-row scaling scenario: ", scenario_name)
}

# ---------------------------------------------------------------------------
# 4) Scaling vs constraints (increasingly constrained feasible space)
# ---------------------------------------------------------------------------
cand_constraints_full = make_candidates(num_levels = 58L, fac_levels = 3L)

constraint_specs = list(
  list(name = "c0_none", expr = NULL),
  list(name = "c1_x1x2_le_0.8", expr = quote(x1 + x2 <= 0.8)),
  list(name = "c2_x1x2_le_0.4", expr = quote(x1 + x2 <= 0.4)),
  list(name = "c3_x1x2_le_0.0", expr = quote(x1 + x2 <= 0.0)),
  list(name = "c4_x1x2_le_m0.4", expr = quote(x1 + x2 <= -0.4)),
  list(name = "c5_x1x2_le_m0.8", expr = quote(x1 + x2 <= -0.8))
)

constraint_trials = 300L

for (k in seq_along(constraint_specs)) {
  spec = constraint_specs[[k]]
  feasible = if (is.null(spec$expr)) {
    cand_constraints_full
  } else {
    subset(cand_constraints_full, eval(spec$expr, cand_constraints_full, parent.frame()))
  }
  feasible = droplevels(feasible)
  constraint_repeats = if (nrow(feasible) >= 15000L) 2L else 3L
  constraint_times = 1L

  out = safe_run_pair_benchmark(
    candidateset = feasible,
    model = base_model,
    trials = constraint_trials,
    repeats = constraint_repeats,
    times = constraint_times,
    ce_constraints = spec$expr
  )
  if (is.null(out)) {
    next
  }

  append_result(
    section = "scaling_constraints",
    scenario = spec$name,
    raw_df = out$raw,
    summary_df = out$summary,
    meta = list(
      candidate_rows = nrow(cand_constraints_full),
      trials = constraint_trials,
      repeats = constraint_repeats,
      benchmark_times = constraint_times,
      model_label = "base_model",
      constraints_label = spec$name,
      constraints_level = k - 1L,
      feasible_rows = nrow(feasible),
      feasible_fraction = nrow(feasible) / nrow(cand_constraints_full),
      complexity_cols = ncol(model.matrix(base_model, feasible))
    )
  )
  message("completed constraints scaling scenario: ", spec$name, " feasible rows=", nrow(feasible))
}

# ---------------------------------------------------------------------------
# 5) Scaling vs model complexity
# ---------------------------------------------------------------------------
cand_model = make_candidates(num_levels = 34L, fac_levels = 3L)
model_specs = list(
  list(name = "m1_main", formula = ~x1 + x2 + x3 + x4),
  list(name = "m2_two_way_full", formula = ~(x1 + x2 + x3 + x4)^2),
  list(name = "m3_three_way_full", formula = ~(x1 + x2 + x3 + x4)^3),
  list(name = "m4_four_way_full", formula = ~(x1 + x2 + x3 + x4)^4),
  list(
    name = "m5_rich_poly3",
    formula = ~x1 + x2 + I(x1^2) + I(x2^2) + x3 + x4 + x1:x2 +
      x1:x3 + x1:x4 + x2:x3 + x2:x4 + x3:x4 + I(x1^3) + I(x2^3)
  )
)

model_trials = 120L
model_budget = function(complexity_cols) {
  if (complexity_cols <= 20L) {
    return(list(repeats = 2L, times = 1L))
  }
  list(repeats = 1L, times = 1L)
}

for (m in model_specs) {
  p_cols = ncol(model.matrix(m$formula, cand_model))
  budget = model_budget(p_cols)
  out = safe_run_pair_benchmark(
    candidateset = cand_model,
    model = m$formula,
    trials = model_trials,
    repeats = budget$repeats,
    times = budget$times
  )
  if (is.null(out)) {
    next
  }
  append_result(
    section = "scaling_model_complexity",
    scenario = m$name,
    raw_df = out$raw,
    summary_df = out$summary,
    meta = list(
      candidate_rows = nrow(cand_model),
      trials = model_trials,
      repeats = budget$repeats,
      benchmark_times = budget$times,
      model_label = m$name,
      constraints_label = "none",
      feasible_rows = nrow(cand_model),
      complexity_cols = p_cols
    )
  )
  message("completed model complexity scenario: ", m$name, " p=", p_cols)
}

summary_df = collect(results_summary)
raw_df = collect(results_raw)

summary_df = summary_df[order(
  summary_df$section,
  summary_df$candidate_rows,
  summary_df$trials,
  summary_df$complexity_cols,
  summary_df$expr
), ]

# Pairwise speed ratio table
speed_df = merge(
  summary_df[summary_df$expr == "point_exchange", c(
    "section", "scenario", "median", "candidate_rows", "trials",
    "repeats", "constraints_label", "feasible_rows", "complexity_cols"
  )],
  summary_df[summary_df$expr == "coordinate_exchange", c("section", "scenario", "median")],
  by = c("section", "scenario"),
  suffixes = c("_point", "_ce")
)
speed_df$point_over_ce = speed_df$median_point / speed_df$median_ce

fit_scaling = function(df, section_name, xvar) {
  use = df[df$section == section_name & df$expr %in% c("point_exchange", "coordinate_exchange"), ]
  use = use[is.finite(use[[xvar]]) & use[[xvar]] > 0 & use$median > 0, ]
  if (nrow(use) == 0) {
    return(data.frame())
  }
  out = list()
  for (method in unique(use$expr)) {
    d = use[use$expr == method, ]
    if (nrow(d) < 3) {
      next
    }
    fit = lm(log(median) ~ log(d[[xvar]]), data = d)
    out[[length(out) + 1L]] = data.frame(
      section = section_name,
      xvar = xvar,
      method = method,
      exponent = unname(coef(fit)[2]),
      intercept = unname(coef(fit)[1]),
      r_squared = summary(fit)$r.squared,
      n = nrow(d),
      stringsAsFactors = FALSE
    )
  }
  collect(out)
}

scaling_df = collect(list(
  fit_scaling(summary_df, "scaling_trials", "trials"),
  fit_scaling(summary_df, "scaling_candidate_rows", "candidate_rows"),
  fit_scaling(summary_df, "scaling_constraints", "feasible_rows"),
  fit_scaling(summary_df, "scaling_model_complexity", "complexity_cols")
))

write.csv(summary_df, "inst/benchmarks/results/benchmark_summary.csv", row.names = FALSE)
write.csv(raw_df, "inst/benchmarks/results/benchmark_raw.csv", row.names = FALSE)
write.csv(speed_df, "inst/benchmarks/results/benchmark_speedup.csv", row.names = FALSE)
write.csv(scaling_df, "inst/benchmarks/results/scaling_exponents.csv", row.names = FALSE)

message("Wrote benchmark outputs to inst/benchmarks/results/")
