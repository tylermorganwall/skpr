suppressPackageStartupMessages({
  library(devtools)
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

extract_d_opt = function(design_obj) {
  d_val = suppressWarnings(as.numeric(attr(design_obj, "D")))
  if (length(d_val) == 0 || !is.finite(d_val[1])) {
    d_val = tryCatch(
      as.numeric(get_optimality(design_obj, "D")[1, 1]),
      error = function(e) NA_real_
    )
  }
  if (length(d_val) == 0) {
    return(NA_real_)
  }
  d_val[1]
}

run_pair_quality = function(
  candidateset,
  model,
  trials,
  repeats,
  seed = 20260306L,
  ce_constraints = NULL
) {
  ce_adv = list(search_method = "coordinate_exchange")
  if (!is.null(ce_constraints)) {
    ce_adv$constraints = list(filter_expr = ce_constraints)
  }

  set.seed(seed)
  point_design = gen_design(
    candidateset = candidateset,
    model = model,
    trials = trials,
    repeats = repeats,
    optimality = "D",
    progress = FALSE,
    advancedoptions = list(search_method = "fedorov")
  )

  set.seed(seed)
  ce_design = gen_design(
    candidateset = candidateset,
    model = model,
    trials = trials,
    repeats = repeats,
    optimality = "D",
    progress = FALSE,
    advancedoptions = ce_adv
  )

  data.frame(
    expr = c("point_exchange", "coordinate_exchange"),
    d_opt = c(extract_d_opt(point_design), extract_d_opt(ce_design)),
    stringsAsFactors = FALSE
  )
}

safe_run_pair_quality = function(...) {
  tryCatch(
    run_pair_quality(...),
    error = function(e) {
      message("quality scenario skipped due to error: ", conditionMessage(e))
      NULL
    }
  )
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

add_meta = function(df, meta) {
  for (nm in names(meta)) {
    df[[nm]] = meta[[nm]]
  }
  df
}

quality_rows = list()
append_quality = function(section, scenario, quality_df, meta = list()) {
  base_meta = c(list(section = section, scenario = scenario), meta)
  quality_rows[[length(quality_rows) + 1L]] <<- add_meta(quality_df, base_meta)
}

base_model = ~x1 + x2 + x3 + x4 + x1:x2 + x3:x4

# ---------------------------------------------------------------------------
# 1) Prior quality scenarios
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
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(prior_scenarios))) {
  s = prior_scenarios[i, ]
  cand = make_candidates(s$num_levels, s$fac_levels)
  q = safe_run_pair_quality(
    candidateset = cand,
    model = base_model,
    trials = s$trials,
    repeats = s$repeats
  )
  if (is.null(q)) {
    next
  }
  append_quality(
    section = "prior",
    scenario = s$scenario,
    quality_df = q,
    meta = list(
      candidate_rows = nrow(cand),
      trials = s$trials,
      repeats = s$repeats,
      model_label = "base_model",
      constraints_label = "none",
      feasible_rows = nrow(cand),
      complexity_cols = ncol(model.matrix(base_model, cand))
    )
  )
  message("completed quality scenario: ", s$scenario)
}

# ---------------------------------------------------------------------------
# 2) Trials scaling quality
# ---------------------------------------------------------------------------
cand_trials = make_candidates(num_levels = 34L, fac_levels = 3L)
trial_specs = data.frame(
  trials = c(12L, 24L, 36L, 48L, 72L, 96L, 192L, 384L, 1000L, 3000L),
  repeats = c(10L, 10L, 10L, 8L, 8L, 8L, 6L, 4L, 2L, 1L),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(trial_specs))) {
  spec = trial_specs[i, ]
  scenario_name = paste0("trial_", spec$trials)
  q = safe_run_pair_quality(
    candidateset = cand_trials,
    model = base_model,
    trials = spec$trials,
    repeats = spec$repeats
  )
  if (is.null(q)) {
    next
  }
  append_quality(
    section = "scaling_trials",
    scenario = scenario_name,
    quality_df = q,
    meta = list(
      candidate_rows = nrow(cand_trials),
      trials = spec$trials,
      repeats = spec$repeats,
      model_label = "base_model",
      constraints_label = "none",
      feasible_rows = nrow(cand_trials),
      complexity_cols = ncol(model.matrix(base_model, cand_trials))
    )
  )
  message("completed quality scenario: ", scenario_name)
}

# ---------------------------------------------------------------------------
# 3) Candidate-row scaling quality
# ---------------------------------------------------------------------------
candidate_level_grid = c(5L, 7L, 11L, 21L, 34L, 58L)
candidate_trials = 300L

candidate_budget = function(candidate_rows) {
  if (candidate_rows <= 1500L) {
    return(8L)
  }
  if (candidate_rows <= 5000L) {
    return(5L)
  }
  if (candidate_rows <= 12000L) {
    return(3L)
  }
  2L
}

for (lv in candidate_level_grid) {
  cand = make_candidates(num_levels = lv, fac_levels = 3L)
  repeats = candidate_budget(nrow(cand))
  scenario_name = paste0("cand_rows_", nrow(cand))
  q = safe_run_pair_quality(
    candidateset = cand,
    model = base_model,
    trials = candidate_trials,
    repeats = repeats
  )
  if (is.null(q)) {
    next
  }
  append_quality(
    section = "scaling_candidate_rows",
    scenario = scenario_name,
    quality_df = q,
    meta = list(
      candidate_rows = nrow(cand),
      trials = candidate_trials,
      repeats = repeats,
      model_label = "base_model",
      constraints_label = "none",
      feasible_rows = nrow(cand),
      complexity_cols = ncol(model.matrix(base_model, cand))
    )
  )
  message("completed quality scenario: ", scenario_name)
}

# ---------------------------------------------------------------------------
# 4) Constraints scaling quality
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
  repeats = if (nrow(feasible) >= 15000L) 2L else 3L

  q = safe_run_pair_quality(
    candidateset = feasible,
    model = base_model,
    trials = constraint_trials,
    repeats = repeats,
    ce_constraints = spec$expr
  )
  if (is.null(q)) {
    next
  }
  append_quality(
    section = "scaling_constraints",
    scenario = spec$name,
    quality_df = q,
    meta = list(
      candidate_rows = nrow(cand_constraints_full),
      trials = constraint_trials,
      repeats = repeats,
      model_label = "base_model",
      constraints_label = spec$name,
      constraints_level = k - 1L,
      feasible_rows = nrow(feasible),
      feasible_fraction = nrow(feasible) / nrow(cand_constraints_full),
      complexity_cols = ncol(model.matrix(base_model, feasible))
    )
  )
  message("completed quality scenario: ", spec$name)
}

# ---------------------------------------------------------------------------
# 5) Model complexity quality
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
    return(2L)
  }
  1L
}

for (m in model_specs) {
  p_cols = ncol(model.matrix(m$formula, cand_model))
  repeats = model_budget(p_cols)
  q = safe_run_pair_quality(
    candidateset = cand_model,
    model = m$formula,
    trials = model_trials,
    repeats = repeats
  )
  if (is.null(q)) {
    next
  }
  append_quality(
    section = "scaling_model_complexity",
    scenario = m$name,
    quality_df = q,
    meta = list(
      candidate_rows = nrow(cand_model),
      trials = model_trials,
      repeats = repeats,
      model_label = m$name,
      constraints_label = "none",
      feasible_rows = nrow(cand_model),
      complexity_cols = p_cols
    )
  )
  message("completed quality scenario: ", m$name, " p=", p_cols)
}

quality_df = collect(quality_rows)
quality_df = quality_df[order(
  quality_df$section,
  quality_df$candidate_rows,
  quality_df$trials,
  quality_df$complexity_cols,
  quality_df$expr
), ]

quality_cmp = merge(
  quality_df[quality_df$expr == "point_exchange", c(
    "section", "scenario", "d_opt", "candidate_rows", "trials", "repeats",
    "constraints_label", "feasible_rows", "complexity_cols"
  )],
  quality_df[quality_df$expr == "coordinate_exchange", c("section", "scenario", "d_opt")],
  by = c("section", "scenario"),
  suffixes = c("_point", "_ce")
)
quality_cmp$d_opt_diff_ce_minus_point = quality_cmp$d_opt_ce - quality_cmp$d_opt_point
quality_cmp$d_opt_ratio_ce_over_point = quality_cmp$d_opt_ce / quality_cmp$d_opt_point

write.csv(quality_df, "inst/benchmarks/results/benchmark_quality.csv", row.names = FALSE)
write.csv(quality_cmp, "inst/benchmarks/results/benchmark_quality_compare.csv", row.names = FALSE)

message("Wrote quality outputs to inst/benchmarks/results/")
