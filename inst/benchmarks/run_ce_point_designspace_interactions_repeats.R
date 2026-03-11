suppressPackageStartupMessages({
  if (requireNamespace("data.table", quietly = TRUE)) {
    library(data.table)
  }
})

load_skpr <- function() {
  loaded <- FALSE
  if (requireNamespace("devtools", quietly = TRUE)) {
    loaded <- tryCatch(
      {
        devtools::load_all(quiet = TRUE)
        TRUE
      },
      error = function(e) {
        message("devtools::load_all() failed, falling back to installed skpr: ", conditionMessage(e))
        FALSE
      }
    )
  }
  if (!loaded) {
    suppressPackageStartupMessages(library(skpr))
  }
}

load_skpr()

results_dir <- "inst/benchmarks/results"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

power_scan_path <- file.path(results_dir, "designspace_interactions_repeats_power_scan.csv")
powered_design_path <- file.path(results_dir, "designspace_interactions_repeats_powered_meta_design.csv")
base_scenarios_path <- file.path(results_dir, "designspace_interactions_repeats_base_scenarios.csv")
raw_results_path <- file.path(results_dir, "designspace_interactions_repeats_raw.csv")
paired_results_path <- file.path(results_dir, "designspace_interactions_repeats_paired.csv")
summary_results_path <- file.path(results_dir, "designspace_interactions_repeats_summary_by_scenario.csv")
lm_global_coef_path <- file.path(results_dir, "designspace_interactions_repeats_lm_global_coefficients.csv")
lm_quality_coef_path <- file.path(results_dir, "designspace_interactions_repeats_lm_quality_coefficients.csv")
lm_quality_anova_path <- file.path(results_dir, "designspace_interactions_repeats_lm_quality_anova.csv")
lm_runtime_coef_path <- file.path(results_dir, "designspace_interactions_repeats_lm_runtime_coefficients.csv")
lm_runtime_anova_path <- file.path(results_dir, "designspace_interactions_repeats_lm_runtime_anova.csv")
stats_results_path <- file.path(results_dir, "designspace_interactions_repeats_stats.txt")

extract_d_opt <- function(design_obj) {
  d_val <- suppressWarnings(as.numeric(attr(design_obj, "D")))
  if (length(d_val) == 0 || !is.finite(d_val[[1]])) {
    d_val <- tryCatch(
      as.numeric(get_optimality(design_obj, "D")[1, 1]),
      error = function(e) NA_real_
    )
  }
  if (length(d_val) == 0) {
    return(NA_real_)
  }
  d_val[[1]]
}

make_candidates <- function(density, candidate_size, nominal_levels) {
  num_grid <- if (density == "low") seq(-1, 1, by = 1) else seq(-1, 1, by = 0.1)
  l <- as.integer(nominal_levels)

  if (candidate_size == "small") {
    lev3 <- LETTERS[seq_len(l)]
    cand <- expand.grid(
      x1 = num_grid,
      x2 = num_grid,
      x3 = lev3,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    cand$x3 <- factor(cand$x3, levels = lev3)
  } else {
    lev3 <- LETTERS[seq_len(l)]
    lev4 <- letters[seq_len(l)]
    cand <- expand.grid(
      x1 = num_grid,
      x2 = num_grid,
      x3 = lev3,
      x4 = lev4,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    cand$x3 <- factor(cand$x3, levels = lev3)
    cand$x4 <- factor(cand$x4, levels = lev4)
  }

  cand
}

build_model <- function(candidate_size, interactions, quadratic) {
  vars <- c("x1", "x2", "x3")
  if (candidate_size == "large") {
    vars <- c(vars, "x4")
  }

  terms <- vars

  if (interactions == "some") {
    some_terms <- c("x1:x2", "x1:x3")
    if (candidate_size == "large") {
      some_terms <- c(some_terms, "x2:x4", "x3:x4")
    }
    terms <- c(terms, some_terms)
  } else if (interactions == "many") {
    pair_terms <- combn(vars, 2, FUN = function(v) paste(v, collapse = ":"))
    terms <- c(terms, pair_terms)
  }

  if (quadratic == "some") {
    terms <- c(terms, "I(x1^2)")
  } else if (quadratic == "many") {
    terms <- c(terms, "I(x1^2)", "I(x2^2)")
  }

  terms <- unique(terms)
  as.formula(paste("~", paste(terms, collapse = " + ")))
}

compute_trials <- function(trial_size, p_cols, candidate_rows) {
  target <- if (trial_size == "small") {
    max(as.integer(p_cols + 2L), as.integer(ceiling(1.25 * p_cols)))
  } else {
    max(as.integer(p_cols + 8L), as.integer(ceiling(1.90 * p_cols)))
  }

  target <- min(target, as.integer(candidate_rows - 1L))
  if (!is.finite(target) || target <= p_cols) {
    return(NA_integer_)
  }
  as.integer(target)
}

compute_search_repeats <- function(trial_size, candidate_rows) {
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

run_one_method <- function(candidateset, model, trials, repeats, method, seed) {
  adv <- if (method == "point_exchange") {
    list(search_method = "fedorov")
  } else {
    list(search_method = "coordinate_exchange")
  }

  tryCatch(
    {
      set.seed(seed)
      t0 <- proc.time()[["elapsed"]]
      des <- gen_design(
        candidateset = candidateset,
        model = model,
        trials = trials,
        repeats = repeats,
        optimality = "D",
        progress = FALSE,
        advancedoptions = adv
      )
      t1 <- proc.time()[["elapsed"]]
      list(
        ok = TRUE,
        d_opt = extract_d_opt(des),
        runtime_ms = 1000 * (t1 - t0),
        error = NA_character_
      )
    },
    error = function(e) {
      list(ok = FALSE, d_opt = NA_real_, runtime_ms = NA_real_, error = conditionMessage(e))
    }
  )
}

extract_effect_power <- function(power_df, patterns) {
  if (nrow(power_df) == 0) {
    return(NA_real_)
  }
  idx <- rep(FALSE, nrow(power_df))
  for (pat in patterns) {
    idx <- idx | grepl(pat, power_df$parameter)
  }
  idx <- idx & power_df$type == "effect.power"
  vals <- suppressWarnings(as.numeric(power_df$power[idx]))
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) {
    return(NA_real_)
  }
  max(vals)
}

# ---------------------------------------------------------------------------
# 1) Power-driven meta-design with pairwise interactions
# ---------------------------------------------------------------------------
meta_space <- expand.grid(
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

power_model <- ~ (method + trial_size + candidate_size + interactions + quadratic + nominal_levels + density)^2
alpha_target <- 0.05
effectsize_target <- 0.5
power_target <- 0.95
power_scan_repeats <- as.integer(Sys.getenv("SKPR_POWER_SCAN_REPEATS", "100"))
power_final_repeats <- as.integer(Sys.getenv("SKPR_POWER_FINAL_REPEATS", "180"))

min_trials_power <- ncol(model.matrix(power_model, meta_space)) + 1L
max_trials_power <- nrow(meta_space)

evaluate_power <- function(n_trials) {
  message(sprintf("power scan trials=%d (repeats=%d)", as.integer(n_trials), power_scan_repeats))
  out <- tryCatch(
    {
      set.seed(81000 + n_trials)
      design_n <- gen_design(
        candidateset = meta_space,
        model = power_model,
        trials = as.integer(n_trials),
        repeats = power_scan_repeats,
        optimality = "D",
        progress = FALSE
      )
      pwr <- eval_design(
        design = design_n,
        model = power_model,
        alpha = alpha_target,
        effectsize = effectsize_target,
        detailedoutput = FALSE,
        conservative = FALSE
      )
      method_power <- extract_effect_power(pwr, c("^method$"))
      min_target_power <- method_power
      list(
        ok = TRUE,
        method_power = method_power,
        min_target_power = min_target_power,
        error = NA_character_
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        method_power = NA_real_,
        min_target_power = NA_real_,
        error = conditionMessage(e)
      )
    }
  )

  data.frame(
    trials = as.integer(n_trials),
    method_power = out$method_power,
    min_target_power = out$min_target_power,
    ok = isTRUE(out$ok),
    error = out$error,
    stringsAsFactors = FALSE
  )
}

eval_cache <- new.env(parent = emptyenv())
evaluate_power_cached <- function(n_trials) {
  key <- as.character(as.integer(n_trials))
  if (exists(key, envir = eval_cache, inherits = FALSE)) {
    return(get(key, envir = eval_cache, inherits = FALSE))
  }
  val <- evaluate_power(as.integer(n_trials))
  assign(key, val, envir = eval_cache)
  val
}

is_power_hit <- function(df_row) {
  isTRUE(df_row$ok[[1]]) && is.finite(df_row$method_power[[1]]) && df_row$method_power[[1]] >= power_target
}

low_trials <- min_trials_power
low_eval <- evaluate_power_cached(low_trials)

if (is_power_hit(low_eval)) {
  powered_trials <- low_trials
} else {
  high_trials <- low_trials
  high_eval <- low_eval

  repeat {
    if (high_trials >= max_trials_power) {
      stop("No powered meta-design found for the method effect at the requested power target.")
    }
    next_trials <- min(max_trials_power, as.integer(high_trials * 2L))
    if (next_trials == high_trials) {
      stop("No powered meta-design found for the method effect at the requested power target.")
    }
    high_trials <- next_trials
    high_eval <- evaluate_power_cached(high_trials)
    if (is_power_hit(high_eval)) {
      break
    }
  }

  lo <- low_trials
  hi <- high_trials
  while ((hi - lo) > 1L) {
    mid <- as.integer(floor((lo + hi) / 2L))
    mid_eval <- evaluate_power_cached(mid)
    if (is_power_hit(mid_eval)) {
      hi <- mid
    } else {
      lo <- mid
    }
  }
  powered_trials <- hi
}

power_scan <- do.call(
  rbind,
  lapply(ls(eval_cache), function(k) get(k, envir = eval_cache, inherits = FALSE))
)
power_scan <- unique(power_scan)
power_scan <- power_scan[order(power_scan$trials), ]
write.csv(power_scan, power_scan_path, row.names = FALSE)

row_hit <- power_scan[power_scan$trials == powered_trials, ][1, ]

set.seed(91000 + powered_trials)
powered_design <- gen_design(
  candidateset = meta_space,
  model = power_model,
  trials = as.integer(powered_trials),
  repeats = power_final_repeats,
  optimality = "D",
  progress = FALSE
)
powered_design_df <- as.data.frame(powered_design, stringsAsFactors = TRUE)

message(
  "Powered meta-design found: trials=", powered_trials,
  " method_power=", sprintf("%.4f", row_hit$method_power)
)

# ---------------------------------------------------------------------------
# 2) Scenario execution with fixed repeat-scale comparison (20 vs 60)
# ---------------------------------------------------------------------------
iterations_per_scenario <- as.integer(Sys.getenv("SKPR_ITER_PER_SCENARIO", "2"))
base_core <- unique(powered_design_df[, setdiff(names(powered_design_df), "method"), drop = FALSE])
repeat_levels <- data.frame(
  repeat_scale = factor(c("r20", "r60"), levels = c("r20", "r60")),
  stringsAsFactors = TRUE
)
base_scenarios <- merge(base_core, repeat_levels, by = NULL)
base_scenarios <- base_scenarios[order(
  base_scenarios$repeat_scale,
  base_scenarios$trial_size,
  base_scenarios$candidate_size,
  base_scenarios$interactions,
  base_scenarios$quadratic,
  base_scenarios$nominal_levels,
  base_scenarios$density
), , drop = FALSE]

build_result_key <- function(
    repeat_scale, trial_size, candidate_size, interactions, quadratic, nominal_levels,
    density, candidate_rows, p_cols, trials, repeats_base, repeats, iteration, method
) {
  paste(
    repeat_scale, trial_size, candidate_size, interactions, quadratic, nominal_levels,
    density, candidate_rows, p_cols, trials, repeats_base, repeats, iteration, method,
    sep = "|"
  )
}

build_result_key_df <- function(df) {
  paste(
    as.character(df$repeat_scale),
    as.character(df$trial_size),
    as.character(df$candidate_size),
    as.character(df$interactions),
    as.character(df$quadratic),
    as.character(df$nominal_levels),
    as.character(df$density),
    as.character(df$candidate_rows),
    as.character(df$p_cols),
    as.character(df$trials),
    as.character(df$repeats_base),
    as.character(df$repeats),
    as.character(df$iteration),
    as.character(df$method),
    sep = "|"
  )
}

resume_enabled <- tolower(Sys.getenv("SKPR_RESUME", "true")) %in% c("1", "true", "yes")
existing_raw <- data.frame()
existing_keys <- character(0)
if (resume_enabled && file.exists(raw_results_path)) {
  existing_raw <- tryCatch(
    read.csv(raw_results_path, stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )
  if (nrow(existing_raw) > 0) {
    existing_keys <- build_result_key_df(existing_raw)
    message(sprintf("resume enabled: loaded %d existing rows", nrow(existing_raw)))
  }
}

results <- list()
row_counter <- 1L

for (i in seq_len(nrow(base_scenarios))) {
  sc <- base_scenarios[i, , drop = FALSE]
  repeat_scale <- as.character(sc$repeat_scale)
  density <- as.character(sc$density)
  candidate_size <- as.character(sc$candidate_size)
  nominal_levels <- as.integer(as.character(sc$nominal_levels))
  interactions <- as.character(sc$interactions)
  quadratic <- as.character(sc$quadratic)
  trial_size <- as.character(sc$trial_size)

  cand <- make_candidates(density = density, candidate_size = candidate_size, nominal_levels = nominal_levels)
  model <- build_model(candidate_size = candidate_size, interactions = interactions, quadratic = quadratic)

  p_cols <- ncol(model.matrix(model, cand))
  candidate_rows <- nrow(cand)
  trials <- compute_trials(trial_size, p_cols, candidate_rows)
  repeats_base <- 20L
  repeats <- if (repeat_scale == "r60") 60L else 20L

  if (is.na(trials) || trials <= p_cols || trials >= candidate_rows) {
    for (iter in seq_len(iterations_per_scenario)) {
      for (method in c("point_exchange", "coordinate_exchange")) {
        key <- build_result_key(
          repeat_scale, trial_size, candidate_size, interactions, quadratic, nominal_levels,
          density, candidate_rows, p_cols, trials, repeats_base, repeats, iter, method
        )
        if (key %in% existing_keys) {
          next
        }
        results[[row_counter]] <- data.frame(
          scenario_id = i,
          iteration = iter,
          seed = NA_integer_,
          method = method,
          repeat_scale = repeat_scale,
          trial_size = trial_size,
          candidate_size = candidate_size,
          interactions = interactions,
          quadratic = quadratic,
          nominal_levels = nominal_levels,
          density = density,
          candidate_rows = candidate_rows,
          p_cols = p_cols,
          trials = trials,
          repeats_base = repeats_base,
          repeats = repeats,
          runtime_ms = NA_real_,
          d_opt = NA_real_,
          error = "Scenario skipped: invalid trials or saturated model.",
          stringsAsFactors = FALSE
        )
        existing_keys <- c(existing_keys, key)
        row_counter <- row_counter + 1L
      }
    }
    next
  }

  for (iter in seq_len(iterations_per_scenario)) {
    seed <- as.integer(200000 + i * 100 + iter)

    point_key <- build_result_key(
      repeat_scale, trial_size, candidate_size, interactions, quadratic, nominal_levels,
      density, candidate_rows, p_cols, trials, repeats_base, repeats, iter, "point_exchange"
    )
    ce_key <- build_result_key(
      repeat_scale, trial_size, candidate_size, interactions, quadratic, nominal_levels,
      density, candidate_rows, p_cols, trials, repeats_base, repeats, iter, "coordinate_exchange"
    )

    point_missing <- !(point_key %in% existing_keys)
    ce_missing <- !(ce_key %in% existing_keys)

    if (!point_missing && !ce_missing) {
      next
    }

    if (point_missing) {
      point_out <- run_one_method(
        candidateset = cand,
        model = model,
        trials = trials,
        repeats = repeats,
        method = "point_exchange",
        seed = seed
      )
      results[[row_counter]] <- data.frame(
        scenario_id = i,
        iteration = iter,
        seed = seed,
        method = "point_exchange",
        repeat_scale = repeat_scale,
        trial_size = trial_size,
        candidate_size = candidate_size,
        interactions = interactions,
        quadratic = quadratic,
        nominal_levels = nominal_levels,
        density = density,
        candidate_rows = candidate_rows,
        p_cols = p_cols,
        trials = trials,
        repeats_base = repeats_base,
        repeats = repeats,
        runtime_ms = point_out$runtime_ms,
        d_opt = point_out$d_opt,
        error = point_out$error,
        stringsAsFactors = FALSE
      )
      existing_keys <- c(existing_keys, point_key)
      row_counter <- row_counter + 1L
    }

    if (ce_missing) {
      ce_out <- run_one_method(
        candidateset = cand,
        model = model,
        trials = trials,
        repeats = repeats,
        method = "coordinate_exchange",
        seed = seed
      )
      results[[row_counter]] <- data.frame(
        scenario_id = i,
        iteration = iter,
        seed = seed,
        method = "coordinate_exchange",
        repeat_scale = repeat_scale,
        trial_size = trial_size,
        candidate_size = candidate_size,
        interactions = interactions,
        quadratic = quadratic,
        nominal_levels = nominal_levels,
        density = density,
        candidate_rows = candidate_rows,
        p_cols = p_cols,
        trials = trials,
        repeats_base = repeats_base,
        repeats = repeats,
        runtime_ms = ce_out$runtime_ms,
        d_opt = ce_out$d_opt,
        error = ce_out$error,
        stringsAsFactors = FALSE
      )
      existing_keys <- c(existing_keys, ce_key)
      row_counter <- row_counter + 1L
    }
  }

  message(
    sprintf(
      "completed scenario %d/%d :: rep=%s repeats=%d trial=%s cand=%s int=%s quad=%s L=%d density=%s",
      i,
      nrow(base_scenarios),
      repeat_scale,
      repeats,
      trial_size,
      candidate_size,
      interactions,
      quadratic,
      nominal_levels,
      density
    )
  )

  if (i %% 8L == 0L || i == nrow(base_scenarios)) {
    new_rows <- if (length(results) > 0) {
      do.call(rbind, results)
    } else {
      data.frame()
    }
    raw_checkpoint <- if (nrow(existing_raw) > 0 && nrow(new_rows) > 0) {
      rbind(existing_raw, new_rows)
    } else if (nrow(existing_raw) > 0) {
      existing_raw
    } else {
      new_rows
    }
    if (nrow(raw_checkpoint) > 0) {
      raw_checkpoint_key <- build_result_key_df(raw_checkpoint)
      raw_checkpoint <- raw_checkpoint[!duplicated(raw_checkpoint_key, fromLast = TRUE), , drop = FALSE]
    }
    write.csv(raw_checkpoint, raw_results_path, row.names = FALSE)
    message(sprintf("checkpoint write: %d rows", nrow(raw_checkpoint)))
  }
}

new_rows <- if (length(results) > 0) {
  do.call(rbind, results)
} else {
  data.frame()
}
raw_df <- if (nrow(existing_raw) > 0 && nrow(new_rows) > 0) {
  rbind(existing_raw, new_rows)
} else if (nrow(existing_raw) > 0) {
  existing_raw
} else {
  new_rows
}
if (nrow(raw_df) > 0) {
  raw_df_key <- build_result_key_df(raw_df)
  raw_df <- raw_df[!duplicated(raw_df_key, fromLast = TRUE), , drop = FALSE]
}

point_df <- raw_df[raw_df$method == "point_exchange", ]
ce_df <- raw_df[raw_df$method == "coordinate_exchange", ]

join_cols <- c(
  "scenario_id", "iteration", "seed", "repeat_scale",
  "trial_size", "candidate_size", "interactions", "quadratic",
  "nominal_levels", "density", "candidate_rows", "p_cols", "trials", "repeats_base", "repeats"
)

paired_df <- merge(point_df, ce_df, by = join_cols, suffixes = c("_point", "_ce"), all = TRUE)
paired_df$d_opt_diff_ce_minus_point <- paired_df$d_opt_ce - paired_df$d_opt_point
paired_df$d_opt_ratio_ce_over_point <- paired_df$d_opt_ce / paired_df$d_opt_point
paired_df$runtime_ratio_point_over_ce <- paired_df$runtime_ms_point / paired_df$runtime_ms_ce
paired_df$runtime_log_ce_over_point <- log(paired_df$runtime_ms_ce / paired_df$runtime_ms_point)
paired_df$both_ok <- is.na(paired_df$error_point) & is.na(paired_df$error_ce)

paired_ok <- paired_df[
  paired_df$both_ok &
    is.finite(paired_df$d_opt_diff_ce_minus_point) &
    is.finite(paired_df$runtime_log_ce_over_point),
]

agg_factor <- aggregate(
  d_opt_diff_ce_minus_point ~ repeat_scale + trial_size + candidate_size + interactions + quadratic + nominal_levels + density,
  data = paired_ok,
  FUN = function(x) {
    c(n = length(x), mean = mean(x), median = median(x), sd = stats::sd(x))
  }
)

agg_factor_expanded <- if (nrow(agg_factor) > 0) {
  agg_col <- agg_factor$d_opt_diff_ce_minus_point
  agg_stats <- if (is.matrix(agg_col)) {
    as.data.frame(agg_col)
  } else if (is.list(agg_col)) {
    as.data.frame(do.call(rbind, agg_col))
  } else {
    as.data.frame(matrix(agg_col, ncol = 1))
  }
  names(agg_stats) <- c("n", "mean", "median", "sd")[seq_len(ncol(agg_stats))]
  data.frame(
    agg_factor[, c("repeat_scale", "trial_size", "candidate_size", "interactions", "quadratic", "nominal_levels", "density"), drop = FALSE],
    agg_stats,
    stringsAsFactors = FALSE
  )
} else {
  data.frame()
}

# ---------------------------------------------------------------------------
# 3) LM analysis (paired difference + runtime log-ratio) with pairwise terms
# ---------------------------------------------------------------------------
lm_global <- NULL
lm_quality <- NULL
lm_runtime <- NULL
coef_global <- data.frame()
coef_quality <- data.frame()
coef_runtime <- data.frame()
anova_quality <- data.frame()
anova_runtime <- data.frame()

if (nrow(paired_ok) >= 30) {
  paired_ok$repeat_scale <- factor(paired_ok$repeat_scale, levels = c("r20", "r60"))
  paired_ok$trial_size <- factor(paired_ok$trial_size, levels = c("small", "large"))
  paired_ok$candidate_size <- factor(paired_ok$candidate_size, levels = c("small", "large"))
  paired_ok$interactions <- factor(paired_ok$interactions, levels = c("none", "some", "many"))
  paired_ok$quadratic <- factor(paired_ok$quadratic, levels = c("none", "some", "many"))
  paired_ok$nominal_levels <- factor(as.character(paired_ok$nominal_levels), levels = c("3", "4", "5"))
  paired_ok$density <- factor(paired_ok$density, levels = c("low", "high"))

  lm_global <- lm(d_opt_diff_ce_minus_point ~ 1, data = paired_ok)
  lm_quality <- lm(
    d_opt_diff_ce_minus_point ~ (repeat_scale + trial_size + candidate_size + interactions + quadratic + nominal_levels + density)^2,
    data = paired_ok
  )
  lm_runtime <- lm(
    runtime_log_ce_over_point ~ (repeat_scale + trial_size + candidate_size + interactions + quadratic + nominal_levels + density)^2,
    data = paired_ok
  )

  coef_global <- as.data.frame(summary(lm_global)$coefficients)
  coef_global$term <- rownames(coef_global)
  rownames(coef_global) <- NULL
  coef_global <- coef_global[, c("term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  names(coef_global) <- c("term", "estimate", "std_error", "t_value", "p_value")

  coef_quality <- as.data.frame(summary(lm_quality)$coefficients)
  coef_quality$term <- rownames(coef_quality)
  rownames(coef_quality) <- NULL
  coef_quality <- coef_quality[, c("term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  names(coef_quality) <- c("term", "estimate", "std_error", "t_value", "p_value")

  coef_runtime <- as.data.frame(summary(lm_runtime)$coefficients)
  coef_runtime$term <- rownames(coef_runtime)
  rownames(coef_runtime) <- NULL
  coef_runtime <- coef_runtime[, c("term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  names(coef_runtime) <- c("term", "estimate", "std_error", "t_value", "p_value")

  anova_quality <- as.data.frame(anova(lm_quality))
  anova_quality$term <- rownames(anova_quality)
  rownames(anova_quality) <- NULL
  anova_quality <- anova_quality[, c("term", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")]

  anova_runtime <- as.data.frame(anova(lm_runtime))
  anova_runtime$term <- rownames(anova_runtime)
  rownames(anova_runtime) <- NULL
  anova_runtime <- anova_runtime[, c("term", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")]
}

# ---------------------------------------------------------------------------
# 4) Write outputs
# ---------------------------------------------------------------------------
write.csv(power_scan, power_scan_path, row.names = FALSE)
write.csv(powered_design_df, powered_design_path, row.names = FALSE)
write.csv(base_scenarios, base_scenarios_path, row.names = FALSE)
write.csv(raw_df, raw_results_path, row.names = FALSE)
write.csv(paired_df, paired_results_path, row.names = FALSE)
write.csv(agg_factor_expanded, summary_results_path, row.names = FALSE)
write.csv(coef_global, lm_global_coef_path, row.names = FALSE)
write.csv(coef_quality, lm_quality_coef_path, row.names = FALSE)
write.csv(anova_quality, lm_quality_anova_path, row.names = FALSE)
write.csv(coef_runtime, lm_runtime_coef_path, row.names = FALSE)
write.csv(anova_runtime, lm_runtime_anova_path, row.names = FALSE)

stats_lines <- c(
  sprintf("alpha_target=%.4f", alpha_target),
  sprintf("effectsize_target=%.4f", effectsize_target),
  sprintf("power_target=%.4f", power_target),
  sprintf("powered_trials=%d", powered_trials),
  sprintf("powered_method_power=%.6f", row_hit$method_power),
  sprintf("base_scenarios=%d", nrow(base_scenarios)),
  sprintf("iterations_per_scenario=%d", iterations_per_scenario),
  sprintf("total_runs=%d", nrow(raw_df)),
  sprintf("paired_runs=%d", nrow(paired_df)),
  sprintf("paired_ok=%d", nrow(paired_ok))
)

if (nrow(coef_global) > 0) {
  stats_lines <- c(
    stats_lines,
    "",
    "lm_global_quality_diff:",
    sprintf("  estimate=%.6f", coef_global$estimate[[1]]),
    sprintf("  std_error=%.6f", coef_global$std_error[[1]]),
    sprintf("  t_value=%.6f", coef_global$t_value[[1]]),
    sprintf("  p_value=%.12f", coef_global$p_value[[1]])
  )
}

if (!is.null(lm_quality)) {
  stats_lines <- c(
    stats_lines,
    "",
    "lm_quality_pairwise:",
    sprintf("  r_squared=%.6f", summary(lm_quality)$r.squared),
    sprintf("  adj_r_squared=%.6f", summary(lm_quality)$adj.r.squared)
  )
}

if (!is.null(lm_runtime)) {
  stats_lines <- c(
    stats_lines,
    "",
    "lm_runtime_pairwise:",
    sprintf("  r_squared=%.6f", summary(lm_runtime)$r.squared),
    sprintf("  adj_r_squared=%.6f", summary(lm_runtime)$adj.r.squared)
  )
}

writeLines(stats_lines, con = stats_results_path)
message("Wrote interaction/repeat benchmark outputs to ", results_dir)
