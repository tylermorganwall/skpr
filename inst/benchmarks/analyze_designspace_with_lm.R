suppressPackageStartupMessages({
  library(data.table)
})

results_dir <- "inst/benchmarks/results"
paired_path <- file.path(results_dir, "designspace_experiment_paired.csv")
raw_path <- file.path(results_dir, "designspace_experiment_raw.csv")

if (!file.exists(paired_path)) {
  stop("Missing paired results file: ", paired_path)
}
if (!file.exists(raw_path)) {
  stop("Missing raw results file: ", raw_path)
}

paired <- fread(paired_path)
raw <- fread(raw_path)

paired_ok <- paired[
  is.na(error_point) &
    is.na(error_ce) &
    is.finite(d_opt_diff_ce_minus_point) &
    is.finite(runtime_ms_point) &
    is.finite(runtime_ms_ce)
]

if (nrow(paired_ok) < 10L) {
  stop("Not enough valid paired rows for lm analysis.")
}

paired_ok[, `:=`(
  trial_size = factor(trial_size, levels = c("small", "large")),
  candidate_size = factor(candidate_size, levels = c("small", "large")),
  interactions = factor(interactions, levels = c("none", "some", "many")),
  quadratic = factor(quadratic, levels = c("none", "some", "many")),
  nominal_levels = factor(as.character(nominal_levels), levels = c("3", "4", "5")),
  density = factor(density, levels = c("low", "high")),
  runtime_log_ce_over_point = log(runtime_ms_ce / runtime_ms_point)
)]

# Quality model: paired difference (CE - point) as response.
lm_quality_global <- lm(d_opt_diff_ce_minus_point ~ 1, data = paired_ok)

# Quality model with scenario covariates (baseline + shifts by factor levels).
lm_quality <- lm(
  d_opt_diff_ce_minus_point ~
    trial_size + candidate_size + interactions + quadratic + nominal_levels + density,
  data = paired_ok
)

# Runtime model: log runtime ratio (CE / point) as response.
lm_runtime <- lm(
  runtime_log_ce_over_point ~
    trial_size + candidate_size + interactions + quadratic + nominal_levels + density,
  data = paired_ok
)

coef_quality <- as.data.table(summary(lm_quality)$coefficients, keep.rownames = "term")
setnames(
  coef_quality,
  old = c("Estimate", "Std. Error", "t value", "Pr(>|t|)"),
  new = c("estimate", "std_error", "t_value", "p_value")
)
coef_runtime <- as.data.table(summary(lm_runtime)$coefficients, keep.rownames = "term")
setnames(
  coef_runtime,
  old = c("Estimate", "Std. Error", "t value", "Pr(>|t|)"),
  new = c("estimate", "std_error", "t_value", "p_value")
)

anova_quality <- as.data.table(anova(lm_quality), keep.rownames = "term")
anova_runtime <- as.data.table(anova(lm_runtime), keep.rownames = "term")
coef_quality_global <- as.data.table(summary(lm_quality_global)$coefficients, keep.rownames = "term")
setnames(
  coef_quality_global,
  old = c("Estimate", "Std. Error", "t value", "Pr(>|t|)"),
  new = c("estimate", "std_error", "t_value", "p_value")
)

write.csv(
  coef_quality,
  file.path(results_dir, "designspace_lm_quality_coefficients.csv"),
  row.names = FALSE
)
write.csv(
  coef_quality_global,
  file.path(results_dir, "designspace_lm_quality_global_coefficients.csv"),
  row.names = FALSE
)
write.csv(
  anova_quality,
  file.path(results_dir, "designspace_lm_quality_anova.csv"),
  row.names = FALSE
)
write.csv(
  coef_runtime,
  file.path(results_dir, "designspace_lm_runtime_coefficients.csv"),
  row.names = FALSE
)
write.csv(
  anova_runtime,
  file.path(results_dir, "designspace_lm_runtime_anova.csv"),
  row.names = FALSE
)

overall <- raw[
  is.na(error),
  .(
    mean_d = mean(d_opt, na.rm = TRUE),
    median_d = median(d_opt, na.rm = TRUE),
    mean_runtime_ms = mean(runtime_ms, na.rm = TRUE)
  ),
  by = method
]

lines <- c(
  sprintf("paired_rows=%d", nrow(paired_ok)),
  sprintf("quality_global_mean_ce_minus_point=%.6f", coef(lm_quality_global)[["(Intercept)"]]),
  sprintf(
    "quality_global_mean_pvalue=%.12f",
    coef_quality_global[term == "(Intercept)", p_value]
  ),
  sprintf("quality_adjusted_baseline_ce_minus_point=%.6f", coef(lm_quality)[["(Intercept)"]]),
  sprintf("quality_r_squared=%.6f", summary(lm_quality)$r.squared),
  sprintf("quality_adj_r_squared=%.6f", summary(lm_quality)$adj.r.squared),
  sprintf("runtime_log_ratio_intercept=%.6f", coef(lm_runtime)[["(Intercept)"]]),
  sprintf("runtime_r_squared=%.6f", summary(lm_runtime)$r.squared),
  sprintf("runtime_adj_r_squared=%.6f", summary(lm_runtime)$adj.r.squared),
  "",
  "mean_by_method:"
)

for (i in seq_len(nrow(overall))) {
  lines <- c(
    lines,
    sprintf(
      "  %s: mean_d=%.6f median_d=%.6f mean_runtime_ms=%.3f",
      overall$method[[i]],
      overall$mean_d[[i]],
      overall$median_d[[i]],
      overall$mean_runtime_ms[[i]]
    )
  )
}

writeLines(lines, file.path(results_dir, "designspace_lm_analysis.txt"))

message("Wrote lm analysis outputs to ", results_dir)
