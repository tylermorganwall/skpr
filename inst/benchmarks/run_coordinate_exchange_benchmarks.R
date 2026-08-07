#!/usr/bin/env Rscript

schema_version = 1L

parse_args = function(args) {
  out = list(
    track = "all",
    profile = "smoke",
    output_dir = file.path("inst", "benchmarks", "results")
  )
  for (arg in args) {
    if (grepl("^--track=", arg)) {
      out$track = sub("^--track=", "", arg)
    }
    if (grepl("^--profile=", arg)) {
      out$profile = sub("^--profile=", "", arg)
    }
    if (grepl("^--output-dir=", arg)) {
      out$output_dir = sub("^--output-dir=", "", arg)
    }
  }
  if (!(out$track %in% c("public", "kernel", "all"))) {
    stop("--track must be public, kernel, or all")
  }
  if (!(out$profile %in% c("smoke", "full"))) {
    stop("--profile must be smoke or full")
  }
  out
}

options = parse_args(commandArgs(trailingOnly = TRUE))
dir.create(options$output_dir, recursive = TRUE, showWarnings = FALSE)

if (
  file.exists("DESCRIPTION") && requireNamespace("devtools", quietly = TRUE)
) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(skpr)
}

internal = function(name) {
  get(name, envir = asNamespace("skpr"), inherits = FALSE)
}
ce_infer = internal("skpr_ce_infer_factor_space")
ce_encode = internal("skpr_ce_encode_points")
ce_modelmatrix = internal("skpr_ce_make_modelmatrix_fn")
compile_constraints = internal("compile_constraints")
resolve_groups = internal("skpr_ce_resolve_coordinate_groups")
ce_engine = internal("genOptimalDesignCoordinateExchangeConstrained")
point_engine = internal("genOptimalDesign")
d_optimality = internal("DOptimalityLog")

numeric_grid = function(q, levels = seq(-1, 1, length.out = 5)) {
  out = expand.grid(
    rep(list(levels), q),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  names(out) = paste0("x", seq_len(q))
  out
}

make_scenarios = function() {
  numeric_small = numeric_grid(3)
  interaction = numeric_grid(4)
  mixed = expand.grid(
    x1 = seq(-1, 1, length.out = 5),
    x2 = seq(-1, 1, length.out = 5),
    A = factor(c("a", "b", "c")),
    B = factor(c("low", "high")),
    KEEP.OUT.ATTRS = FALSE
  )
  linear_full = numeric_grid(4)
  linear_filter = quote(x1 + x2 + x3 <= 0)
  equality_full = numeric_grid(4)
  equality_filter = quote(x1 + x2 == 0)
  forbidden_full = mixed
  forbidden = data.frame(A = c("c", "a"), B = c("high", "low"))

  disconnected_full = numeric_grid(3)
  disconnected_filter = quote(
    (x1 <= -0.5 & x2 <= -0.5) | (x1 >= 0.5 & x2 >= 0.5)
  )
  sparse_full = numeric_grid(5)
  sparse_filter = quote(x1 + x2 + x3 + x4 + x5 == 0)
  oversized_full = numeric_grid(6)
  oversized_filter = quote(
    x1 + x2 <= 2 & x2 + x3 <= 2 & x3 + x4 <= 2 & x4 + x5 <= 2 & x5 + x6 <= 2
  )
  masked_full = numeric_grid(8, 0:4)
  near_saturated = numeric_grid(4, c(-1, 1))
  ill_conditioned = numeric_grid(3)

  make = function(
    id,
    full,
    model,
    trials,
    filter_expr = TRUE,
    forbidden_tuples = NULL,
    smoke = FALSE,
    ce_options = list()
  ) {
    keep = if (isTRUE(filter_expr)) {
      rep(TRUE, nrow(full))
    } else {
      as.logical(eval(filter_expr, full, parent.frame()))
    }
    if (!is.null(forbidden_tuples)) {
      for (row in seq_len(nrow(forbidden_tuples))) {
        hit = rep(TRUE, nrow(full))
        for (name in names(forbidden_tuples)) {
          hit = hit &
            as.character(full[[name]]) ==
              as.character(forbidden_tuples[[name]][row])
        }
        keep = keep & !hit
      }
    }
    feasible = droplevels(full[keep, , drop = FALSE])
    constraints = if (isTRUE(filter_expr) && is.null(forbidden_tuples)) {
      NULL
    } else {
      list(filter_expr = filter_expr, forbidden_tuples = forbidden_tuples)
    }
    list(
      id = id,
      candidates = feasible,
      model = model,
      trials = trials,
      constraints = constraints,
      smoke = smoke,
      ce_options = ce_options
    )
  }

  list(
    make("numeric_main_small", numeric_small, ~., 12L, smoke = TRUE),
    make("numeric_interaction", interaction, ~ (.)^2, 20L, smoke = TRUE),
    make("mixed_factors", mixed, ~ x1 * A + x2 + B, 18L, smoke = TRUE),
    make(
      "linear_halfspace",
      linear_full,
      ~ (.)^2,
      18L,
      linear_filter,
      smoke = TRUE
    ),
    make(
      "equality_coupled",
      equality_full,
      ~ x1 + x3 + x1:x2,
      12L,
      equality_filter,
      smoke = TRUE
    ),
    make(
      "forbidden_mixed",
      forbidden_full,
      ~ x1 + x2 + A + B,
      16L,
      forbidden_tuples = forbidden,
      smoke = TRUE
    ),
    make(
      "disconnected_regions",
      disconnected_full,
      ~ (.)^2,
      14L,
      disconnected_filter
    ),
    make("sparse_equality", sparse_full, ~., 14L, sparse_filter),
    make(
      "oversized_connected",
      oversized_full,
      ~ x1 + x2 + x3,
      12L,
      oversized_filter,
      ce_options = list(coordinate_group_max_candidates = 1000L)
    ),
    make(
      "masked_dependency",
      masked_full,
      ~ x5 + I(x1 * (x2 == 0) * (x3 == 0) * (x4 == 0)),
      10L
    ),
    make(
      "near_saturated",
      near_saturated,
      ~ (.)^2,
      15L
    ),
    make(
      "ill_conditioned",
      ill_conditioned,
      ~ x1 + I(x1 + 1e-7 * x2) + x3,
      12L
    )
  )
}

scenarios = make_scenarios()
if (options$profile == "smoke") {
  scenarios = Filter(function(x) isTRUE(x$smoke), scenarios)
  seeds = 1101:1103
  public_repeats = 2L
  kernel_timings = 2L
  bootstrap_repetitions = 200L
} else {
  seeds = 2101:2130
  public_repeats = 20L
  kernel_timings = 5L
  bootstrap_repetitions = 2000L
}

constraint_ok = function(design, constraints) {
  if (is.null(constraints)) {
    return(TRUE)
  }
  ok = if (is.null(constraints$filter_expr)) {
    rep(TRUE, nrow(design))
  } else {
    as.logical(eval(constraints$filter_expr, design, parent.frame()))
  }
  forbidden = constraints$forbidden_tuples
  if (!is.null(forbidden)) {
    forbidden = as.data.frame(forbidden, stringsAsFactors = FALSE)
    for (row in seq_len(nrow(forbidden))) {
      hit = rep(TRUE, nrow(design))
      for (name in names(forbidden)) {
        hit = hit &
          as.character(design[[name]]) == as.character(forbidden[[name]][row])
      }
      ok = ok & !hit
    }
  }
  all(ok)
}

matrix_metrics = function(model_matrix) {
  information = crossprod(model_matrix)
  eigenvalues = eigen(information, symmetric = TRUE, only.values = TRUE)$values
  list(
    d_eff = 100 * d_optimality(model_matrix),
    rank = qr(model_matrix)$rank,
    reciprocal_condition = min(eigenvalues) / max(eigenvalues)
  )
}

empty_result = function(
  track,
  scenario,
  seed,
  method,
  method_order,
  timing_repetition,
  phase
) {
  data.frame(
    schema_version = schema_version,
    profile = options$profile,
    track = track,
    scenario = scenario$id,
    seed = seed,
    method = method,
    method_order = method_order,
    timing_repetition = timing_repetition,
    phase = phase,
    status = "error",
    error_class = NA_character_,
    error_message = NA_character_,
    elapsed_ms = NA_real_,
    trials = scenario$trials,
    repeats = if (track == "public") public_repeats else 1L,
    candidate_rows = nrow(scenario$candidates),
    model_columns = NA_integer_,
    d_eff = NA_real_,
    rank = NA_integer_,
    reciprocal_condition = NA_real_,
    feasible = FALSE,
    start_id = paste(scenario$id, seed, sep = ":"),
    ce_iterations = NA_integer_,
    ce_accepted_moves = NA_integer_,
    ce_proposals_scored = NA_real_,
    ce_capped_groups = NA_integer_,
    ce_stop_reason = NA_character_,
    stringsAsFactors = FALSE
  )
}

record_run = function(template, expression, metric_fn) {
  started = as.numeric(Sys.time())
  value = tryCatch(
    force(expression),
    error = function(error) {
      structure(list(error = error), class = "benchmark_error")
    }
  )
  template$elapsed_ms = 1000 * (as.numeric(Sys.time()) - started)
  if (inherits(value, "benchmark_error")) {
    template$error_class = class(value$error)[[1L]]
    template$error_message = conditionMessage(value$error)
    return(template)
  }
  metrics = tryCatch(
    metric_fn(value),
    error = function(error) {
      structure(list(error = error), class = "benchmark_error")
    }
  )
  if (inherits(metrics, "benchmark_error")) {
    template$error_class = class(metrics$error)[[1L]]
    template$error_message = conditionMessage(metrics$error)
    return(template)
  }
  for (name in names(metrics)) {
    template[[name]] = metrics[[name]]
  }
  template$status = "ok"
  template
}

public_run = function(scenario, seed, method) {
  set.seed(seed)
  advanced = if (method == "coordinate_exchange") {
    c(
      list(search_method = "coordinate_exchange"),
      if (is.null(scenario$constraints)) {
        list()
      } else {
        list(constraints = scenario$constraints)
      },
      scenario$ce_options
    )
  } else {
    list(search_method = "fedorov")
  }
  gen_design(
    candidateset = scenario$candidates,
    model = scenario$model,
    trials = scenario$trials,
    repeats = public_repeats,
    optimality = "D",
    progress = FALSE,
    advancedoptions = advanced
  )
}

public_metrics = function(design, scenario) {
  model_matrix = attr(design, "model_matrix")
  metrics = matrix_metrics(model_matrix)
  diagnostics = attr(design, "coordinate_exchange_diagnostics")
  selected = if (is.null(diagnostics)) NULL else diagnostics$selected
  c(
    metrics,
    list(
      model_columns = ncol(model_matrix),
      feasible = constraint_ok(design, scenario$constraints),
      ce_iterations = if (is.null(selected)) {
        NA_integer_
      } else {
        selected$iterations
      },
      ce_accepted_moves = if (is.null(selected)) {
        NA_integer_
      } else {
        selected$accepted_moves
      },
      ce_proposals_scored = if (is.null(selected)) {
        NA_real_
      } else {
        selected$proposals_scored
      },
      ce_capped_groups = if (is.null(selected)) {
        NA_integer_
      } else {
        selected$capped_groups
      },
      ce_stop_reason = if (is.null(selected)) {
        NA_character_
      } else {
        selected$stop_reason
      }
    )
  )
}

prepare_kernel = function(scenario, seed) {
  set.seed(seed)
  space = ce_infer(scenario$candidates)
  encoded = ce_encode(scenario$candidates, space$factor_meta)
  model_matrix_fn = ce_modelmatrix(scenario$model, space$factor_meta)
  candidate_matrix = model_matrix_fn(encoded)
  if (qr(candidate_matrix)$rank < ncol(candidate_matrix)) {
    stop("feasible candidate model matrix is rank deficient")
  }
  initial_rows = integer()
  current_rank = 0L
  candidate_order = sample(seq_len(nrow(candidate_matrix)))
  while (current_rank < ncol(candidate_matrix)) {
    chosen = NA_integer_
    for (candidate in candidate_order) {
      trial_rows = c(initial_rows, candidate)
      trial_rank = qr(candidate_matrix[trial_rows, , drop = FALSE])$rank
      if (trial_rank > current_rank) {
        chosen = candidate
        current_rank = trial_rank
        break
      }
    }
    if (is.na(chosen)) {
      stop("could not construct a full-rank matched start")
    }
    initial_rows = c(initial_rows, chosen)
  }
  if (length(initial_rows) < scenario$trials) {
    initial_rows = c(
      initial_rows,
      sample(
        seq_len(nrow(encoded)),
        scenario$trials - length(initial_rows),
        replace = TRUE
      )
    )
  }
  initial_rows = sample(initial_rows)
  initial_points = encoded[initial_rows, , drop = FALSE]
  ir = if (is.null(scenario$constraints)) {
    NULL
  } else {
    compile_constraints(
      filter_expr = scenario$constraints$filter_expr,
      forbidden_tuples = scenario$constraints$forbidden_tuples,
      factor_meta = space$factor_meta,
      factor_levels = space$factor_levels
    )
  }
  groups = resolve_groups(names(space$factor_meta), ir)$coordinate_groups
  list(
    space = space,
    encoded = encoded,
    model_matrix_fn = model_matrix_fn,
    candidate_matrix = candidate_matrix,
    initial_rows = initial_rows,
    initial_points = initial_points,
    ir = ir,
    groups = groups
  )
}

kernel_run = function(prepared, scenario, method) {
  if (method == "point_exchange") {
    initial_matrix = prepared$candidate_matrix[
      prepared$initial_rows,
      ,
      drop = FALSE
    ]
    point_engine(
      initialdesign = initial_matrix,
      candidatelist = prepared$candidate_matrix,
      condition = "D",
      momentsmatrix = matrix(0, 0, 0),
      initialRows = as.numeric(prepared$initial_rows),
      aliasdesign = initial_matrix,
      aliascandidatelist = prepared$candidate_matrix,
      minDopt = 0,
      tolerance = 1e-4,
      augmentedrows = 0L,
      kexchange = scenario$trials
    )
  } else {
    arguments = c(
      list(
        points = prepared$initial_points,
        factor_levels = unname(prepared$space$factor_levels),
        modelmatrix_fn = prepared$model_matrix_fn,
        coordinate_groups = prepared$groups,
        constraints_ir = prepared$ir,
        tolerance = 1e-4,
        kexchange = scenario$trials
      ),
      scenario$ce_options
    )
    do.call(ce_engine, arguments)
  }
}

kernel_metrics = function(result, scenario, method) {
  model_matrix = result$model_matrix
  metrics = matrix_metrics(model_matrix)
  diagnostics = if (method == "coordinate_exchange") {
    result$diagnostics
  } else {
    NULL
  }
  c(
    metrics,
    list(
      model_columns = ncol(model_matrix),
      feasible = TRUE,
      ce_iterations = if (is.null(diagnostics)) {
        NA_integer_
      } else {
        diagnostics$iterations
      },
      ce_accepted_moves = if (is.null(diagnostics)) {
        NA_integer_
      } else {
        diagnostics$accepted_moves
      },
      ce_proposals_scored = if (is.null(diagnostics)) {
        NA_real_
      } else {
        diagnostics$proposals_scored
      },
      ce_capped_groups = if (is.null(diagnostics)) {
        NA_integer_
      } else {
        diagnostics$capped_groups
      },
      ce_stop_reason = if (is.null(diagnostics)) {
        NA_character_
      } else {
        diagnostics$stop_reason
      }
    )
  )
}

rows = list()
append_row = function(row) rows[[length(rows) + 1L]] <<- row

for (scenario in scenarios) {
  message("benchmark scenario: ", scenario$id)
  for (seed_index in seq_along(seeds)) {
    seed = seeds[[seed_index]]
    method_order = if (seed_index %% 2L) {
      c("point_exchange", "coordinate_exchange")
    } else {
      c("coordinate_exchange", "point_exchange")
    }

    if (options$track %in% c("public", "all")) {
      for (order_index in seq_along(method_order)) {
        method = method_order[[order_index]]
        template = empty_result(
          "public",
          scenario,
          seed,
          method,
          order_index,
          1L,
          "end_to_end"
        )
        append_row(record_run(
          template,
          public_run(scenario, seed, method),
          function(value) public_metrics(value, scenario)
        ))
      }
    }

    if (options$track %in% c("kernel", "all")) {
      setup_template = empty_result(
        "kernel",
        scenario,
        seed,
        "shared",
        0L,
        1L,
        "setup"
      )
      setup_started = as.numeric(Sys.time())
      prepared = tryCatch(
        prepare_kernel(scenario, seed),
        error = function(error) {
          structure(list(error = error), class = "benchmark_error")
        }
      )
      setup_template$elapsed_ms =
        1000 * (as.numeric(Sys.time()) - setup_started)
      if (inherits(prepared, "benchmark_error")) {
        setup_template$error_class = class(prepared$error)[[1L]]
        setup_template$error_message = conditionMessage(prepared$error)
        append_row(setup_template)
        next
      }
      setup_template$status = "ok"
      setup_template$model_columns = ncol(prepared$candidate_matrix)
      setup_template$feasible = TRUE
      append_row(setup_template)

      for (timing_repetition in seq_len(kernel_timings)) {
        for (order_index in seq_along(method_order)) {
          method = method_order[[order_index]]
          set.seed(seed + timing_repetition - 1L)
          template = empty_result(
            "kernel",
            scenario,
            seed,
            method,
            order_index,
            timing_repetition,
            "search"
          )
          append_row(record_run(
            template,
            kernel_run(prepared, scenario, method),
            function(value) kernel_metrics(value, scenario, method)
          ))
        }
      }
    }
  }
}

raw = do.call(rbind, rows)
raw = raw[order(raw$track, raw$scenario, raw$seed, raw$phase, raw$method), ]

method_rows = raw[
  raw$method %in%
    c("point_exchange", "coordinate_exchange") &
    raw$phase %in% c("end_to_end", "search"),
]
point = method_rows[method_rows$method == "point_exchange", ]
coordinate = method_rows[method_rows$method == "coordinate_exchange", ]
join = c(
  "schema_version",
  "profile",
  "track",
  "scenario",
  "seed",
  "timing_repetition",
  "phase"
)
paired = merge(
  point,
  coordinate,
  by = join,
  suffixes = c("_point", "_ce"),
  all = TRUE
)
paired$both_ok = paired$status_point == "ok" & paired$status_ce == "ok"
paired$runtime_ratio_point_over_ce = paired$elapsed_ms_point /
  paired$elapsed_ms_ce
paired$d_ratio_ce_over_point = paired$d_eff_ce / paired$d_eff_point
paired$d_difference_ce_minus_point = paired$d_eff_ce - paired$d_eff_point

bootstrap_interval = function(x, statistic = median, repetitions) {
  x = x[is.finite(x)]
  if (length(x) == 0L) {
    return(c(NA_real_, NA_real_))
  }
  estimates = replicate(
    repetitions,
    statistic(sample(x, length(x), replace = TRUE))
  )
  unname(quantile(estimates, c(0.025, 0.975), na.rm = TRUE))
}

wilson_interval = function(successes, total, confidence = 0.95) {
  if (total == 0L) {
    return(c(NA_real_, NA_real_))
  }
  z = qnorm(1 - (1 - confidence) / 2)
  proportion = successes / total
  denominator = 1 + z^2 / total
  centre = (proportion + z^2 / (2 * total)) / denominator
  half = z *
    sqrt(proportion * (1 - proportion) / total + z^2 / (4 * total^2)) /
    denominator
  c(max(0, centre - half), min(1, centre + half))
}

set.seed(90210)
summary_keys = unique(paired[c("track", "scenario", "phase")])
summary_rows = lapply(seq_len(nrow(summary_keys)), function(index) {
  key = summary_keys[index, ]
  subset = paired[
    paired$track == key$track &
      paired$scenario == key$scenario &
      paired$phase == key$phase,
  ]
  ok = subset[subset$both_ok %in% TRUE, ]
  runtime_ci = bootstrap_interval(
    ok$runtime_ratio_point_over_ce,
    repetitions = bootstrap_repetitions
  )
  quality_ci = bootstrap_interval(
    ok$d_ratio_ce_over_point,
    repetitions = bootstrap_repetitions
  )
  failure_ci = wilson_interval(sum(!subset$both_ok), nrow(subset))
  data.frame(
    schema_version = schema_version,
    profile = options$profile,
    track = key$track,
    scenario = key$scenario,
    phase = key$phase,
    pairs = nrow(subset),
    successful_pairs = nrow(ok),
    failure_rate = mean(!subset$both_ok),
    failure_rate_low = failure_ci[[1L]],
    failure_rate_high = failure_ci[[2L]],
    runtime_ratio_median = median(ok$runtime_ratio_point_over_ce, na.rm = TRUE),
    runtime_ratio_iqr = IQR(ok$runtime_ratio_point_over_ce, na.rm = TRUE),
    runtime_ratio_low = runtime_ci[[1L]],
    runtime_ratio_high = runtime_ci[[2L]],
    d_ratio_median = median(ok$d_ratio_ce_over_point, na.rm = TRUE),
    d_ratio_iqr = IQR(ok$d_ratio_ce_over_point, na.rm = TRUE),
    d_ratio_low = quality_ci[[1L]],
    d_ratio_high = quality_ci[[2L]]
  )
})
summary = do.call(rbind, summary_rows)

prefix = paste0("ce_point_", options$profile)
raw_path = file.path(options$output_dir, paste0(prefix, "_raw.csv"))
paired_path = file.path(options$output_dir, paste0(prefix, "_paired.csv"))
summary_path = file.path(options$output_dir, paste0(prefix, "_summary.csv"))
provenance_path = file.path(
  options$output_dir,
  paste0(prefix, "_provenance.txt")
)
write.csv(raw, raw_path, row.names = FALSE, na = "")
write.csv(paired, paired_path, row.names = FALSE, na = "")
write.csv(summary, summary_path, row.names = FALSE, na = "")

git_value = function(args) {
  tryCatch(
    paste(system2("git", args, stdout = TRUE, stderr = FALSE), collapse = " "),
    error = function(error) NA_character_
  )
}
script_path = file.path(
  "inst",
  "benchmarks",
  "run_coordinate_exchange_benchmarks.R"
)
schema_md5 = digest::digest(
  list(
    version = schema_version,
    raw = names(raw),
    paired = names(paired),
    summary = names(summary)
  ),
  algo = "md5"
)
scenario_table_md5 = digest::digest(
  lapply(scenarios, function(scenario) {
    list(
      id = scenario$id,
      model = paste(deparse(scenario$model), collapse = " "),
      trials = scenario$trials,
      candidate_dimensions = dim(scenario$candidates),
      constraints = scenario$constraints,
      ce_options = scenario$ce_options
    )
  }),
  algo = "md5"
)
hardware_value = function(label) {
  if (Sys.info()[["sysname"]] != "Darwin") {
    return(NA_character_)
  }
  hardware = tryCatch(
    system2(
      "/usr/sbin/system_profiler",
      "SPHardwareDataType",
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(error) character()
  )
  line = grep(
    paste0("^[[:space:]]*", label, ":"),
    hardware,
    value = TRUE
  )
  if (length(line) != 1L) {
    return(NA_character_)
  }
  trimws(sub("^[^:]+:", "", line))
}
cpu_model = hardware_value("Chip")
if (is.na(cpu_model)) {
  cpu_model = Sys.info()[["machine"]]
}
logical_cores = parallel::detectCores(logical = TRUE)
if (is.na(logical_cores)) {
  core_description = hardware_value("Total Number of Cores")
  logical_cores = suppressWarnings(as.integer(sub(
    " .*",
    "",
    core_description
  )))
}
ce_defaults = paste(
  c(
    "design_search_tolerance=1e-4",
    "ce_max_iter=200",
    "ce_recompute_every=10",
    "ce_repair_stuck_limit=5",
    "ce_repair_max_tries=2000",
    "coordinate_group_max_candidates=10000"
  ),
  collapse = ";"
)
ce_overrides = vapply(
  scenarios,
  function(scenario) {
    values = scenario$ce_options
    text = if (length(values) == 0L) {
      "none"
    } else {
      paste(paste0(names(values), "=", unlist(values)), collapse = ";")
    }
    paste0(scenario$id, "{", text, "}")
  },
  character(1L)
)
provenance = c(
  paste0("schema_version: ", schema_version),
  paste0("profile: ", options$profile),
  paste0("track: ", options$track),
  paste0("timestamp_utc: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("package_version: ", as.character(utils::packageVersion("skpr"))),
  paste0("git_sha: ", git_value(c("rev-parse", "HEAD"))),
  paste0("git_dirty: ", nzchar(git_value(c("status", "--porcelain")))),
  paste0("script_md5: ", unname(tools::md5sum(script_path))),
  paste0("schema_md5: ", schema_md5),
  paste0("scenario_table_md5: ", scenario_table_md5),
  paste0("platform: ", R.version$platform),
  paste0("os: ", Sys.info()[["sysname"]], " ", Sys.info()[["release"]]),
  paste0("cpu_model: ", cpu_model),
  paste0("logical_cores: ", logical_cores),
  paste0("seeds: ", paste(seeds, collapse = ",")),
  paste0("public_repeats: ", public_repeats),
  paste0("kernel_timing_repetitions: ", kernel_timings),
  paste0("ce_defaults: ", ce_defaults),
  paste0("ce_scenario_overrides: ", paste(ce_overrides, collapse = ",")),
  "",
  "session_info:",
  capture.output(sessionInfo())
)
writeLines(provenance, provenance_path)

successful = paired[paired$both_ok %in% TRUE, ]
if (nrow(successful) == 0L) {
  stop("benchmark produced no successful pairs")
}
if (any(successful$d_ratio_ce_over_point < 0.95, na.rm = TRUE)) {
  stop("coordinate-exchange quality ratio fell below 0.95")
}
if (median(successful$d_ratio_ce_over_point, na.rm = TRUE) < 0.99) {
  stop("median coordinate-exchange quality ratio fell below 0.99")
}
if (any(!paired$both_ok)) {
  stop("one or more benchmark method pairs failed")
}

message("wrote benchmark outputs to ", normalizePath(options$output_dir))
