test_that("model tables retain R coding and structural dependencies", {
  grid = expand.grid(
    x = c(-1, 0, 1),
    y = c(-1, 0, 1),
    A = factor(c("a", "b", "c")),
    B = factor(c("l", "h"))
  )
  models = list(
    ~.,
    ~ (.)^2,
    ~ 0 + A + B,
    ~ A:B,
    ~ x:A + B,
    ~ x + I(x^2) + I(x > 0),
    ~ I((x + y)^2) + pmin(x, y),
    ~ ifelse(x > 0, y, 0) + I(x %in% c(-1, 1)),
    ~ I(x * y) + x + y
  )
  for (contrast in list(contr.sum, contr.treatment, contr.simplex)) {
    for (model in models) {
      prep = skpr_ce_prepare(grid, model, contrasts_fun = contrast)
      plan = skpr_ce_prepare_model_tables(
        prep$modelmatrix_fn,
        prep$factor_levels,
        "tables"
      )
      points = skpr_ce_encode_points(grid, prep$factor_meta)
      codes = sapply(seq_along(prep$factor_levels), function(j) {
        match(points[, j], prep$factor_levels[[j]]) - 1L
      })
      expected = prep$modelmatrix_fn(points)
      actual = matrix(NA_real_, nrow(expected), ncol(expected))
      for (block in plan$tables$blocks) {
        support = block$factors + 1L
        sizes = lengths(prep$factor_levels[support])
        strides = cumprod(c(1, sizes))[seq_along(sizes)]
        index = 1 + drop(codes[, support, drop = FALSE] %*% strides)
        actual[, block$columns + 1L] = block$values[index, , drop = FALSE]
      }
      expect_identical(as.vector(actual), as.vector(expected))
      expect_identical(plan$tables$columns, colnames(expected))
    }
  }
})

test_that("tables preserve sequential exchanges and avoid every native callback", {
  grid = expand.grid(x = c(-1, 0, 1), y = c(-1, 0, 1), z = c(-1, 0, 1))
  prep = skpr_ce_prepare(grid, ~ x * z)
  plan = skpr_ce_prepare_model_tables(
    prep$modelmatrix_fn,
    prep$factor_levels,
    "tables"
  )
  ir = compile_constraints(
    quote(x + y == 0),
    factor_meta = prep$factor_meta,
    factor_levels = prep$factor_levels
  )
  corners = as.matrix(expand.grid(x = c(-1, 1), z = c(-1, 1)))
  points = rbind(cbind(corners[, 1], -corners[, 1], corners[, 2]), c(0, 0, 0))
  arguments = list(
    points = points,
    factor_levels = unname(prep$factor_levels),
    modelmatrix_fn = prep$modelmatrix_fn,
    coordinate_groups = list(1:2, 3L),
    constraints_ir = ir,
    augmentedrows = 4L,
    kexchange = 1L,
    max_iter = 1L,
    tolerance = 0
  )
  reference = do.call(genOptimalDesignCoordinateExchangeConstrained, arguments)
  arguments$model_tables = plan$tables
  arguments$modelmatrix_fn = function(...) stop("unexpected R callback")
  actual = do.call(genOptimalDesignCoordinateExchangeConstrained, arguments)
  expect_identical(actual$points, reference$points)
  expect_identical(actual$model_matrix, reference$model_matrix)
  expect_identical(
    actual$diagnostics$objective_history,
    reference$diagnostics$objective_history
  )
  expect_identical(actual$points[5, ], c(-1, 1, -1))
  expect_identical(actual$diagnostics$model_matrix_calls, 0L)
  # Force initialization repair, then also exercise constrained proposal search.
  arguments$points[,] = 0
  arguments$augmentedrows = 0L
  arguments$kexchange = 5L
  arguments$max_iter = 5L
  for (seed in 1:3) {
    set.seed(seed)
    actual = do.call(genOptimalDesignCoordinateExchangeConstrained, arguments)
    arguments$model_tables = NULL
    arguments$modelmatrix_fn = prep$modelmatrix_fn
    set.seed(seed)
    reference = do.call(
      genOptimalDesignCoordinateExchangeConstrained,
      arguments
    )
    expect_identical(actual$level_codes, reference$level_codes)
    expect_identical(
      actual$diagnostics$objective_history,
      reference$diagnostics$objective_history
    )
    expect_identical(actual$diagnostics$model_matrix_calls, 0L)
    expect_match(actual$diagnostics$initialization_mode, "pool")
    arguments$model_tables = plan$tables
    arguments$modelmatrix_fn = function(...) stop("unexpected R callback")
  }
})

test_that("local tables preserve rows with unrelated undefined expressions", {
  grid = expand.grid(x = seq(-1, 1, 0.5), y = seq(-1, 1, 0.5))
  prep = skpr_ce_prepare(grid, ~ x + y + log(x + y))
  plan = skpr_ce_prepare_model_tables(
    prep$modelmatrix_fn,
    prep$factor_levels,
    "tables"
  )
  # Every preparation block anchors unrelated variables at -1; this makes log
  # undefined in many rows but must not remove valid x/y table entries.
  points = rbind(c(0, 1), c(1, 0), c(1, 1), c(0.5, 0.5), c(0.5, 1), c(1, 0.5))
  out = genOptimalDesignCoordinateExchangeConstrained(
    points,
    unname(prep$factor_levels),
    function(...) stop("callback"),
    list(1L, 2L),
    max_iter = 0L,
    model_tables = plan$tables
  )
  expect_identical(
    as.vector(out$model_matrix),
    as.vector(prep$modelmatrix_fn(points))
  )
  expect_identical(out$diagnostics$model_matrix_calls, 0L)
  points[1, ] = c(-1, -1)
  expect_error(
    genOptimalDesignCoordinateExchangeConstrained(
      points,
      unname(prep$factor_levels),
      function(...) stop("callback"),
      list(1L, 2L),
      max_iter = 0L,
      model_tables = plan$tables
    ),
    "non-finite"
  )
})

test_that("table limits provide explicit fallback and validate native schema", {
  prep = skpr_ce_prepare(data.frame(x = c(-1, 0, 1)), ~x)
  fallback = skpr_ce_prepare_model_tables(
    prep$modelmatrix_fn,
    prep$factor_levels,
    max_rows = 1
  )
  expect_identical(fallback$evaluator, "r")
  expect_match(fallback$fallback_reason, "limit")
  expect_error(
    skpr_ce_prepare_model_tables(
      prep$modelmatrix_fn,
      prep$factor_levels,
      "tables",
      max_bytes = 1
    ),
    "Cannot precompute"
  )
  expect_error(
    skpr_ce_prepare_model_tables(
      prep$modelmatrix_fn,
      prep$factor_levels,
      "invalid"
    ),
    "ce_model_evaluator"
  )
  tables = skpr_ce_prepare_model_tables(
    prep$modelmatrix_fn,
    prep$factor_levels
  )$tables
  arguments = list(
    points = matrix(c(-1, 1), ncol = 1),
    factor_levels = list(c(-1, 0, 1)),
    modelmatrix_fn = prep$modelmatrix_fn,
    coordinate_groups = list(1L),
    model_tables = tables
  )
  arguments$model_tables$factor_levels[[1L]] = c(-2, 0, 2)
  expect_error(
    do.call(genOptimalDesignCoordinateExchangeConstrained, arguments),
    "factor grids"
  )
  arguments$model_tables = tables
  arguments$model_tables$blocks[[1L]]$columns = 100L
  expect_error(
    do.call(genOptimalDesignCoordinateExchangeConstrained, arguments),
    "partition"
  )
})

test_that("model tables stay small and can be serialized for workers", {
  grid = as.data.frame(matrix(rep(seq(-1, 1, length.out = 5), 8L), ncol = 8L))
  names(grid) = paste0("x", 1:8)
  prep = skpr_ce_prepare(grid, ~ (.)^2)
  plan = skpr_ce_prepare_model_tables(
    prep$modelmatrix_fn,
    prep$factor_levels,
    "tables"
  )
  expect_equal(plan$tables$bytes, 741 * 8)
  expect_identical(unserialize(serialize(plan$tables, NULL)), plan$tables)
  # An eight-factor transformation requires its full local grid and exceeds the
  # default row budget; auto must fall back without allocating that table.
  prep = skpr_ce_prepare(grid, ~ I(x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8))
  plan = skpr_ce_prepare_model_tables(prep$modelmatrix_fn, prep$factor_levels)
  expect_identical(plan$evaluator, "r")
  expect_match(plan$fallback_reason, "limit")
  expect_identical(plan$preparation_calls, 1L)
})

test_that("public CE uses tables, audits results, and can force the R evaluator", {
  grid = expand.grid(x = c(-1, 0, 1), A = factor(c("a", "b", "c")))
  run = function(evaluator, ...) {
    set.seed(10)
    gen_design(
      grid,
      ~ x * A,
      trials = 12,
      repeats = 2,
      progress = FALSE,
      advancedoptions = list(
        search_method = "coordinate_exchange",
        ce_model_evaluator = evaluator,
        ...
      )
    )
  }
  tables = run("tables")
  reference = run("r")
  expect_identical(
    attr(tables, "model_matrix"),
    attr(reference, "model_matrix")
  )
  diag = attr(tables, "coordinate_exchange_diagnostics")$selected
  expect_identical(diag$model_evaluator, "tables")
  expect_identical(diag$model_matrix_calls, 0L)
  expect_identical(diag$model_verification_calls, 1L)
  fallback = run("auto", ce_model_table_max_bytes = 1)
  diag = attr(fallback, "coordinate_exchange_diagnostics")$selected
  expect_identical(diag$model_evaluator, "r")
  expect_match(diag$model_fallback_reason, "limit")
})
