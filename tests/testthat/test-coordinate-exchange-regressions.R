test_that("CE rejects formulas whose model basis depends on the batch", {
  cand = data.frame(x = seq(-1, 1, length.out = 5))
  expect_error(
    gen_design(
      cand,
      ~ x + I((x - mean(x))^2),
      trials = 6,
      repeats = 1,
      progress = FALSE,
      advancedoptions = list(search_method = "coordinate_exchange")
    ),
    "fixed pointwise model"
  )
  expect_error(
    skpr_ce_prepare(cand, ~ x + I(scale(x)^2)),
    "fixed pointwise model"
  )
  transform = function(x) x - mean(x)
  expect_error(
    skpr_ce_prepare(cand, ~ x + I(transform(x)^2)),
    "fixed pointwise model"
  )
  prep = skpr_ce_prepare(cand, ~ x + I(x^2) + I(ifelse(x > 0, x^3, 0)))
  points = as.matrix(cand)
  expect_equal(
    as.vector(prep$modelmatrix_fn(points)[1:2, , drop = FALSE]),
    as.vector(prep$modelmatrix_fn(points[1:2, , drop = FALSE]))
  )
})

test_that("CE checks final model rows for direct callback users", {
  points = matrix(c(-1, 0, 0, 0.5, 1, 1), ncol = 1)
  callback = function(points) {
    model.matrix(~ x + I((x - mean(x))^2), data.frame(x = points[, 1]))
  }
  expect_error(
    genOptimalDesignCoordinateExchangeConstrained(
      points,
      list(seq(-1, 1, length.out = 5)),
      callback,
      list(1L)
    ),
    "fixed pointwise model"
  )
})

test_that("CE freezes scalar constants and rejects recycled or data-dependent vectors", {
  cand = expand.grid(x = c(-1, 0, 1), y = c(-1, 1))
  for (model in list(
    ~ x + I(x %in% c(y)),
    ~ x + I(pmax(x, c(-1, 1))),
    ~ x + I(x:y)
  )) {
    expect_error(skpr_ce_prepare(cand, model), "fixed pointwise model")
  }
  coefficient = 2
  prep = skpr_ce_prepare(cand, ~ x + I(coefficient * x^2) + I(base::abs(y)))
  before = prep$modelmatrix_fn(as.matrix(cand))
  coefficient = 9
  expect_identical(prep$modelmatrix_fn(as.matrix(cand)), before)
  expect_no_error(skpr_ce_prepare(cand, ~.)$modelmatrix_fn(as.matrix(cand)))
  expect_no_error(skpr_ce_prepare(cand, ~ x * y + I(x^2)))
  membership = skpr_ce_prepare(cand, ~ x + I(x %in% c(-1, 1)) + I(x %in% -1:1))
  expect_equal(
    as.vector(membership$modelmatrix_fn(as.matrix(cand))[c(2, 5), ]),
    as.vector(membership$modelmatrix_fn(as.matrix(cand[c(2, 5), ])))
  )
})

test_that("bounded CE initialization spans high-dimensional factor grids", {
  q = 15L
  cand = as.data.frame(rbind(rep(-1, q), rep(1, q)))
  names(cand) = paste0("x", seq_len(q))
  prep = skpr_ce_prepare(cand, reformulate(names(cand)))
  points = matrix(-1, q + 1L, q)
  set.seed(1)
  out = genOptimalDesignCoordinateExchangeConstrained(
    points,
    unname(prep$factor_levels),
    prep$modelmatrix_fn,
    as.list(seq_len(q)),
    max_iter = 0L
  )
  expect_equal(qr(out$model_matrix)$rank, q + 1L)
  expect_equal(out$diagnostics$initialization_mode, "bounded_pool")
  expect_true(all(apply(out$points, 2, function(x) length(unique(x)) == 2L)))

  # Neither a 10-point pool nor the original one-row support can span 16
  # columns. Successful repair must retain directions across multiple pools.
  set.seed(21)
  accumulated = genOptimalDesignCoordinateExchangeConstrained(
    points,
    unname(prep$factor_levels),
    prep$modelmatrix_fn,
    as.list(seq_len(q)),
    max_iter = 0L,
    repair_max_tries = 10L
  )
  expect_gt(accumulated$diagnostics$initialization_restarts, 1L)
  expect_gt(accumulated$diagnostics$initialization_pool_size, 10L)
  expect_equal(qr(accumulated$model_matrix)$rank, q + 1L)

  set.seed(1)
  design = gen_design(
    cand,
    ~.,
    trials = 24,
    repeats = 1,
    progress = FALSE,
    advancedoptions = list(
      search_method = "coordinate_exchange",
      ce_max_iter = 0L,
      constraints = list(filter_expr = quote(x1 + x2 <= 0))
    )
  )
  expect_true(all(design$x1 + design$x2 <= 0))
  expect_equal(qr(attr(design, "model_matrix"))$rank, q + 1L)
})

test_that("exact feasible pools provide reproducible but varied CE starts", {
  cand = expand.grid(x = c(-1, 0, 1), y = c(-1, 0, 1), z = c(-1, 1))
  prep = skpr_ce_prepare(cand, ~ x + z + x:y)
  ir = compile_constraints(
    quote(x + y == 0),
    factor_meta = prep$factor_meta,
    factor_levels = prep$factor_levels
  )
  run = function(seed) {
    set.seed(seed)
    genOptimalDesignCoordinateExchangeConstrained(
      matrix(1, 8, 3),
      unname(prep$factor_levels),
      prep$modelmatrix_fn,
      list(1:2, 3L),
      constraints_ir = ir,
      max_iter = 0L
    )
  }
  results = lapply(1:6, run)
  canonical = lapply(results, function(out) {
    points = as.data.frame(out$points)
    points = points[do.call(order, points), ]
    rownames(points) = NULL
    points
  })
  expect_gt(length(unique(canonical)), 1L)
  expect_identical(run(3)$level_codes, results[[3]]$level_codes)
  for (out in results) {
    expect_equal(out$diagnostics$initialization_mode, "exact_pool")
    expect_equal(qr(out$model_matrix)$rank, 4L)
    expect_true(all(out$points[, 1] + out$points[, 2] == 0))
  }
})

test_that("CE rank and search are invariant to model-column units", {
  cand = data.frame(x = c(-1, 0, 1))
  prep = skpr_ce_prepare(cand, ~ x + I(x^2))
  points = matrix(c(-1, -1, 0, 0, 0, 1), ncol = 1)
  scales = c(1, 1e8, 1e-12)
  scaled_fn = function(points) {
    sweep(prep$modelmatrix_fn(points), 2, scales, `*`)
  }
  base = genOptimalDesignCoordinateExchangeConstrained(
    points,
    unname(prep$factor_levels),
    prep$modelmatrix_fn,
    list(1L)
  )
  scaled = genOptimalDesignCoordinateExchangeConstrained(
    points,
    unname(prep$factor_levels),
    scaled_fn,
    list(1L)
  )
  expect_identical(base$level_codes, scaled$level_codes)
  expect_equal(
    scaled$logdet,
    base$logdet + 2 * sum(log(scales)),
    tolerance = 1e-10
  )
  expect_equal(scaled$diagnostics$final_rank, 3L)
  expect_equal(
    scaled$diagnostics$reciprocal_condition,
    base$diagnostics$reciprocal_condition,
    tolerance = 1e-10
  )

  set.seed(1)
  design = gen_design(
    cand,
    ~ x + I(1e-8 * x^2),
    trials = 6,
    repeats = 1,
    progress = FALSE,
    advancedoptions = list(search_method = "coordinate_exchange")
  )
  mm = attr(design, "model_matrix")
  logdet = as.numeric(determinant(crossprod(mm), logarithm = TRUE)$modulus)
  expect_equal(
    attr(design, "D"),
    100 * exp(logdet / ncol(mm)) / nrow(mm),
    tolerance = 1e-10
  )
  expect_equal(
    attr(design, "coordinate_exchange_diagnostics")$selected$final_rank,
    3L
  )
})

test_that("CE factor updates agree with exact rebuilds and honor their interval", {
  cand = expand.grid(
    x = seq(-1, 1, length.out = 5),
    y = seq(-1, 1, length.out = 5)
  )
  prep = skpr_ce_prepare(cand, ~ x + y + I(x^2) + I(y^2))
  set.seed(19)
  points = as.matrix(cand[sample(nrow(cand), 12, replace = TRUE), ])
  run = function(interval) {
    genOptimalDesignCoordinateExchangeConstrained(
      points,
      unname(prep$factor_levels),
      prep$modelmatrix_fn,
      as.list(1:2),
      recompute_every = interval,
      tolerance = 0
    )
  }
  incremental = run(0L)
  rebuilt = run(1L)
  expect_gt(incremental$diagnostics$factor_updates, 0L)
  expect_equal(incremental$diagnostics$numerical_fallbacks, 0L)
  expect_equal(incremental$diagnostics$information_recomputes, 2L)
  expect_equal(
    rebuilt$diagnostics$information_recomputes,
    2L + rebuilt$diagnostics$accepted_moves
  )
  for (out in list(incremental, rebuilt)) {
    expected = as.numeric(
      determinant(crossprod(out$model_matrix), logarithm = TRUE)$modulus
    )
    expect_equal(out$logdet, expected, tolerance = 1e-9)
    expect_lt(out$diagnostics$logdet_agreement, 1e-8)
    expect_true(all(diff(out$diagnostics$objective_history) >= -1e-10))
    expect_equal(
      as.vector(out$model_matrix),
      as.vector(prep$modelmatrix_fn(out$points))
    )
  }
  expect_equal(incremental$criterion, rebuilt$criterion, tolerance = 1e-8)

  # Row removal would temporarily make a saturated design singular. Adding the
  # proposed row first must still allow the determinant-improving replacement.
  saturated = genOptimalDesignCoordinateExchangeConstrained(
    matrix(c(-1, 0), ncol = 1),
    list(c(-1, 0, 1)),
    function(x) cbind(1, x[, 1]),
    list(1L),
    recompute_every = 0L
  )
  expect_equal(sort(saturated$points[, 1]), c(-1, 1))
  expect_equal(saturated$logdet, log(4), tolerance = 1e-10)
  expect_gt(saturated$diagnostics$factor_updates, 0L)
})

test_that("CE rebuilds unsafe updates for nearly dependent model columns", {
  set.seed(13)
  points = matrix(sample(c(-1, 0, 1), 60, replace = TRUE), ncol = 3)
  epsilon = 1e-9
  callback = function(x) cbind(1, x[, 1], x[, 2], x[, 1] + epsilon * x[, 3])
  out = expect_warning(
    genOptimalDesignCoordinateExchangeConstrained(
      points,
      rep(list(c(-1, 0, 1)), 3),
      callback,
      as.list(1:3),
      recompute_every = 0L
    ),
    "full rank but ill-conditioned"
  )
  expect_gt(out$diagnostics$numerical_fallbacks, 0L)
  expect_equal(
    out$diagnostics$information_recomputes,
    2L + out$diagnostics$numerical_fallbacks
  )
  expect_equal(out$diagnostics$final_rank, 4L)
  expect_equal(out$diagnostics$conditioning_basis, "column_equilibrated")
  # Removing the nearly dependent column combination gives an independent
  # determinant reference without squaring the ill-conditioned model matrix.
  reference = as.numeric(determinant(crossprod(cbind(1, out$points)))$modulus) +
    2 * log(epsilon)
  expect_lt(abs(out$logdet - reference), 1e-5)
  expect_equal(as.vector(out$model_matrix), as.vector(callback(out$points)))
})

test_that("serial CE repeats retain successes before and after a failed repeat", {
  cand = expand.grid(x = c(-1, 0, 1), y = c(-1, 0, 1))
  set.seed(1)
  design = expect_warning(
    gen_design(
      cand,
      ~ x + y,
      trials = 6,
      repeats = 3,
      progress = FALSE,
      advancedoptions = list(
        search_method = "coordinate_exchange",
        .ce_test_fail_repeats = 2L
      )
    ),
    "failed in 1 of 3 repeats"
  )
  outcomes = attr(design, "coordinate_exchange_diagnostics")$repeats
  expect_identical(
    vapply(outcomes, `[[`, character(1), "status"),
    c("ok", "error", "ok")
  )
  expect_match(
    outcomes[[2]]$error_message,
    "injected coordinate-exchange repeat failure"
  )
  expect_true(is.finite(attr(design, "D")))
  expect_error(
    gen_design(
      cand,
      ~ x + y,
      trials = 6,
      repeats = 2,
      progress = FALSE,
      advancedoptions = list(
        search_method = "coordinate_exchange",
        .ce_test_fail_repeats = 1:2
      )
    ),
    "failed in all 2 repeats.*injected coordinate-exchange repeat failure"
  )
})

test_that("bounded neighborhoods report sampled stopping and default k works", {
  points = rbind(rep(-1, 8), rep(1, 8))
  out = genOptimalDesignCoordinateExchangeConstrained(
    points,
    rep(list(c(-1, 1)), 8),
    function(x) cbind(1, x[, 1]),
    list(1:8),
    coordinate_group_max_candidates = 1L
  )
  expect_equal(out$diagnostics$stop_reason, "sampled_no_improvement")
  expect_gt(out$diagnostics$capped_groups, 0L)
  expect_equal(out$diagnostics$accepted_moves, 0L)
  expect_equal(out$logdet, log(4), tolerance = 1e-10)
})
