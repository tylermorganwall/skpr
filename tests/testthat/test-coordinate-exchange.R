test_that("coordinate exchange engine runs end-to-end with decode", {
  cand = expand.grid(
    x = c(-1, 0, 1),
    z = factor(c("A", "B")),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  cand$z = factor(cand$z)
  cand_norm = normalize_design(cand)

  prep = skpr_ce_prepare(cand_norm, ~x + z + x:z, contrasts_fun = contr.simplex)
  factor_meta = prep$factor_meta
  factor_levels = prep$factor_levels

  set.seed(123)
  n = 6
  points0 = matrix(0, nrow = n, ncol = length(factor_levels))
  colnames(points0) = names(factor_levels)
  for (j in seq_along(factor_levels)) {
    points0[, j] = sample(as.numeric(factor_levels[[j]]), n, replace = TRUE)
  }

  out = genOptimalDesignCoordinateExchangeConstrained(
    points = points0,
    factor_levels = unname(factor_levels),
    modelmatrix_fn = prep$modelmatrix_fn,
    factor_columns = prep$factor_columns,
    tolerance = 1e-5,
    kexchange = n,
    augmentedrows = 0L
  )

  decoded = skpr_ce_decode_points(out$points, factor_meta)
  expect_s3_class(decoded, "data.frame")
  expect_true(is.numeric(decoded$x))
  expect_true(is.factor(decoded$z))
  expect_false(anyNA(decoded))
  expect_true(is.finite(out$criterion))
})

test_that("gen_design CE search is comparable to default D-optimal search", {
  cand = expand.grid(
    x = c(-1, 0, 1),
    z = factor(c("A", "B")),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  cand$z = factor(cand$z)

  set.seed(1)
  design_fedorov = gen_design(
    candidateset = cand,
    model = ~x + z + x:z,
    trials = 6,
    repeats = 40,
    optimality = "D",
    progress = FALSE
  )

  set.seed(1)
  design_ce = gen_design(
    candidateset = cand,
    model = ~x + z + x:z,
    trials = 6,
    repeats = 40,
    optimality = "D",
    progress = FALSE,
    advancedoptions = list(
      search_method = "coordinate_exchange"
    )
  )

  expect_true(is.factor(design_ce$z))
  expect_true(is.finite(attr(design_ce, "D")))
  expect_true(attr(design_ce, "D") >= (attr(design_fedorov, "D") - 5))
  expect_false(is.na(get_optimality(design_ce, "D")[[1]]))
})

test_that("CE in gen_design enforces explicit constraints for sparse candidate sets", {
  cand_sparse = subset(
    expand.grid(
      x = c(-1, 1),
      a = factor(c("A", "B")),
      b = factor(c("X", "Y")),
      KEEP.OUT.ATTRS = FALSE
    ),
    !(a == "B" & b == "Y")
  )
  cand_sparse$a = factor(cand_sparse$a)
  cand_sparse$b = factor(cand_sparse$b)

  expect_error(
    gen_design(
      candidateset = cand_sparse,
      model = ~x + a + b + a:b,
      trials = 6,
      repeats = 10,
      optimality = "D",
      progress = FALSE,
      advancedoptions = list(search_method = "coordinate_exchange")
    ),
    "CE requires explicit constraints"
  )

  expect_no_error(
    gen_design(
      candidateset = cand_sparse,
      model = ~x + a + b,
      trials = 6,
      repeats = 10,
      optimality = "D",
      progress = FALSE,
      advancedoptions = list(
        search_method = "coordinate_exchange",
        constraints = list(
          filter_expr = quote(!(a == "B" & b == "Y"))
        )
      )
    )
  )
})

test_that("CE output retains standard gen_design attributes and guardrails", {
  cand = expand.grid(
    x = c(-1, 0, 1),
    z = factor(c("A", "B")),
    KEEP.OUT.ATTRS = FALSE
  )
  cand$z = factor(cand$z)

  design_ce = gen_design(
    candidateset = cand,
    model = ~x + z + x:z,
    trials = 6,
    repeats = 10,
    optimality = "D",
    progress = FALSE,
    advancedoptions = list(search_method = "coordinate_exchange")
  )

  expect_true(!is.null(attr(design_ce, "model_matrix")))
  expect_true(!is.null(attr(design_ce, "model_matrix_cor")))
  expect_identical(attr(design_ce, "generating.criterion"), "D")
  expect_true(is.finite(get_optimality(design_ce, "D")[[1]]))

  expect_error(
    gen_design(
      candidateset = cand,
      model = ~x + z + x:z,
      trials = 6,
      repeats = 5,
      optimality = "A",
      progress = FALSE,
      advancedoptions = list(search_method = "coordinate_exchange")
    ),
    "supports only optimality = 'D'"
  )

  expect_error(
    gen_design(
      candidateset = cand,
      model = ~x + z + x:z,
      trials = 6,
      blocksizes = 2,
      repeats = 5,
      optimality = "D",
      progress = FALSE,
      advancedoptions = list(search_method = "coordinate_exchange")
    ),
    "fully randomized, non-blocked designs"
  )
})

test_that("CE repairs singular near-saturated starts before inversion", {
  cand = expand.grid(
    x = c(-1, 1),
    y = c(-1, 1),
    z = c(-1, 1),
    KEEP.OUT.ATTRS = FALSE
  )
  prep = skpr_ce_prepare(cand, ~x * y * z, contrasts_fun = contr.simplex)

  points0 = matrix(
    -1,
    nrow = 8,
    ncol = length(prep$factor_levels),
    dimnames = list(NULL, names(prep$factor_levels))
  )

  out = genOptimalDesignCoordinateExchangeConstrained(
    points = points0,
    factor_levels = unname(prep$factor_levels),
    modelmatrix_fn = prep$modelmatrix_fn,
    factor_columns = prep$factor_columns,
    tolerance = 1e-5,
    kexchange = 8L,
    augmentedrows = 0L
  )

  expect_true(is.finite(out$criterion))
  expect_equal(qr(out$model_matrix)$rank, ncol(out$model_matrix))
})

test_that("CE repairs constrained singular and infeasible starts", {
  cand = expand.grid(
    x = c(-1, 0, 1),
    y = c(-1, 0, 1),
    z = factor(c("A", "B")),
    KEEP.OUT.ATTRS = FALSE
  )
  cand$z = factor(cand$z)
  prep = skpr_ce_prepare(cand, ~x + y + z + x:z + y:z, contrasts_fun = contr.simplex)
  ir = compile_constraints(
    filter_expr = quote(x + y <= 0),
    factor_meta = prep$factor_meta,
    factor_levels = prep$factor_levels
  )

  points0 = matrix(
    rep(c(1, 1, 0), 8),
    nrow = 8,
    ncol = 3,
    byrow = TRUE,
    dimnames = list(NULL, names(prep$factor_levels))
  )

  out = genOptimalDesignCoordinateExchangeConstrained(
    points = points0,
    factor_levels = unname(prep$factor_levels),
    modelmatrix_fn = prep$modelmatrix_fn,
    factor_columns = prep$factor_columns,
    constraints_ir = ir,
    tolerance = 1e-5,
    kexchange = 8L,
    augmentedrows = 0L
  )

  decoded = skpr_ce_decode_points(out$points, prep$factor_meta)
  expect_false(isTRUE(out$any_infeasible_remaining))
  expect_true(all(decoded$x + decoded$y <= 1e-12))
  expect_true(is.finite(out$criterion))
  expect_equal(qr(out$model_matrix)$rank, ncol(out$model_matrix))
})

test_that("CE factor-column detection is deterministic and catches interactions", {
  cand = expand.grid(
    x = c(-1, 0, 1),
    y = c(-1, 0, 1),
    z = c(-1, 0, 1),
    KEEP.OUT.ATTRS = FALSE
  )
  prep = skpr_ce_prepare(cand, ~x * y * z, contrasts_fun = contr.simplex)
  mm = prep$modelmatrix_fn(matrix(c(-1, -1, -1), nrow = 1))
  mm_cols = colnames(mm)
  term_has = function(term) {
    vapply(
      strsplit(mm_cols, ":", fixed = TRUE),
      function(parts) term %in% parts,
      logical(1)
    )
  }

  for (nm in c("x", "y", "z")) {
    expected = which(term_has(nm))
    observed = prep$factor_columns[[nm]]
    expect_true(all(expected %in% observed), info = nm)
  }

  three_way = match("x:y:z", mm_cols)
  expect_true(three_way %in% prep$factor_columns$x)
  expect_true(three_way %in% prep$factor_columns$y)
  expect_true(three_way %in% prep$factor_columns$z)
})

test_that("CE setup does not reset or consume global RNG state", {
  cand = expand.grid(
    x = c(-1, 0, 1),
    y = c(-1, 0, 1),
    z = c(-1, 0, 1),
    KEEP.OUT.ATTRS = FALSE
  )

  set.seed(42)
  before = .Random.seed
  invisible(skpr_ce_prepare(cand, ~x * y * z, contrasts_fun = contr.simplex))
  expect_identical(.Random.seed, before)
})

test_that("CE user-level random starts are not forced through an internal seed", {
  cand = expand.grid(
    x = seq(10, 30, by = 5),
    y = seq(100, 160, by = 10),
    KEEP.OUT.ATTRS = FALSE
  )

  set.seed(100)
  design_100 = gen_design(
    candidateset = cand,
    model = ~x + y,
    trials = 10,
    repeats = 1,
    optimality = "D",
    progress = FALSE,
    advancedoptions = list(
      search_method = "coordinate_exchange",
      ce_max_iter = 0L
    )
  )

  set.seed(200)
  design_200 = gen_design(
    candidateset = cand,
    model = ~x + y,
    trials = 10,
    repeats = 1,
    optimality = "D",
    progress = FALSE,
    advancedoptions = list(
      search_method = "coordinate_exchange",
      ce_max_iter = 0L
    )
  )

  expect_false(identical(as.data.frame(design_100), as.data.frame(design_200)))
})

test_that("CE factor_levels_original uses original units for constraints and output", {
  cand = expand.grid(
    temp = c(25, 50, 75),
    pressure = c(1, 3, 5),
    KEEP.OUT.ATTRS = FALSE
  )

  set.seed(11)
  design = gen_design(
    candidateset = cand,
    model = ~temp + pressure + temp:pressure,
    trials = 6,
    repeats = 10,
    optimality = "D",
    progress = FALSE,
    advancedoptions = list(
      search_method = "coordinate_exchange",
      factor_levels_original = list(
        temp = c(25, 50, 75),
        pressure = c(1, 3, 5)
      ),
      constraints = list(
        filter_expr = quote(temp + 10 * pressure <= 100)
      )
    )
  )

  expect_true(all(design$temp %in% c(25, 50, 75)))
  expect_true(all(design$pressure %in% c(1, 3, 5)))
  expect_true(all(design$temp + 10 * design$pressure <= 100 + 1e-8))
  expect_true(max(design$temp) > 1)
  expect_equal(qr(attr(design, "model_matrix"))$rank, ncol(attr(design, "model_matrix")))

  expect_error(
    gen_design(
      candidateset = cand,
      model = ~temp + pressure,
      trials = 4,
      repeats = 1,
      optimality = "D",
      progress = FALSE,
      advancedoptions = list(
        search_method = "coordinate_exchange",
        factor_levels = list(temp = c(-1, 0, 1)),
        factor_levels_original = list(temp = c(25, 50, 75))
      )
    ),
    "Supply only one"
  )
})

test_that("CE parallel repeats preserve partial and all-failure diagnostics", {
  old_plan = future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)

  cand = expand.grid(
    x = c(-1, 0, 1),
    y = c(-1, 0, 1),
    KEEP.OUT.ATTRS = FALSE
  )

  design = expect_warning(
    gen_design(
      candidateset = cand,
      model = ~x + y,
      trials = 5,
      repeats = 3,
      optimality = "D",
      parallel = TRUE,
      progress = FALSE,
      advancedoptions = list(
        search_method = "coordinate_exchange",
        .ce_test_fail_repeats = 1L
      )
    ),
    "failed in 1 of 3 repeats"
  )
  expect_true(is.finite(attr(design, "D")))
  expect_equal(qr(attr(design, "model_matrix"))$rank, ncol(attr(design, "model_matrix")))

  expect_error(
    gen_design(
      candidateset = cand,
      model = ~x + y,
      trials = 5,
      repeats = 2,
      optimality = "D",
      parallel = TRUE,
      progress = FALSE,
      advancedoptions = list(
        search_method = "coordinate_exchange",
        .ce_test_fail_repeats = 1:2
      )
    ),
    "failed in all 2 repeats.*injected coordinate-exchange repeat failure"
  )
})

test_that("CE k-row selection chooses monotone lowest-leverage sets", {
  x = seq(-1, 1, length.out = 12)
  y = rep(c(-1, 1), length.out = 12)
  X = cbind("(Intercept)" = 1, x = x, y = y)
  V = solve(t(X) %*% X)
  leverage = diag(X %*% V %*% t(X))

  expected_rows = function(k) {
    ord = order(leverage, seq_along(leverage))
    cutoff = leverage[ord[k]]
    which(leverage <= cutoff + 1e-12)
  }

  rows_k1 = skpr_ce_select_rows_by_leverage(X, V, kexchange = 1L)
  rows_k2 = skpr_ce_select_rows_by_leverage(X, V, kexchange = 2L)
  rows_k3 = skpr_ce_select_rows_by_leverage(X, V, kexchange = 3L)

  expect_equal(rows_k1, expected_rows(1L))
  expect_equal(rows_k2, expected_rows(2L))
  expect_equal(rows_k3, expected_rows(3L))
  expect_true(all(rows_k1 %in% rows_k2))
  expect_true(all(rows_k2 %in% rows_k3))
})
