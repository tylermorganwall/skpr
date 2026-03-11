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
