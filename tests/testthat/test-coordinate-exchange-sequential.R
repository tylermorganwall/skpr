test_that("CE commits each coordinate before expanding the next interaction", {
  grid = expand.grid(x = c(-1, 0, 1), y = c(-1, 0, 1))
  prep = skpr_ce_prepare(grid, ~ x * y)
  groups = skpr_ce_resolve_coordinate_groups(names(grid))$coordinate_groups
  expect_identical(groups, list(1L, 2L))
  # Four fixed corner runs give X'X = 4I. The mutable center run should move
  # (0, 0) -> (-1, 0) -> (-1, -1), with determinants 320 -> 384 -> 512.
  points = rbind(as.matrix(expand.grid(x = c(-1, 1), y = c(-1, 1))), c(0, 0))
  batches = list()
  callback = function(points) {
    batches[[length(batches) + 1L]] <<- points
    prep$modelmatrix_fn(points)
  }
  out = genOptimalDesignCoordinateExchangeConstrained(
    points,
    unname(prep$factor_levels),
    callback,
    groups,
    augmentedrows = 4L,
    kexchange = 1L,
    max_iter = 1L,
    recompute_every = 0L,
    tolerance = 0
  )

  expect_equal(out$points[1:4, ], unname(points[1:4, ]))
  expect_equal(out$points[5, ], c(-1, -1))
  expect_equal(out$diagnostics$accepted_moves, 2L)
  expect_equal(exp(out$diagnostics$objective_history), c(320, 384, 512))
  expect_length(batches, 4L)
  expect_equal(batches[[2L]], rbind(c(-1, 0), c(1, 0)))
  expect_equal(batches[[3L]], rbind(c(-1, -1), c(-1, 1)))
  expect_equal(
    as.vector(out$model_matrix),
    as.vector(prep$modelmatrix_fn(out$points))
  )
  expect_equal(out$model_matrix[5, ], c(1, -1, -1, 1))
  # Rebuild the complete information matrix independently of incremental scoring.
  expected = as.numeric(determinant(crossprod(out$model_matrix))$modulus)
  expect_equal(out$logdet, expected, tolerance = 1e-10)
  expect_equal(out$diagnostics$information_recomputes, 2L)
})

test_that("CE continues to the next coordinate when a group cannot improve", {
  points = rbind(as.matrix(expand.grid(x = c(-1, 1), y = c(-1, 1))), c(-1, 0))
  out = genOptimalDesignCoordinateExchangeConstrained(
    points,
    rep(list(c(-1, 0, 1)), 2),
    function(x) cbind(1, x[, 1], x[, 2], x[, 1] * x[, 2]),
    list(1L, 2L),
    augmentedrows = 4L,
    kexchange = 1L,
    max_iter = 1L,
    tolerance = 0
  )
  expect_equal(out$points[5, ], c(-1, -1))
  expect_equal(out$points[1:4, ], unname(points[1:4, ]))
  expect_equal(out$diagnostics$accepted_moves, 1L)
  expect_equal(exp(out$diagnostics$objective_history), c(384, 512))
})

test_that("CE commits a constrained group before searching an independent factor", {
  grid = expand.grid(x = c(-1, 0, 1), y = c(-1, 0, 1), z = c(-1, 0, 1))
  prep = skpr_ce_prepare(grid, ~ x * z)
  ir = compile_constraints(
    quote(x + y == 0),
    factor_meta = prep$factor_meta,
    factor_levels = prep$factor_levels
  )
  groups = skpr_ce_resolve_coordinate_groups(names(grid), ir)$coordinate_groups
  expect_identical(groups, list(1:2, 3L))
  corners = as.matrix(expand.grid(x = c(-1, 1), z = c(-1, 1)))
  points = rbind(cbind(corners[, 1], -corners[, 1], corners[, 2]), c(0, 0, 0))
  batches = list()
  callback = function(points) {
    batches[[length(batches) + 1L]] <<- points
    prep$modelmatrix_fn(points)
  }
  out = genOptimalDesignCoordinateExchangeConstrained(
    points,
    unname(prep$factor_levels),
    callback,
    groups,
    constraints_ir = ir,
    augmentedrows = 4L,
    kexchange = 1L,
    max_iter = 1L,
    recompute_every = 1L,
    tolerance = 0
  )
  expect_equal(out$points[1:4, ], points[1:4, ])
  expect_equal(out$points[5, ], c(-1, 1, -1))
  expect_equal(out$diagnostics$accepted_moves, 2L)
  expect_equal(exp(out$diagnostics$objective_history), c(320, 384, 512))
  expect_length(batches, 4L)
  expect_equal(batches[[2L]], rbind(c(-1, 1, 0), c(1, -1, 0)))
  expect_equal(batches[[3L]], rbind(c(-1, 1, -1), c(-1, 1, 1)))
  expect_true(all(out$points[, 1] + out$points[, 2] == 0))
  expect_equal(out$model_matrix[5, ], c(1, -1, -1, 1))
  expect_equal(out$diagnostics$information_recomputes, 4L)
})
