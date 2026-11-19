test_that("G exchange scores candidates omitted from the final design", {
  grid = data.frame(x = seq(-1, 1, by = 0.5))
  model = ~x + I(x^2)
  points = model.matrix(model, grid)
  # Enumerate all four-run designs, including replicates.
  counts = expand.grid(rep(list(0:4), nrow(grid)))
  counts = as.matrix(counts[rowSums(counts) == 4L, ])
  optimum = min(apply(counts, 1L, function(n) {
    info = crossprod(points, points * n)
    if (qr(info)$rank < ncol(points)) return(Inf)
    max(rowSums((points %*% solve(info)) * points))
  }))
  expect_equal(optimum, 17 / 18)

  for (fixed in list(NULL, data.frame(run = 1:4))) {
    set.seed(5)
    d = gen_design(grid, model, trials = 4, covariates = fixed, optimality = "G",
                   repeats = 10, progress = FALSE)
    expect_equal(sort(d$x), c(-1, -0.5, 0.5, 1))
    inverse = solve(crossprod(model.matrix(model, d)))
    variances = rowSums((points %*% inverse) * points)
    expect_equal(unname(variances[grid$x == 0]), optimum)
    expect_lt(max(variances[grid$x %in% d$x]), optimum)
    efficiency = 100 * ncol(points) / (4 * optimum)
    expect_equal(get_optimality(d, "G")$G, efficiency)
    expect_equal(max(attr(d, "optimalsearchvalues")), efficiency)

    # An explicit diagnostic region changes the report only.
    set.seed(5)
    report = gen_design(grid, model, trials = 4, covariates = fixed, optimality = "G",
                        repeats = 10, progress = FALSE, advancedoptions = list(
                          g_efficiency_method = "custom",
                          g_efficiency_samples = grid[grid$x != 0, , drop = FALSE]
                        ))
    expect_equal(sort(report$x), sort(d$x))
    expect_equal(attr(report, "optimalsearchvalues"), attr(d, "optimalsearchvalues"))
    expect_equal(get_optimality(report, "G")$G, 100 * ncol(points) / (4 * max(variances[grid$x != 0])))
  }
})

test_that("G scoring retains covariate profiles used only in an interaction", {
  grid = data.frame(x = -1:1)
  fixed = data.frame(position = c(1, 1, 1, 1, 3, 3))
  model = ~0 + x:position
  set.seed(1)
  design = gen_design(grid, model, covariates = fixed, optimality = "G",
                      repeats = 10, progress = FALSE)
  expect_identical(design$position, fixed$position)
  expect_equal(abs(design$x), rep(1, 6))

  # For this one-coefficient model the variance is (x * position)^2 / information.
  # The largest numerator is 9 at the second profile, and the information is 22.
  worst = 9 / 22
  expect_equal(get_optimality(design, "G")$G, 100 / (6 * worst))
  expect_equal(max(attr(design, "optimalsearchvalues")), 100 / (6 * worst))
})
