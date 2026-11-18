test_that("plot_correlations includes generating-model quadratic terms by default", {
  design = expand.grid(a = c(-1, 0, 1), b = c(-1, 0, 1))
  model = ~ a + b + I(a^2) + I(b^2) + a:b
  attr(design, "generating_model") = model
  attr(design, "variance.matrix") = diag(nrow(design))

  cormat = plot_correlations(design, plot = FALSE)
  expect_equal(
    cormat,
    plot_correlations(design, model = model, plot = FALSE)
  )
  expect_true(all(c("I(a^2)", "I(b^2)") %in% colnames(cormat)))
  expect_identical(
    colnames(plot_correlations(design, model = ~ a + b, plot = FALSE)),
    c("a", "b")
  )
})
