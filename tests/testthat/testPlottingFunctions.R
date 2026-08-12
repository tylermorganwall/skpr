library(lme4)

context("plottingFunctions")

set.seed(1)

test_that("plot_correlations works as intended", {
  candlist = expand.grid(
    Location = as.character(c("East", "West")),
    Climate = as.factor(c("Dry", "Wet", "Arid")),
    Vineyard = as.factor(c("A", "B", "C", "D")),
    Age = c(1, -1)
  )
  design = gen_design(candlist, ~., 23)

  expect_silent(plot_correlations(design))

  expect_silent(plot_correlations(
    design,
    model = ~ Location + Climate + Vineyard + Age + I(Age^2)
  ))

  expect_silent(plot_correlations(design, pow = 3))

  expect_silent(plot_correlations(
    design,
    customcolors = c("black", "grey", "white")
  ))

  expect_warning(
    {
      plot_correlations(
        eval_design(candlist, ~., 0.2),
        customcolors = c("black", "grey", "white")
      )
    },
    "high_resolution_candidate_set"
  )

  expect_silent(plot_correlations(
    eval_design(design, ~., 0.2),
    customcolors = c("black", "grey", "white")
  ))
  expect_silent(plot_correlations(
    eval_design(design, ~., 0.2),
    model = ~ . + Location:Climate,
    customcolors = c("black", "grey", "white")
  ))
  expect_warning(
    plot_correlations(
      eval_design(design, ~., 0.2),
      model = ~ . + Location:Climate:Vineyard,
      customcolors = c("black", "grey", "white")
    ),
    "dubious"
  )
})

test_that("plot_correlations tile values match returned correlation matrix", {
  design = data.frame(
    a = c(-1, 1, 1, -1, -1, -1, 1),
    b = c(-1, 1, 1, 1, -1, 1, -1),
    c = c(1, -1, 1, -1, -1, 1, 0)
  )
  attr(design, "variance.matrix") = diag(nrow(design))

  model = ~ a + b + c + a:b + a:c
  cormat = plot_correlations(design, model = model, plot = FALSE)
  expect_silent(plot_correlations(design, model = model))

  labels = colnames(cormat)
  expected_plot_df = expand.grid(
    x = labels,
    y = rev(labels),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  expected_plot_df$value = as.vector(cormat[labels, rev(labels), drop = FALSE])

  plot_df = ggplot2::last_plot()$data
  expect_equal(plot_df[, c("x", "y", "value")], expected_plot_df)
  expect_gt(
    plot_df$value[plot_df$x == "c" & plot_df$y == "a:c"],
    plot_df$value[plot_df$x == "a" & plot_df$y == "c"]
  )
})

test_that("plot_fds works as intended", {
  candlist = expand.grid(
    Location = as.character(c("East", "West")),
    Climate = as.factor(c("Dry", "Wet", "Arid")),
    Vineyard = as.factor(c("A", "B", "C", "D")),
    Age = c(1, -1)
  )
  design = gen_design(candlist, ~., 23)

  expect_silent(plot_fds(design))

  expect_silent(plot_fds(design, continuouslength = 3))

  expect_silent(plot_fds(
    design,
    model = ~ Location + Climate + Vineyard + Age + Age:Vineyard
  ))

  expect_silent(plot_fds(eval_design(design, ~., 0.2)))

  expect_warning({
    cand_evaled = eval_design(candlist, ~., 0.2)
  })
  expect_error(
    plot_fds(cand_evaled),
    "If design was not originally generated with skpr"
  )
})
