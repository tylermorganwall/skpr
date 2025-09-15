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
