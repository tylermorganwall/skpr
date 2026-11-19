test_that("failed Wald tests return one missing p-value per tested term", {
  for (formula in list(breaks ~ wool * tension, breaks ~ 0 + wool * tension)) {
    fit = glm(formula, warpbreaks, family = poisson())
    for (type in c("II", "III")) {
      expected = rownames(car::Anova(fit, type = type, test.statistic = "Wald"))
      failed = effectpowermc(fit, type = type, effect_anova = TRUE, glmfamily = "poisson",
                             test.statistic = "Wald", vcov. = 0 * vcov(fit))
      expect_identical(names(failed), expected)
      expect_true(all(is.na(failed)))
    }
  }
})

test_that("an unavailable effect test keeps its term columns in Monte Carlo power", {
  skip_if_not_installed("mbest")
  skip_if_not_installed("lmtest")
  previous_plan = future::plan(future::sequential)
  on.exit(future::plan(previous_plan), add = TRUE)
  # Exercise the same error on the second fit in both execution paths.
  anova = car::Anova
  local_mocked_bindings(Anova = function(mod, ...) {
    if (model.response(model.frame(mod))[1L] == 99) {
      return(anova(mod, ..., vcov. = 0 * as.matrix(vcov(mod))))
    }
    anova(mod, ...)
  }, .package = "car")

  design = expand.grid(Location = factor(c("East", "West")), Climate = factor(c("Dry", "Wet", "Arid")))
  design = design[rep(seq_len(nrow(design)), 2L), ]
  rownames(design) = paste(rep(1:6, each = 2L), 1:12, sep = ".")
  attr(design, "candidate_set") = unique(design)
  for (parallel in c(FALSE, TRUE)) {
    i = 0L
    simulate_counts = function(X, b, blockvector) {
      i <<- i + 1L
      counts = c(1, 4, 2, 3, 16, 8, 6, 20, 3, 7, 3, 4)
      if (i == 2L) counts[1L] = 99
      counts
    }
    result = eval_design_mc(design, ~., 0.2, nsim = 2, blocking = TRUE, glmfamily = "poisson",
                            effectsize = c(1, 10), rfunction = simulate_counts,
                            parallel = parallel, progress = FALSE)
    p = attr(result, "effect_pvals")
    expect_identical(colnames(p), c("(Intercept)", "Location", "Climate"))
    expect_true(all(is.finite(p[1, ])))
    expect_true(all(is.na(p[2, ])))
    expect_equal(result$power[result$type == "effect.power.mc"], as.numeric(p[1, ] < 0.2) / 2)
    expect_equal(ncol(attr(result, "pvals")), 4L)
  }
})
