context("testeffectsizeProcessing")

test_that("providing anticoef and effectsize to eval_design produces a warning", {
  des = expand.grid(x = c(-1, 1), y = 10:11)
  expect_warning(eval_design(des, ~., effectsize = 2, anticoef = c(1, 1, 1), alpha = 0.2))
})



test_that("providing anticoef and effectsize to eval_design_mc produces a warning", {
  des = expand.grid(x = c(-1, 1), y = 10:11)
  expect_warning(eval_design_mc(des, ~., effectsize = 2, anticoef = c(1, 1, 1), alpha = 0.2, nsim = 1))
})

test_that("providing anticoef and effectsize to eval_design_survival_mc produces a warning", {
  des = expand.grid(x = c(-1, 1), y = 10:11)
  expect_warning(eval_design_survival_mc(des, ~., effectsize = 2, anticoef = c(1, 1, 1), alpha = 0.2, nsim = 1))
})


test_that("binomial coefficient generation is numerically correct", {
  acoef = c(1, 1) #default anticoef for a single continuous factor
  for (p_low in seq(0.01, 0.99, length.out = 10)) {
    for (p_high in seq(0.02, 0.95, length.out = 10)) {
      bcoef = skpr:::gen_binomial_anticoef(acoef, p_low, p_high)
      p_low_calculated = 1 / (1 + exp(-(bcoef[1] - bcoef[2])))
      p_high_calculated = 1 / (1 + exp(-(bcoef[1] + bcoef[2])))

      expect_equal(p_low, p_low_calculated, tolerance = 1e-8)
      expect_equal(p_high, p_high_calculated, tolerance = 1e-8)
    }
  }
})



test_that("binomial coefficient generation rejects invalid probabilities", {
  acoef = c(1, 1)
  #negative
  expect_error(skpr:::gen_binomial_anticoef(acoef, -0.5, -0.3))
  expect_error(skpr:::gen_binomial_anticoef(acoef, -0.5, 0.3))
  expect_error(skpr:::gen_binomial_anticoef(acoef, 0.5, -0.3))
  #> 1
  expect_error(skpr:::gen_binomial_anticoef(acoef, 0.2, 1.1))
  expect_error(skpr:::gen_binomial_anticoef(acoef, 1.1, 0.2))
  expect_error(skpr:::gen_binomial_anticoef(acoef, 1.1, 1.2))
  #wrong order - now ok!
  expect_silent(skpr:::gen_binomial_anticoef(acoef, 0.9, 0.1))
})



test_that("exponential coefficient generation is numerically correct", {
  acoef = c(1, 1)
  lowval = 3
  highval = 5
  bcoef = skpr:::gen_exponential_anticoef(acoef, lowval, highval)
  lowval_calc = exp((bcoef[1] - bcoef[2]))
  highval_calc = exp((bcoef[1] + bcoef[2]))
  expect_equal(lowval, lowval_calc, tolerance = 1e-8)
  expect_equal(highval, highval_calc, tolerance = 1e-8)

})

test_that("exponential coefficient generation rejects invalid parameters", {
  acoef = c(1, 1)
  #negative
  expect_error(skpr:::gen_exponential_anticoef(acoef, -0.5, -0.3),
               "Exponential anticipated coefficients generation error: mean values must be positive.")
  expect_error(skpr:::gen_exponential_anticoef(acoef, -0.5, 0.3),
               "Exponential anticipated coefficients generation error: mean values must be positive.")
  expect_error(skpr:::gen_exponential_anticoef(acoef, 0.5, -0.3),
               "Exponential anticipated coefficients generation error:")
  expect_error(skpr:::gen_exponential_anticoef(acoef, -0.3, -0.5),
               "Exponential anticipated coefficients generation error:")
  expect_error(skpr:::gen_exponential_anticoef(acoef, -0.3, 0.5),
               "Exponential anticipated coefficients generation error:")
  expect_error(skpr:::gen_exponential_anticoef(acoef, 0.3, -0.5),
               "Exponential anticipated coefficients generation error:")

  #wrong order - now ok!
  expect_silent(skpr:::gen_exponential_anticoef(acoef, 0.4, 0.3))
})


test_that("poisson coefficient generation is numerically correct", {
  acoef = c(1, 1)
  for (lowcount in 1:5) {
    for (highcount in seq(1.1, 5.1, length.out = 5)) {
      bcoef = skpr:::gen_poisson_anticoef(acoef, lowcount, highcount)
      lowcount_calc = exp(bcoef[1] - bcoef[2])
      highcount_calc = exp(bcoef[1] + bcoef[2])
      expect_equal(lowcount, lowcount_calc, tolerance = 1e-8)
      expect_equal(highcount, highcount_calc, tolerance = 1e-8)
    }
  }
})

test_that("poisson coefficient generation rejects invalid parameters", {
  acoef = c(1, 1)
  #negative
  expect_error(skpr:::gen_poisson_anticoef(acoef, -0.5, -0.3),
               "anticipated coefficients generation error:")
  expect_error(skpr:::gen_poisson_anticoef(acoef, -0.5, 0.3),
               "anticipated coefficients generation error:")
  expect_error(skpr:::gen_poisson_anticoef(acoef, 0.5, -0.3),
               "anticipated coefficients generation error:")
  expect_error(skpr:::gen_poisson_anticoef(acoef, -0.3, -0.5),
               "anticipated coefficients generation error:")
  expect_error(skpr:::gen_poisson_anticoef(acoef, -0.3, 0.5),
               "anticipated coefficients generation error:")
  expect_error(skpr:::gen_poisson_anticoef(acoef, 0.3, -0.5),
               "anticipated coefficients generation error:")

  #wrong order - now ok!
  expect_silent(skpr:::gen_poisson_anticoef(acoef, 0.4, 0.3))
})



test_that("eval_design_mc processes effectsize properly for glm", {
  cand = expand.grid(x = c(-1, 1), y = c(-1, 1))
  des = gen_design(cand, ~., trials = 100)

  #length = 1 effectsize, warning except for gaussian:
  expect_silent(
    res1 <- eval_design_mc(des, ~., glmfamily = 'gaussian', effectsize = 5,
                           alpha = 0.2, nsim = 1, detailedoutput = TRUE))
  expect_equal(res1$anticoef, c(2.5, 2.5, 2.5), tolerance = 1e-8)

  expect_warning(res2 <- eval_design_mc(des, ~., glmfamily = 'exponential', effectsize = 3,
                           alpha = 0.2, nsim = 1, detailedoutput = TRUE),
               "default or length 1 delta used with glmfamily == 'exponential'. This can lead to unrealistic effect sizes - make sure the generated anticipated coeffcients are appropriate.")
  expect_equal(res2$anticoef, c(1.5, 1.5, 1.5))

  expect_warning(
    res3 <- eval_design_mc(des, ~., glmfamily = 'poisson', effectsize = 1,
                           alpha = 0.2, nsim = 1, detailedoutput = TRUE),
               "default or length 1 delta used with glmfamily == 'poisson'. This can lead to unrealistic effect sizes - make sure the generated anticipated coeffcients are appropriate.")
  expect_equal(res3$anticoef, c(0.5, 0.5, 0.5))

  expect_warning(
    res4 <- eval_design_mc(des, ~., glmfamily = 'binomial', effectsize = 2,
                           alpha = 0.2, nsim = 1, detailedoutput = TRUE),
               "default or length 1 delta used with glmfamily == 'binomial'. This can lead to unrealistic effect sizes - make sure the generated anticipated coeffcients are appropriate")
  expect_equal(res4$anticoef, c(1, 1, 1))

  #length = 2 effectsize, works in all cases:
  expect_silent(
    res5 <- eval_design_mc(des, ~., glmfamily = 'binomial', effectsize = c(0.6, 0.8),
                           alpha = 0.2, nsim = 1, detailedoutput = TRUE))
  expect_equal(res5$anticoef, skpr:::gen_binomial_anticoef(c(1,1,1), 0.6, 0.8))

  expect_silent(
    res6 <- eval_design_mc(des, ~., glmfamily = 'exponential', effectsize = c(3, 5),
                           alpha = 0.2, nsim = 1, detailedoutput = TRUE))
  expect_equal(res6$anticoef, skpr:::gen_exponential_anticoef(c(1,1,1), 3, 5))

  expect_silent(
    res7 <- eval_design_mc(des, ~., glmfamily = 'poisson', effectsize = c(5.2, 8.3),
                           alpha = 0.2, nsim = 1, detailedoutput = TRUE))
  expect_equal(res7$anticoef, skpr:::gen_poisson_anticoef(c(1,1,1), 5.2, 8.3))

  expect_silent(
    res8 <- eval_design_mc(des, ~., glmfamily = 'gaussian', effectsize = c(5, 8),
                           alpha = 0.2, nsim = 1, detailedoutput = TRUE))
  expect_equal(res8$anticoef, c(1.5, 1.5, 1.5))

})

test_that("eval_design_survival_mc processes effectsize properly", {
  #TODO: fix this
  cand = expand.grid(x = c(-1, 1), y = c(-1, 1))
  des = gen_design(cand, ~., trials = 100)

  #length = 1 effectsize, warning except for gaussian:
  expect_silent(
    res1 <- eval_design_survival_mc(des, ~., distribution = 'gaussian', effectsize = 5,
                           alpha = 0.2, nsim = 1, detailedoutput = TRUE))
  expect_equal(res1$anticoef, c(2.5, 2.5, 2.5), tolerance = 1e-8)

  expect_warning(
    res2 <- eval_design_survival_mc(des, ~., distribution = 'lognormal', effectsize = 5,
                                    alpha = 0.2, nsim = 1, detailedoutput = TRUE),
    "default or length 1 delta used with distribution == 'lognormal'. This can lead to unrealistic effect sizes - make sure the generated anticipated coeffcients are appropriate.")
  expect_equal(res2$anticoef, c(2.5, 2.5, 2.5), tolerance = 1e-8)

  expect_warning(
    res3 <- eval_design_survival_mc(des, ~., distribution = 'exponential', effectsize = 4,
                                    alpha = 0.2, nsim = 1, detailedoutput = TRUE),
    "default or length 1 delta used with distribution == 'exponential'. This can lead to unrealistic effect sizes - make sure the generated anticipated coeffcients are appropriate.")
  expect_equal(res3$anticoef, c(2, 2, 2), tolerance = 1e-8)

  #length = 2 effectsize, no warnings
  expect_silent(
    res4 <- eval_design_survival_mc(des, ~., distribution = 'gaussian', effectsize = c(2,3),
                                    alpha = 0.2, nsim = 1, detailedoutput = TRUE))
  expect_equal(res4$anticoef, c(0.5, 0.5, 0.5), tolerance = 1e-8)

  expect_silent(
    res5 <- eval_design_survival_mc(des, ~., distribution = 'lognormal', effectsize = c(1, 5),
                                    alpha = 0.2, nsim = 1, detailedoutput = TRUE)
  )
  expect_equal(res5$anticoef,
               skpr:::gen_exponential_anticoef(c(1,1,1), 1, 5), tolerance = 1e-8)

  expect_silent(
    res6 <- eval_design_survival_mc(des, ~., distribution = 'exponential',
                                    effectsize = c(4, 7.3),
                                    alpha = 0.2, nsim = 1, detailedoutput = TRUE)
  )
  expect_equal(res6$anticoef,
               skpr:::gen_exponential_anticoef(c(1,1,1), 4, 7.3), tolerance = 1e-8)

})
