context("testDeltaProcessing")

test_that("providing anticoef and delta to eval_design produces a warning", {
  des = expand.grid(x = c(-1, 1), y = 10:11)
  expect_warning(eval_design(des, ~., delta = 2, anticoef = c(1, 1, 1), alpha = 0.2),
                 "Because you provided anticoef, we will ignore the delta argument.")
})



test_that("providing anticoef and delta to eval_design_mc produces a warning", {
  des = expand.grid(x = c(-1, 1), y = 10:11)
  expect_warning(eval_design_mc(des, ~., delta = 2, anticoef = c(1, 1, 1), alpha = 0.2, nsim = 1),
                 "Because you provided anticoef, we will ignore the delta argument.")
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

test_that("eval_design_mc processes delta properly for glm", {
  #TODO: fill this in
  cand = expand.grid(x = c(-1, 1), y = c(-1, 1))
  des = gen_design(cand, ~., trials = 100)
  #length = 1 delta, warning except for gaussian:
  expect_silent(eval_design_mc(des, ~., glmfamily = 'gaussian', delta = 2, alpha = 0.2, nsim = 1))
  expect_warning(eval_design_mc(des, ~., glmfamily = 'exponential', delta = 2, alpha = 0.2, nsim = 1),
               "default or length 1 delta used with glmfamily == 'exponential'")
  expect_warning(eval_design_mc(des, ~., glmfamily = 'poisson', delta = 2, alpha = 0.2, nsim = 1),
               "default or length 1 delta used with glmfamily == 'poisson'")
  expect_warning(eval_design_mc(des, ~., glmfamily = 'binomial', delta = 2, alpha = 0.2, nsim = 1),
               "default or length 1 delta used with glmfamily == 'binomial'")

  #length = 2 delta, works in all cases:
  expect_silent(eval_design_mc(des, ~., glmfamily = 'binomial', delta = c(0.6, 0.8), alpha = 0.2, nsim = 1))
  expect_silent(eval_design_mc(des, ~., glmfamily = 'exponential', delta = c(3, 5), alpha = 0.2, nsim = 1))
  expect_silent(eval_design_mc(des, ~., glmfamily = 'poisson', delta = c(5.2, 8.3), alpha = 0.2, nsim = 1))
  expect_silent(eval_design_mc(des, ~., glmfamily = 'gaussian', delta = c(5, 8), alpha = 0.2, nsim = 1))
})
