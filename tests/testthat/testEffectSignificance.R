

context("effectSignificance")

test_that("effectSignificance works with all continuous, lm", {
  #all continuous variables
  X = expand.grid(x1 = c(-1, 1), x2 = c(-1, 1))
  set.seed(1)
  X$y = X$x1 + rnorm(nrow(X), 0, 0.00001)
  fit = lm(y ~ x1 + x2, data=X)
  effect_sig = effectSignificance(fit, 0.05, fit$assign)
  expected = c(0, 1, 0)
  expected == effect_sig
  expect_equal(expected, effect_sig)
})

test_that("works with two categorical factors, one is significant, lm", {
  X = expand.grid(x1 = factor(c("a", "b", "c")), x2 = factor(c(1, 2, 3, 4, 5)))
  X$y = rep(1:8, length.out = nrow(X))
  fit = lm(y ~ x1 + x2, X)
  expected_effect_sig = c(0, 0, 1)
  effect_sig = effectSignificance(fit, 0.05, fit$assign)
  effect_sig == expected_effect_sig
  expect_equal(expected_effect_sig, effect_sig)

})

test_that("works with continuous and categorical factors, lm", {
  set.seed(1)
  X = expand.grid(x1 = c(-1,1), x2 = factor(c(1, 2, 3, 4)), x3 = factor(c('a', 'b', 'c')))
  X$y = rnorm(nrow(X), 0, 0.01)
  X$y[X$x2 == 2] = 1

  fit = lm(y ~ x1 + x2 + x3, X)
  expected_effect_sig = c(0, 0, 1, 0)
  effect_sig = effectSignificance(fit, 0.05, fit$assign)
  effect_sig == expected_effect_sig
  expect_equal(effect_sig, expected_effect_sig)
})


test_that("Works with glm", {
  X = expand.grid(x1 = c(-1,1), x2 = factor(c(1, 2, 3, 4)), x3 = factor(c('a', 'b', 'c')))
  X = rbind(X, X, X, X)
  set.seed(1999)
  X$p = 1 / (1 + exp(-(X$x2 == 2)))
  X$y = rbinom(nrow(X), 1, X$p)

  fit = glm(y ~ x1 + x2 + x3, data = X, family = 'binomial')
  summary(fit)
  assignment = attr(model.matrix(fit), 'assign')
  expected_effect_sig = c(0, 0, 0, 1)
  effect_sig = effectSignificance(fit, 0.05, assignment)
  effect_sig == expected_effect_sig
  expect_equal(effect_sig, expected_effect_sig)
})

test_that("Works with lmer", {
  set.seed(1)
  X = expand.grid(x1 = c(-1,1), x2 = factor(c(1, 2, 3, 4)), x3 = factor(c('a', 'b', 'c')))
  X$y = rnorm(nrow(X), 0, 0.01)
  X['block1'] = rep(c(1, 2), each = nrow(X)/2)
  X$y[X$x2 == 2] = 1
  X$y = X$y + X$block1/10

  fit = lme4::lmer(y ~ x1 + x2 + x3 + (1 | block1), X)
  parameter_assignment = attr(model.matrix(fit), 'assign')
  expected_effect_sig = c(1, 0, 1, 0)
  effect_sig = effectSignificance(fit, 0.05, parameter_assignment)
  effect_sig == expected_effect_sig
  expect_equal(effect_sig, expected_effect_sig)
})

#todo

test_that("Works with glmer", {
  X = expand.grid(x1 = c(-1,1), x2 = factor(c(1, 2, 3, 4)), x3 = factor(c('a', 'b', 'c')))
  X = rbind(X, X, X, X)
  X['block1'] = rep(c(1, 2), each = nrow(X)/2)
  X$p = 1 / (1 + exp(-(X$x2 == 2) + X$block1/100))
  set.seed(1999)
  X$y = rbinom(nrow(X), 1, X$p)

  fit = lme4::glmer(y ~ x1 + x2 + x3 + (1 | block1), X, family = 'binomial')
  parameter_assignment = attr(model.matrix(fit), 'assign')
  expected_effect_sig = c(0, 0, 0, 1)
  effect_sig = effectSignificance(fit, 0.1, parameter_assignment)
  effect_sig == expected_effect_sig
  expect_equal(effect_sig, expected_effect_sig)
})
