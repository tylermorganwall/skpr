

context("effectSignificance")

test_that("effectSignificance works with all continuous, lm", {
  #all continuous variables
  X = expand.grid(x1 = c(-1, 1), x2 = c(-1, 1))
  X$y = X$x1 + rnorm(nrow(X), 0, 0.00001)
  fit = lm(y ~ x1 + x2, data=X)
  effect_sig = effectSignificance(fit, 0.05)
  expected = c(0, 1, 0)
  expected == effect_sig
  expect_equal(expected, effect_sig)
})

test_that("works with two categorical factors, one is significant, lm", {
  X = expand.grid(x1 = factor(c("a", "b", "c")), x2 = factor(c(1, 2, 3, 4, 5)))
  X$y = rep(1:8, length.out = nrow(X))
  fit = lm(y ~ x1 + x2, X)
  expected_effect_sig = c(0, 0, 1)
  effect_sig = effectSignificance(fit, 0.05)
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
  effect_sig = effectSignificance(fit, 0.05)
  effect_sig == expected_effect_sig
  expect_equal(effect_sig, expected_effect_sig)
})

#todo
test_that("Works with glm", {
  set.seed(1)
  X = expand.grid(x1 = c(-1,1), x2 = factor(c(1, 2, 3, 4)), x3 = factor(c('a', 'b', 'c')))
  X$y = 0
  X$y[X$x2 == 2] = 1
  fit = glm(y ~ x1 + x2 + x3, data = X, family = 'binomial')
  expected_effect_sig = c(0, 1, 1, 1)
  effect_sig = effectSignificance(fit, 0.05)
  effect_sig == expected_effect_sig
  expect_equal(effect_sig, expected_effect_sig)
})

#todo
test_that("Works with lmer", {
  set.seed(1)
  X = expand.grid(x1 = c(-1,1), x2 = factor(c(1, 2, 3, 4)), x3 = factor(c('a', 'b', 'c')))
  X$y = rnorm(nrow(X), 0, 0.01)
  X$y[X$x2 == 2] = 1
  fit = lm(y ~ x1 + x2 + x3, X)
  expected_effect_sig = c(0, 0, 1, 0)
  effect_sig = effectSignificance(fit, 0.05)
  effect_sig == expected_effect_sig
  expect_equal(effect_sig, expected_effect_sig)
})

#todo
test_that("Works with glmer", {
  set.seed(1)
  X = expand.grid(x1 = c(-1,1), x2 = factor(c(1, 2, 3, 4)), x3 = factor(c('a', 'b', 'c')))
  X$y = rnorm(nrow(X), 0, 0.01)
  X$y[X$x2 == 2] = 1
  fit = lm(y ~ x1 + x2 + x3, X)
  expected_effect_sig = c(0, 0, 1, 0)
  effect_sig = effectSignificance(fit, 0.05)
  effect_sig == expected_effect_sig
  expect_equal(effect_sig, expected_effect_sig)
})
