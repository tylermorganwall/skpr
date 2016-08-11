

context("effectSignificance")

test_that("effectSignificance works with all continuous", {
  #all continuous variables
  x = 1:10
  y = sin(x/2/pi)
  fit = lm(y ~ x)
  pvals = coef(summary(fit))[,4]
  signif = rep(0, 2)
  signif[pvals < 0.1] = 1
  effect_sig = effectSignificance(pvals, 0.05, attr(model.matrix(fit), 'assign'))
  expect_equal(signif, effect_sig)
})

test_that("works with two categorical factors, one is significant", {
  X = expand.grid(x1 = factor(c("a", "b", "c")), x2 = factor(c(1, 2, 3, 4, 5)))
  X$y = rep(1:8, length.out = nrow(X))
  fit = lm(y ~ x1 + x2, X)
  pvals = coef(summary(fit))[,4]
  signif = rep(0, length(coef(fit)))
  signif[pvals < 0.05] = 1
  expected_effect_sig = c(0, 0, 1)
  effect_sig = effectSignificance(pvals, 0.05, attr(model.matrix(fit), 'assign'))
  effect_sig == expected_effect_sig
  expect_equal(expected_effect_sig, effect_sig)

})

test_that("works with continuous and categorical factors", {
  set.seed(1)
  X = expand.grid(x1 = c(-1,1), x2 = factor(c(1, 2, 3, 4)), x3 = factor(c('a', 'b', 'c')))
  X$y = rnorm(nrow(X), 0, 0.01)
  X$y[X$x2 == 2] = 1
  fit = lm(y ~ x1 + x2 + x3, X)
  pvals = coef(summary(fit))[,4]
  signif = rep(0, length(pvals))
  signif[pvals < 0.05] = 1
  expected_effect_sig = c(0, 0, 1, 0)
  effect_sig = effectSignificance(pvals, 0.05, attr(model.matrix(fit), 'assign'))
  effect_sig == expected_effect_sig
  expect_equal(effect_sig, expected_effect_sig)
})
