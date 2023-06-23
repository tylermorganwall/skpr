library(lme4)

context("extractPvalues")

test_that("extractPvalues works as intended", {
  #lm
  x = 1:10
  y = sin(x / 2 / pi)
  fit = lm(y ~ x)
  expect_equal(coef(summary(fit))[, 4], extractPvalues(fit))

  #glm
  fitglm = glm(y ~ x, family = "gaussian")
  expect_equal(coef(summary(fitglm))[, 4], extractPvalues(fitglm))

  #glmer
  gm1 <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                data = cbpp, family = binomial)
  expect_equal(coef(summary(gm1))[, 4], extractPvalues(gm1))

  #survreg
  fitsurvreg = survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx, survival::ovarian, dist = "weibull",
                       scale = 1)
  expect_equal(summary(fitsurvreg)$table[, 4], extractPvalues(fitsurvreg))
})
