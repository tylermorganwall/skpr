

#' Compute effect powers from fit object
#'
#' @description Given a fit object and a specified type-1 error, return a vector of effect significances
#'
#' @param fit Fit object (e.g. from lm, glm, lmer, or glmer).
#' @param alpha Type-1 error. If the p-value for an effect is less than alpha, it will be declared significant.
#' @param parameter_assignment A vector indicating to which effect a parameter corresponds. Typically you would
#' generate this from fit$assign [for an lm object], or from attr(model.matrix(fit), 'assign').
#' @return Returns a vector of 1s and 0s, with 1 at position i indicating that
#' effect i is significant. The p-value for this test is computed with an F-test on the coeffecient
#' estimates for that effect.
#' The parameter names are given by c("(Intercept)", attr(terms(fit), 'term.labels')).
#' The returned vector has a length max(parameter_assignment) + 1.
#' @export
#'
effectSignificance <- function(fit, alpha, parameter_assignment) {
  retval = rep(0, max(parameter_assignment) + 1)

  if (class(fit)[1] == "lm" || class(fit)[1] == "glm") {
    estimates = coef(fit)
    variance = vcov(fit)
    dfres = df.residual(fit)
  }
  if (class(fit)[1] == "lmerMod" || class(fit)[1] == "glmerMod") {
    estimates = coef(summary(fit))[, 1]
    variance = as.matrix(vcov(fit))
    dfres = df.residual(fit)
  }
  for (i in 1:length(retval)) {
    L = parameter_assignment == (i - 1)  #picks out the rows/columns corresponding to the i-th effect
    g = sum(L)  #the degrees of freedom of the effect
    statistic = estimates[L] %*% solve(variance[L, L], estimates[L])
    if (pf(statistic, g, dfres, lower.tail = FALSE) < alpha) {
      retval[i] = 1
    }
  }
  retval
}
