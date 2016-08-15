

#' Compute effect powers from fit object
#'
#' @description Given a fit object and a specified type-1 error, return a vector of effect significances
#'
#' @param fit Fit object (e.g. from lm, glm, etc.).
#' @param alpha Type-1 error. If the p-value for an effect is less than alpha, it will be declared significant.
#'
#' @return Returns a vector of 1s and 0s, with 1 at position i indicating that
#' effect i has at least one significant parameter.
#' The parameter names are given by c("(Intercept)", attr(terms(fit), 'term.labels')).
#' The returned vector has a length max(fit$assign) + 1.
#' p-values are computed with an F-test on the coeffecient estimates.
#' @export
#'
effectSignificance <- function(fit, alpha, parameter_assignment) {
  estimates = coef(fit)
  variance = vcov(fit)
  retval = rep(0, max(parameter_assignment) + 1)
  for (i in 1:length(retval)) {
    L = parameter_assignment == (i - 1)  #picks out the rows/columns corresponding to the i-th effect
    g = sum(L)  #the degrees of freedom of the effect
    statistic = estimates[L] %*% solve(variance[L, L], estimates[L])
    if (pf(statistic, g, fit$df.residual, lower.tail = FALSE) < alpha) {
      retval[i] = 1
    }
  }
  retval
}
