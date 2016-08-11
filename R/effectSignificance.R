

#' Compute effect powers from parameter powers
#'
#' @description Given a vector of parameter p-values, return a vector of effect p-values
#'
#' @param pvals Vector of p-values, from a fit (e.g. lm, glm, etc.)
#' @param alpha Type-1 error.
#' @param parameter_assignment Vector that assigns each parameter to an effect.
#'
#' @return Returns a vector of 1s and 0s, with 1 at position i indicating that
#' effect i has at least one significant parameter. If parameter_assignment came from
#' attr(model.matrix, 'assign'), then the i-th parameter name is given by attr(terms(model), 'term.labels').
#' The returned vector has a length max(parameter_assignment) + 1.
#' @export
#'
effectSignificance <- function(pvals, alpha, parameter_assignment) {
  retval = rep(0, max(parameter_assignment) + 1)
  for (i in 1:length(retval)) {
    if (any(pvals[parameter_assignment == (i - 1)] < alpha)) {
      retval[i] = 1
    }
  }
  retval
}
