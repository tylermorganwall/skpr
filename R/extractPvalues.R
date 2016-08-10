#'@title Extract p-values from a model object
#'
#'@description Extract p-values from a model object. Currently works with lm, glm, lme4, glmer,
#'and survreg model objects. If possible, uses the p-values reported in summary(model_fit).
#'If those do not exist (I'm looking at you, lme4), returns the Wald p-value:
#'2*pnorm(-abs(estimate / se))
#'
#'@param model_fit The model object from which to extract.
#'@keywords internal
#'@return Returns a vector of p-values. If model_fit is not a supported model type, returns NULL.
extractPvalues = function(model_fit) {
  model_type = class(model_fit)
  if (model_type == 'lm' || model_type == 'glm' || model_type == 'glmerMod') {
    return(coef(summary(model_fit))[,4])
  }
  if (model_type == 'lmerMod') {
    estimates = coef(summary(model_fit))[, 1]
    se = coef(summary(model_fit))[, 2]
    return(2 * pnorm(-abs(estimates / se)))
  }
  if (model_type == 'survreg') {
    return(summary(model_fit)$table[,4])
  }
  return(NULL)
}
