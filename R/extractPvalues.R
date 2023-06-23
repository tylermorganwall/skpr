#'@title Extract p-values from a model object
#'
#'@description Extract p-values from a model object. Currently works with lm, glm, lme4, glmer,
#'and survreg model objects. If possible, uses the p-values reported in summary(model_fit).
#'
#'@param model_fit The model object from which to extract.
#'@keywords internal
#'@return Returns a vector of p-values. If model_fit is not a supported model type, returns NULL.
extractPvalues = function(model_fit, glmfamily = "gaussian") {
  model_type = class(model_fit)
  if ("lm" %in% model_type || "glm" %in% model_type || "glmerMod" %in% model_type) {
    if(glmfamily != "exponential") {
      return(coef(summary(model_fit))[, 4])
    } else {
      return(coef(summary(model_fit, dispersion = 1))[, 4])
    }
  }
  if ("lmerModLmerTest" %in% model_type) {
    return(coef(summary(model_fit))[, 5])
  }
  if ("survreg" %in% model_type) {
    return(summary(model_fit)$table[, 4])
  }
  return(NULL)
}
