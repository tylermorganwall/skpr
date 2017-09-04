#'@title Generates Anticipated Coefficients from delta
#'
#'@description Generates Anticipated Coefficients from delta parameter
#'The logic for generating anticipated coefficients from delta varies with
#'glm family.
#'@param default_coef a vector of default coefficients, from gen_anticoef
#'@param delta the user-input delta parameter, must be a numeric vector of length 1 or 2
#'@param glmfamily the user-supplied glmfamily, either a string or a glmfamily object
#'@return Anticipated coefficients.
#'@keywords internal
anticoef_from_delta = function(default_coef, delta, glmfamily) {
  #check that delta is proper
  if (!is.numeric(delta)) {
    stop("delta parameter must be a numeric vector of length 1 or 2")
  }
  if (length(delta) > 2 || length(delta) < 1) {
    stop("delta parameter must be a numeric vector of length 1 or 2")
  }
  #generate anticipated coefficients
  if (glmfamily == "gaussian") {
    if (length(delta) == 1) {
      anticoef = default_coef * delta / 2
    }
    else {
      anticoef = default_coef * (delta[2] - delta[1]) / 2
    }
  }
  else if (glmfamily == "exponential") {
    if (length(delta) == 1) {
      anticoef = default_coef * delta / 2
      warning("default or length 1 delta used with glmfamily == 'exponential'. This can lead to unrealistic effect sizes - make sure the generated anticipated coeffcients are appropriate.")
    }
    else {
      anticoef = gen_exponential_anticoef(default_coef, delta[1], delta[2])
    }
  }
  else if (glmfamily == "poisson") {
    if (length(delta) == 1) {
      anticoef = default_coef * delta / 2
      warning("default or length 1 delta used with glmfamily == 'poisson'. This can lead to unrealistic effect sizes - make sure the generated anticipated coeffcients are appropriate.")
    }
    else {
      anticoef = gen_exponential_anticoef(default_coef, delta[1], delta[2])
    }
  }
  else if (glmfamily == "binomial") {
    if (length(delta) == 1) {
      anticoef = default_coef * delta / 2
      warning("default or length 1 delta used with glmfamily == 'binomial'. This can lead to unrealistic effect sizes - make sure the generated anticipated coeffcients are appropriate.")
    }
    else {
      anticoef = gen_exponential_anticoef(default_coef, delta[1], delta[2])
    }
  }
  else {
    if (length(delta) == 1) {
      anticoef = default_coef * delta / 2
    }
    else {
      anticoef = gen_exponential_anticoef(default_coef, delta[1], delta[2])
    }
    warning("delta parameter used with unsupported glmfamily. Make sure the generated anticipated coefficients are appropriate.")
  }
  anticoef
}
