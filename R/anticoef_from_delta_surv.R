#'@title Generates Anticipated Coefficients from delta for eval_design_suvival_mc
#'
#'@description Generates Anticipated Coefficients from delta parameter
#'The logic for generating anticipated coefficients from delta varies with
#'glm family.
#'@param default_coef a vector of default coefficients, from gen_anticoef
#'@param delta the user-input delta parameter, must be a numeric vector of length 1 or 2
#'@param distribution the user-supplied distribution, either a string or a survreg distribution object
#'@return Anticipated coefficients.
#'@keywords internal
anticoef_from_delta_surv = function(default_coef, delta, distribution) {
  #check that delta is proper
  if (!is.numeric(delta)) {
    stop("delta parameter must be a numeric vector of length 1 or 2")
  }
  if (length(delta) > 2 || length(delta) < 1) {
    stop("delta parameter must be a numeric vector of length 1 or 2")
  }
  #generate anticipated coefficients
  if (distribution == "gaussian") {
    if (length(delta) == 1) {
      anticoef = default_coef * delta / 2
    }
    else {
      anticoef = default_coef * (delta[2] - delta[1]) / 2
    }
  }
  else if (distribution == "exponential") {
    if (length(delta) == 1) {
      anticoef = default_coef * delta / 2
      warning("default or length 1 delta used with distribution == 'exponential'. This can lead to unrealistic effect sizes - make sure the generated anticipated coeffcients are appropriate.")
    }
    else {
      anticoef = gen_exponential_anticoef(default_coef, delta[1], delta[2])
    }
  }
  else if (distribution == "lognormal") {
    if (length(delta) == 1) {
      anticoef = default_coef * delta / 2
      warning("default or length 1 delta used with distribution == 'lognormal'. This can lead to unrealistic effect sizes - make sure the generated anticipated coeffcients are appropriate.")
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
    warning("delta parameter used with unsupported distribution. Make sure the generated anticipated coefficients are appropriate.")
  }
  anticoef
}
