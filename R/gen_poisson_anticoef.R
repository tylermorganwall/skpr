#'@title Generates Poisson Anticipated Coefficients
#'
#'@description Generates Poisson Anticipated Coefficients
#'Solves the Poisson link function
#'mean = exp(beta0 + beta1 * x)
#'such that mean = mean_low when x = -1, and mean = mean_high when x = +1.
#'Equivalently, solves this set of equations for beta0 and beta1:
#'mean_low = exp(beta0 - beta1)
#'mean_high = exp(beta0 + beta1)
#'@param anticoef input anticipated coefficients of the proper length for your model matrix
#'@param mean_low The low value of the mean value
#'@param mean_high The high value of the mean value
#'@return Anticipated coefficients.
#'@keywords internal
gen_poisson_anticoef = function(anticoef, mean_low, mean_high) {
  if (mean_low <= 0 || mean_high <= 0) {
    stop("Exponential anticipated coefficients generation error: mean values must be positive.")
  }
  b0 = 1 / 2 * (log(mean_high) + log(mean_low))
  b  = 1 / 2 * (log(mean_high) - log(mean_low))
  newanticoef = anticoef*b
  newanticoef[1] = b0
  return(newanticoef)
}
