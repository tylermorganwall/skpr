#'@title Generates Binomial Anticipated Coefficients
#'
#'@description Generates Binomial Anticipated Coefficients
#'Solves the logistic function
#'log(p / (1-p)) = beta0 + beta1 * x
#'such that p = lowprob when x = -1, and p = highprob when x = +1.
#'Equivalently, solves this set of equations for beta0 and beta1:
#'log(lowprob / (1 - lowprob)) = beta0 - beta1
#'log(highprob / (1 - highprob)) = beta0 + beta1
#'@param runmatrix The Run Matrix
#'@param model Base model
#'@param contrastslist Contrasts
#'@param anticoef Anticipated coefficeints
#'@param lowprob Default 0.50. The base probability
#'@param highprob Default 0.80. The base probability
#'@return Anticipated coefficients.
#'@keywords internal
gen_binomial_anticoef = function(anticoef, lowprob, highprob) {
  if (highprob > 1 || highprob < 0 || lowprob > 1 || lowprob < 0) {
    stop("Binomial anticipated coefficients generation error: Probabilities must be between 0 and 1")
  }
  b0 = 1/2 * log(lowprob * highprob / (1 - lowprob) / (1 - highprob))
  b = 1/2 * log(highprob * (1 - lowprob) / (1 - highprob) / lowprob)
  newanticoef = anticoef*b
  newanticoef[1] = b0
  return(newanticoef)
}
