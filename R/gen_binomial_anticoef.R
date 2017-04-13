#'@title Generates Binomial Anticipated Coefficients
#'
#'@description Generates Binomial Anticipated Coefficients
#'
#'@param runmatrix The Run Matrix
#'@param model Base model
#'@param contrastslist Contrasts
#'@param anticoef Anticipated coefficeints
#'@param lowprob Default 0.50. The base probability
#'@param highprob Default 0.80. The base probability
#'@return Anticipated coefficients.
#'@keywords internal
gen_binomial_anticoef = function(anticoef, lowprob, highprob) {

  if(highprob <= lowprob) {
    stop("Binomial anticipated coefficients generation error: High probabilty must be higher than low probability")
  }
  if(highprob > 1 || lowprob > 1) {
    stop("Binomial anticipated coefficients generation error: Probabilities must be between 0 and 1")
  }

  b0 = log(sqrt(highprob*lowprob)/(1-sqrt(highprob*lowprob)))
  b = 1/2*log(highprob/lowprob*(1-lowprob)/(1-highprob))
  newanticoef = anticoef*b
  newanticoef[1] = b0

  return(newanticoef)

}
