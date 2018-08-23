#'@title Calculate Effect Power
#'
#'@description Calculates the effect power given the anticipated coefficients and the type-I error
#'
#'
#'@param X The model matrix
#'@param levelvector The number of levels in each parameter (1st is always the intercept)
#'@param anticoef The anticipated coefficients
#'@param alpha the specified type-I error
#'@param vinv The V inverse matrix
#'@return The effect power for the parameters
#'@keywords internal
effectpower = function(RunMatrix, levelvector, anticoef, alpha, vinv = NULL) {

  L = replicate(length(levelvector), matrix(NA, nrow = 0, ncol = 0))

  g = priorlevels(levelvector)

  for (i in 1:(length(g) - 1)) {
    L[[i]] = genparammatrix(dim(attr(RunMatrix, "modelmatrix"))[2], levelvector[i], g[i])
  }

  power = c(length(L))
  for (j in 1:length(L)) {
    power[j] = calculatepower(attr(RunMatrix, "modelmatrix"), L[[j]],
                              calcnoncentralparam(attr(RunMatrix, "modelmatrix"), L[[j]], anticoef, vinv = vinv), alpha)
  }
  return(power)
}
