#'@title Calculates parameter power
#'
#'@description Calculates parameter power
#'
#'@param X The model matrix
#'@param anticoef The anticipated coefficients
#'@param alpha the specified type-I error
#'@param vinv The V inverse matrix
#'@return The parameter power for the parameters
#'@keywords internal
parameterpower = function(RunMatrix, anticoef, alpha, vinv = NULL) {
  #Generating the parameter isolating vectors
  q = vector("list", dim(attr(RunMatrix, "modelmatrix"))[2])
  for (i in 1:length(q)) {
    vec = rep(0, dim(attr(RunMatrix, "modelmatrix"))[2])
    vec[i] = 1
    q[[i]] = t(vec)
  }
  power = c(length(q))
  for (j in 1:length(q)) {
    power[j] = calculatepower(attr(RunMatrix, "modelmatrix"), q[[j]],
                              calcnoncentralparam(attr(RunMatrix, "modelmatrix"),
                                                  q[[j]], anticoef, vinv = vinv),
                              alpha)
  }
  return(power)
}
