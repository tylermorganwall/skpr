#'@title Calculate Effect Power
#'
#'@description Calculates the effect power given the anticipated coefficients and the type-I error
#'
#'
#'@param X The model matrix
#'@param levelvector The number of levels in each parameter (1st is always the intercept)
#'@param anticoef The anticipated coefficients
#'@param alpha the specified type-I error
#'@return The effect power for the parameters
#'@export
#'@examples
#'genparammatrix(6,2,1)
effectpower = function(X,levelvector,anticoef,alpha,priorcat) {

  levelvectoradj = levelvector
  levelvectoradj[attr(X,"type")=="numeric"] = 2
  levelvectoradj = c(1,levelvectoradj-1)

  L = replicate(length(levelvectoradj), matrix(NA, nrow = 0, ncol = 0))

  g = priorlevels(levelvectoradj)

  for (i in 1:(length(g)-1)) {
    L[[i]] = genparammatrix(dim(X)[2],levelvectoradj[i],g[i])
  }

  power = c(length(L))
  for(j in 1:length(L)) {
    #NEED TO ADJUST to include the intercept term as well
    power[j] = calculatepower(X,L[[j]],calcnoncentralparam(X,L[[j]],anticoef),alpha)
  }
  return(power)
}
