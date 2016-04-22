#'@title Calculates parameter power
#'
#'@description Calculates parameter power
#'
#'@param X The model matrix
#'@param anticoef The anticipated coefficients
#'@param alpha the specified type-I error
#'@return The parameter power for the parameters
#'@keywords internal
#'@examples mean(1)
parameterpower = function(X,anticoef,alpha) {
  #Generating the parameter isolating vectors
  Q = vector("list",dim(X)[2])
  for(i in 1:length(Q)) {
    vec = rep(0,dim(X)[2])
    vec[i] = 1
    Q[[i]] = t(vec)
  }

  power = c(length(Q))
  for(j in 1:length(Q)) {
    power[j] = calculatepower(X,Q[[j]],calcnoncentralparam(X,Q[[j]],anticoef),alpha)
  }

  return(power)
}
