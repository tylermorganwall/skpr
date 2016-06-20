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
parameterpower = function(RunMatrix,anticoef,alpha,varianceratio=rep(0,length(anticoef))) {
  #Generating the parameter isolating vectors
  Q = vector("list",dim(attr(RunMatrix,"modelmatrix"))[2])
  for(i in 1:length(Q)) {
    vec = rep(0,dim(attr(RunMatrix,"modelmatrix"))[2])
    vec[i] = 1
    Q[[i]] = t(vec)
  }

  power = c(length(Q))
  for(j in 1:length(Q)) {
    power[j] = calculatepower(attr(RunMatrix,"modelmatrix"),Q[[j]],calcnoncentralparam(attr(RunMatrix,"modelmatrix"),Q[[j]],anticoef,V=Q[[j]]%*%varianceratio),alpha)
  }

  return(power)
}
