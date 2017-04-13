#'@title Calculates parameter power
#'
#'@description Calculates parameter power
#'
#'@param X The model matrix
#'@param anticoef The anticipated coefficients
#'@param alpha the specified type-I error
#'@param vInv The V inverse matrix
#'@return The parameter power for the parameters
#'@keywords internal
parameterpower = function(RunMatrix,anticoef,alpha,vInv=NULL) {
  #Generating the parameter isolating vectors
  Q = vector("list",dim(attr(RunMatrix,"modelmatrix"))[2])
  for(i in 1:length(Q)) {
    vec = rep(0,dim(attr(RunMatrix,"modelmatrix"))[2])
    vec[i] = 1
    Q[[i]] = t(vec)
  }

  power = c(length(Q))
  for(j in 1:length(Q)) {
    power[j] = calculatepower(attr(RunMatrix,"modelmatrix"),Q[[j]],calcnoncentralparam(attr(RunMatrix,"modelmatrix"),Q[[j]],anticoef,vInv=vInv),alpha)
  }

  return(power)
}
