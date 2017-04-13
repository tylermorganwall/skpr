#'@title Calculate Effect Power
#'
#'@description Calculates the effect power given the anticipated coefficients and the type-I error
#'
#'
#'@param X The model matrix
#'@param levelvector The number of levels in each parameter (1st is always the intercept)
#'@param anticoef The anticipated coefficients
#'@param alpha the specified type-I error
#'@param vInv The V inverse matrix
#'@return The effect power for the parameters
#'@keywords internal
effectpower = function(RunMatrix,levelvector,anticoef,alpha,vInv=NULL) {
  levelvectoradj = levelvector
  levelvectoradj[sapply(RunMatrix,class)=="numeric"] = 2
  levelvectoradj = c(1,levelvectoradj-1)

  L = replicate(length(levelvectoradj), matrix(NA, nrow = 0, ncol = 0))

  g = priorlevels(levelvectoradj)

  for (i in 1:(length(g)-1)) {
    L[[i]] = genparammatrix(dim(attr(RunMatrix,"modelmatrix"))[2],levelvectoradj[i],g[i])
  }

  Q = vector("list",dim(attr(RunMatrix,"modelmatrix"))[2])

  power = c(length(L))
  for(j in 1:length(L)) {
    power[j] = calculatepower(attr(RunMatrix,"modelmatrix"),L[[j]],calcnoncentralparam(attr(RunMatrix,"modelmatrix"),L[[j]],anticoef,vInv=vInv),alpha)
  }
  return(power)
}
