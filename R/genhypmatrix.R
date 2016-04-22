#'@title Generate Hypothesis Matrix
#'
#'@description Generates hypothesis matrix for power calculation
#'
#'@param Parameters Number of parameters total in model
#'@param Levels Number of levels in parameter of interest
#'@param G Number of levels/parameters preceeding parameter of interest
#'@return The parameter matrix L isolating the levels of parameter of interest
#'@keywords internal
genhypmatrix = function(parameters, levels, g) {
  if (parameters <= 0 | levels <= 0 | g <= 0) {
    stop("All inputs must be greater than zero")
  }
  if (parameters < levels) {
    stop("Number of parameters must be greater than number of levels")
  }
  if (g+levels-1 > parameters) {
    stop("Too many parameters/levels")
  }
  L = matrix(0,ncol=parameters,nrow=levels-1)
  L[,(g+1):(g+levels-1)] = diag(levels-1)
  if(dim(L)[1] == 1) {
    return(t(as.vector(L)))
  }
  return(L)
}
