#'@title Generate Parameter Matrix
#'
#'@description Generates the parameter matrix L to isolate the levels of interest
#' in the calculation of power
#'
#'@param Parameters Number of parameters total in model.
#'@param Levels Number of levels in parameter of interest
#'@param G Number of levels/parameters preceeding parameter of interest
#'@return The parameter vector Q isolating the levels of parameter of interest
#'@keywords internal
genparammatrix = function(parameters, levels, g) {
  if (parameters <= 0 | levels <= 0 | g < 0) {
    stop("Number of parameters and levels must be non-zero, preceeding levels must be non-negative")
  }
  if (parameters < levels) {
    stop("Number of parameters must be greater than number of levels")
  }
  L = matrix(0,ncol=parameters,nrow=levels)
  L[,(g+1):(g+levels)] = diag(levels)
  if(dim(L)[1] == 1) {
    return(t(as.vector(L)))
  }
  return(L)
}
