#'@title Calculate Power
#'
#'@description Calculates the power of the model given the non-centrality parameter
#'
#'@param X The model matrix
#'@param L The parameter matrix/vector
#'@param lambda The non-centrality parameter for the F test
#'@param alpha the specified type-I error
#'@return The power for a given parameter L, given the
#'@keywords internal
calculatepower = function(X,L, lambda, alpha) {
  return(1-pf(qf(1-alpha, dim(L)[1], dim(X)[1]-dim(X)[2]),dim(L)[1],dim(X)[1]-dim(X)[2],lambda))
}
