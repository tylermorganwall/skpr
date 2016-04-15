#'@title Calculate Non-Centrality Parameter
#'
#'@description Calculates the non-centrality parameter for the model matrix
#'
#'@param X The model matrix
#'@param L The parameter matrix/vector
#'@param b The anticipated coefficients
#'@return The non-centrality parameter for the F test
#'@export
#'@examples
#'genparammatrix(6,2,1)
calcnoncentralparam = function(X,L,b) {
  return(t(L %*% b) %*% solve(L %*% solve(t(X) %*% X) %*% t(L)) %*% L %*% b)
}
