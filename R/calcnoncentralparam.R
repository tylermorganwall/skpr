#'@title Calculate Non-Centrality Parameter
#'
#'@description Calculates the non-centrality parameter for the model matrix
#'
#'@param X The model matrix
#'@param L The parameter matrix/vector
#'@param b The anticipated coefficients
#'@return The non-centrality parameter for the F test
#'@keywords internal
calcnoncentralparam = function(X,L,b,vInv=NULL) {
  if(is.null(vInv)) {
    vInv = diag(nrow(X))
  }
  return((t(L %*% b) %*% solve(L %*% solve(t(X) %*% vInv %*% X) %*% t(L)) %*% L %*% b))
}
