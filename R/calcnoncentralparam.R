#'@title Calculate Non-Centrality Parameter
#'
#'@description Calculates the non-centrality parameter for the model matrix
#'
#'@param X The model matrix
#'@param L The parameter matrix/vector
#'@param b The anticipated coefficients
#'@param vinv variance-covariance matrix
#'@return The non-centrality parameter for the F test
#'@keywords internal
calcnoncentralparam = function(X, L, b, vinv=NULL) {
  if (is.null(vinv)) {
    vinv = diag(nrow(X))
  }
  return(t(L %*% b) %*% solve(L %*% solve(t(X) %*% vinv %*% X) %*% t(L)) %*% L %*% b)
}
