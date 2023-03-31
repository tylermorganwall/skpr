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
  matrix_solve = tryCatch({solve(t(X) %*% vinv %*% X)}, error = function(e) e)
  if(inherits(matrix_solve,"error")) {
    stop(sprintf("skpr: Can't calculate non-centrality parameter with given design matrix (X) and variance-covariance (Vinv) matrix due to `t(X) %*% Vinv %*% X` being singular. `solve()` error message: '%s'",
                 as.character(matrix_solve)))
  }
  return(t(L %*% b) %*% solve(L %*% matrix_solve %*% t(L)) %*% L %*% b)
}
