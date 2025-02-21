#'@title Permutations
#'
#'@description Return permutations
#'
#'@param n Number of elements to permute
#'@keywords internal
#'@return Matrix of permuted element ids
permutations = function(n) {
  if (n == 1) {
    return(matrix(1))
  } else {
    sp = permutations(n - 1)
    p = nrow(sp)
    A = matrix(nrow = n * p, ncol = n)
    for (i in 1:n) {
      A[(i - 1) * p + 1:p, ] = cbind(i, sp + (sp >= i))
    }
    return(A)
  }
}

#'@title Find potential permuted interactions
#'
#'@description Returns permuted interactions
#'
#'@param x character vector of interaction terms
#'@keywords internal
#'@return character vector of potential interaction terms
potential_permuted_factors = function(x) {
  numberterms = length(x)
  if (numberterms == 1) {
    return(x)
  }
  return(unique(unlist(apply(
    matrix(x[permutations(numberterms)], ncol = numberterms),
    1,
    paste,
    collapse = ":"
  ))))
}
