#'@title Simplex Contrast Generator
#'
#'@description Generates orthogonal contrasts. Each row is the vertex of an N-dimensional simplex. The only exception are contrasts for the 2-level case, which return 1 and -1.
#'
#'@param n The number of levels in the catagorical variable
#'@param size The length of the simplex vector. Default 1.
#'@return A matrix of orthogonal contrasts.
#'@export
#'@examples contr.simplex(4)
contr.simplex = function(n,size=NULL) {
  results = matrix(nrow = n-1, ncol = n)
  if (n==1) {
    stop("Too few dimensions for contrast generation")
  }
  if (n==2) {
    results[,1]=1
    results[,2]=-1
    return(t(results))
  }
  if(is.null(size)) {
    size=sqrt(n-1)
  }
  results[1,] = c(size,rep(-size/(n-1),n-1))
  results[-1,1] = 0
  for(i in 2:(n-1)) {
    results[i:(n-1),i] = 0
    results[i,i] = sqrt(size^2-sum(results[1:(i-1),i]^2))
    results[i,(i+1):n] = -results[i,i]/length((i+1):n)
  }
  return(t(results))
}
