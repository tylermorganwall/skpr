#'@title Simplex Contrast Generator
#'
#'@description Evaluates power for a right censored survival design with a Monte Carlo simulation,
#'using the survival package and survreg to fit the data.
#'
#'@param n The number of levels in the catagorical variable
#'@export
#'@examples contr.simplex(4)
contr.simplex = function(n,size=1) {
  results = matrix(nrow = n-1, ncol = n)
  if (n==1) {
    stop("Too few dimensions for contrast generation")
  }
  if (n==2) {
    results[,1]=1
    results[,2]=-1
    return(t(results))
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
