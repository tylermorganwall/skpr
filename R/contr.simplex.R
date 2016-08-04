contr.simplex = function(n) {
  results = matrix(nrow = n-1, ncol = n)
  if (n==1){
    error("Too few dimensions for contrast generation")
  }
  if (n==2) {
    results[,1]=1
    results[,2]=-1
    return(t(results))
  }
  results[1,] = c(sqrt(n-1),rep(-1/sqrt(n-1),n-1))
  results[-1,1] = 0
  for(i in 2:(n-1)) {
    results[i:(n-1),i] = 0
    results[i,i] = sqrt(n-1-sum(results[1:(i-1),i]^2))
    results[i,(i+1):n] = -results[i,i]/length((i+1):n)
  }
  return(t(results))
}
