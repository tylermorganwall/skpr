#'@title Generates conservative anticipated coefficients
#'
#'@description Generates conservative anticipated coefficients
#'
#'@param modelmatrix The model matrix
#'@return Conservative anticipated coefficients for given model matrix, with first parameter for each catagorical factor's anticipated coefficient set to
#'one and the rest set to zero. All continuous parameters' anticipated coefficients are set to one.
#'@export
#'@examples
#'test = expand.grid(a=as.factor(c(1,2,3,4,5,6)),b=c(1,0,-1),c=as.factor(c(1,2,3,4)),
#'                   d=c(-1,1), stringsAsFactors = TRUE)
#'de = gen_design(test,~(a+b+c+d),trials=12,optimality = "D",repeats=100)
#'eval_design(de,0.05,gen_conservative_anticoef(de))

gen_conservative_anticoef = function(modelmatrix) {

  levels = attr(modelmatrix,"levels")-1
  type = attr(modelmatrix,"type")

  anticoef = c(1)
  for(i in 1:length(levels)) {
    if (type[i] == "factor") {
      anticoef = c(anticoef,1,rep(0,levels[i]-1))
    }
    if (type[i] == "numeric") {
      anticoef = c(anticoef,1)
    }
  }
  return(anticoef)
}
