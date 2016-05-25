#'@title Generates Anticipated Coefficients
#'
#'@description Generates Anticipated Coefficients
#'
#'@param RunMatrix The run matrix
#'@param conservative Default FALSE. If true, will generate conservative anticipated coefficients.
#'@return Anticipated coefficients.
#'@export
#'@examples
#'test = expand.grid(a=as.factor(c(1,2,3,4,5,6)),b=c(1,0,-1),c=as.factor(c(1,2,3,4)),
#'                   d=c(-1,1), stringsAsFactors = TRUE)
#'de = gen_design(test,~(a+b+c+d),trials=12,optimality = "D",repeats=100)
#'eval_design(de,0.05,gen_anticoef(de))
#'eval_design(de,0.05,gen_anticoef(de),conservative=TRUE)
gen_anticoef = function(RunMatrix,conservative=FALSE) {

  levels = sapply(sapply(RunMatrix,unique),length)-1
  type = sapply(RunMatrix,class)

  anticoef = c(1)
  if(!conservative) {
    for(i in 1:length(levels)) {
      if (type[i] == "factor" && levels[i] %% 2 == 0) {
        anticoef = c(anticoef,rep(c(1,-1),levels[i]/2))
      }
      if (type[i] == "factor" && levels[i] %% 2 == 1) {
        anticoef = c(anticoef,1,rep(c(-1,1),(levels[i]-1)/2))
      }
      if (type[i] == "numeric") {
        anticoef = c(anticoef,1)
      }
    }
    return(anticoef)
  } else {
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
}
