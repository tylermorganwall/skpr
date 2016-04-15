#'@title Generates Default Conservative Anticipated Coefficients
#'
#'@description Generates Default Conservative Anticipated Coefficients
#'
#'@param modelmatrix The model matrix
#'@return Conservative Anticipated coefficients
#'@export
#'@examples genparammatrix(6,2,1)
#'genparammatrix(6,2,1)
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
