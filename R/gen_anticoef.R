#'@title Generates Default Anticipated Coefficients
#'
#'@description Generates Default Anticipated Coefficients
#'
#'@param modelmatrix The model matrix
#'@return Anticipated coefficients
#'@export
#'@examples genparammatrix(6,2,1)
#'genparammatrix(6,2,1)
gen_anticoef = function(modelmatrix) {

  levels = attr(modelmatrix,"levels")-1
  type = attr(modelmatrix,"type")

  anticoef = c(1)
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
}
