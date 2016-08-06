#'@title Remove columns not in model
#'
#'@description Remove columns not in model
#'
#'@param ModelMatrix The model matrix
#'@param model The formula
#'@return The reduced model matrix
#'@keywords internal
reduceRunMatrix = function(RunMatrix,model) {
  ModelMatrix = model.matrix(model,RunMatrix)
  ReduceRM = RunMatrix
  if(length(as.character(model)) == 2 && as.character(model)[2] == ".") {
    return(ReduceRM)
  }
  removecols = rep(TRUE,ncol(ModelMatrix))
  for(parameter in colnames(RunMatrix)) {
    if(!(parameter %in%  attr(terms(model),"term.labels"))) {
      if(!is.null(sapply(RunMatrix,class)[parameter]) && sapply(RunMatrix,class)[parameter] == "factor") {
        removecols = (removecols & !(parameter == substr(colnames(ModelMatrix), 1, nchar(colnames(ModelMatrix))-1)))
        attributelist[["contrasts"]][[parameter]] = NULL
      }
      if(!is.null(sapply(RunMatrix,class)[parameter]) && sapply(RunMatrix,class)[parameter] == "numeric") {
        removecols = (removecols & !(parameter == colnames(ModelMatrix)))
      }
      ReduceRM[parameter] = NULL
    }
  }

  #return original attributes to new reduced model matrix
  return(ReduceRM)
}
