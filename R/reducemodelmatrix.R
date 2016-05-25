#'@title Remove columns not in model
#'
#'@description Remove columns not in model
#'
#'@param ModelMatrix The model matrix
#'@param model The formula
#'@return The reduced model matrix
#'@examples mean(1)
#'@keywords internal

reducemodelmatrix = function(RunMatrix,model) {
  ModelMatrix = attr(RunMatrix,"modelmatrix")
  ReduceRM = RunMatrix
  #save the attributes to reapply them to the reduced matrix
  attributelist = attributes(ModelMatrix)
  attributelist$dim = NULL
  attributelist$dimnames = NULL

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
      #attributelist[["levels"]] = attributelist[["levels"]][!(parameter == names(attributelist[["levels"]]))]
      #attributelist[["type"]] = attributelist[["type"]][!(parameter == names(attributelist[["type"]]))]
      ReduceRM[parameter] = NULL
    }
  }
  #return original attributes to new reduced model matrix
  attr(ReduceRM,"modelmatrix") = ModelMatrix[,removecols]
  for(i in 1:length(attributelist)) {
    attr(attr(ReduceRM,"modelmatrix"), names(attributelist)[i]) = attributelist[[i]]
  }
  return(ReduceRM)
}
