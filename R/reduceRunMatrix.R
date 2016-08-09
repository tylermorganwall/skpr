#'@title Remove columns not in model
#'
#'@description Remove columns not in model
#'
#'@param ModelMatrix The model matrix
#'@param model The formula
#'@return The reduced model matrix
#'@keywords internal
reduceRunMatrix = function(RunMatrix,model) {
  ReduceRM = RunMatrix
  if(length(as.character(model)) == 2 && as.character(model)[2] == ".") {
    return(ReduceRM)
  }
  return(ReduceRM[colnames(ReduceRM) %in% all.vars(model)])
}
