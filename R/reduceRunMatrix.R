#'@title Remove columns not in model
#'
#'@description Remove columns not in model
#'
#'@param ModelMatrix The model matrix
#'@param model The formula
#'@return The reduced model matrix.
#'@keywords internal
reduceRunMatrix = function(RunMatrix,model) {
  if((as.character(model)[2] == "." || as.character(model)[2] == "quad(.)") || model == as.formula("~.*.")) {
    ReduceRM = RunMatrix
  } else {
    orderone = attr(terms(model),"term.labels")[attr(terms(model),"order")==1]
    nohighorder = orderone[!unlist(lapply(pattern="^",FUN=grepl,X=orderone,fixed=TRUE))]
    ReduceRM = RunMatrix[nohighorder]
  }

  if(length(as.character(model)) == 2 && (as.character(model)[2] == "." || as.character(model)[2] == "quad(.)") || model == as.formula("~.*.")) {
    return(ReduceRM)
  }
  for (var in colnames(ReduceRM)) {
    if (!(var %in% all.vars(model))) ReduceRM[var] = NULL
  }
  ReduceRM
}
