#'@title quadratic
#'
#'@description quadratic
#'
#'@param formula The formula to be expanded
#'@keywords internal
#'@return Returns quadratic model from formula
quad = function(formula) {
  variables = all.vars(formula)
  quadratic = paste(variables, collapse=" + ")
  quadratic = paste("~", quadratic, sep="")
  for(i in 1:length(variables)) {
    for(j in i:length(variables)) {
      if(i == j) {
        quadratic = paste(quadratic," + I(", variables[i],"^2)",sep="",collapse="")
      } else {
        quadratic = paste(quadratic," + ", variables[i], ":", variables[j], sep="", collapse="")
      }
    }
  }
  return(as.formula(quadratic))
}
