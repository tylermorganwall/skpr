#'@title Remove columns not in model
#'
#'@description Remove columns not in model
#'
#'@param ModelMatrix The model matrix
#'@param model The formula
#'@return The reduced model matrix.
#'@keywords internal
reduceRunMatrix = function(RunMatrix, model) {
  orderone = attr(terms(model), "term.labels")[attr(terms(model), "order") == 1]
  nohighorder = orderone[!unlist(lapply(pattern = "^", FUN = grepl, X = orderone, fixed = TRUE))]
  nohighorder = gsub("`", "", nohighorder, fixed = TRUE)
  reducerm = RunMatrix[nohighorder]

  if (length(as.character(model)) == 2 && (as.character(model)[2] == "." || as.character(model)[2] == "quad(.)") || model == as.formula("~.*.")) {
    return(reducerm)
  }
  for (var in colnames(reducerm)) {
    if (!(var %in% all.vars(model))) reducerm[var] = NULL
  }
  reducerm
}
