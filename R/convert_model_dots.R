#'@title Converts dot operator to terms
#'
#'@description Converts the dot operator `.` in a formula to the linear terms in the model. Includes interactions (e.g. .*.)
#'@param RunMatrix The Run Matrix
#'@param model Base model
#'@return New model with dot operator replaced
#'@keywords internal
convert_model_dots = function(RunMatrix, model) {
  if (any(unlist(strsplit(as.character(model[2]), "\\s\\+\\s|\\s\\*\\s|\\:")) == ".")) {
    dotreplace = paste0("(", paste0(colnames(RunMatrix), collapse = " + "), ")")
    additionterms = unlist(strsplit(as.character(model[2]), "\\s\\+\\s"))
    multiplyterms = unlist(lapply(lapply(strsplit(additionterms, split = "\\s\\*\\s"), gsub, pattern = "^\\.$", replacement = dotreplace), paste0, collapse = " * "))
    interactionterms = unlist(lapply(lapply(strsplit(multiplyterms, split = "\\:"), gsub, pattern = "^\\.$", replacement = dotreplace), paste0, collapse = ":"))
    model = as.formula(paste0("~", paste(interactionterms, collapse = " + "), sep = ""))
  }
  return(model)
}
