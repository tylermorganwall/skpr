#'@title Converts dot operator to terms
#'
#'@description Converts the dot operator `.` in a formula to the linear terms in the model. Includes interactions (e.g. .*.)
#'@param design The design
#'@param model Base model
#'@param splitplotdesign split plot design data.frame
#'@return New model with dot operator replaced
#'@keywords internal
convert_model_dots = function(design, model, splitplotdesign = NULL) {
  if (any(unlist(strsplit(as.character(model[2]), "\\s\\+\\s|\\s\\*\\s|\\:|\\^")) == ".")) {
    if (is.null(splitplotdesign)) {
      dotreplace = paste0("(", paste0(attr(design, "names"), collapse = " + "), ")")
      additionterms = unlist(strsplit(as.character(model[2]), "\\s\\+\\s"))
      multiplyterms = unlist(lapply(lapply(strsplit(additionterms, split = "\\s\\*\\s"), gsub, pattern = "^\\.$", replacement = dotreplace), paste0, collapse = " * "))
      interactionterms = unlist(lapply(lapply(strsplit(multiplyterms, split = "\\:"), gsub, pattern = "^\\.$", replacement = dotreplace), paste0, collapse = ":"))
      powerterms = unlist(lapply(lapply(strsplit(interactionterms, split = "\\^"), gsub, pattern = "^\\.$", replacement = dotreplace), paste0, collapse = "^"))
      model = as.formula(paste0("~", paste(powerterms, collapse = " + "), sep = ""))
    } else {
      dotreplace = paste0("(", paste(c(colnames(splitplotdesign), colnames(design)), collapse = " + "), ")")
      additionterms = unlist(strsplit(as.character(model[2]), "\\s\\+\\s"))
      multiplyterms = unlist(lapply(lapply(strsplit(additionterms, split = "\\s\\*\\s"), gsub, pattern = "^\\.$", replacement = dotreplace), paste0, collapse = " * "))
      interactionterms = unlist(lapply(lapply(strsplit(multiplyterms, split = "\\:"), gsub, pattern = "^\\.$", replacement = dotreplace), paste0, collapse = ":"))
      powerterms = unlist(lapply(lapply(strsplit(interactionterms, split = "\\^"), gsub, pattern = "^\\.$", replacement = dotreplace), paste0, collapse = "^"))
      model = as.formula(paste0("~", paste(powerterms, collapse = " + "), sep = ""))
    }
  }
  return(model)
}

