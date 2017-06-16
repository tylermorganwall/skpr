#'@title Alias terms
#'
#'@description Creates alias terms for Alias-optimal designs
#'
#'@param formula The formula to be expanded
#'@keywords internal
#'@import utils
#'@return Returns aliased model terms from formula
aliasmodel = function(formula, power) {
  existingterms = attr(terms(formula),"term.labels")
  # existingterms = c(existingterms,disallowed)
  variables = all.vars(formula)

  if(power < 2) {
    stop("Aliased terms must be greater than linear")
  }

  aliasterms=c()

  if(length(variables) == 1) {
    return(formula)
  }
  for(pow in 2:power) {
    powerterms = apply(combn(variables,pow), 2, paste, collapse="*")
    powerterms = powerterms[!(powerterms %in% existingterms)]
    if(length(powerterms) > 0) {
      aliasterms = c(aliasterms,paste(powerterms, collapse=" + "))
    }
  }

  if(length(aliasterms) == 0) {
    return(formula)
  }

  aliasterms = paste0(aliasterms,collapse=" + ")
  aliasterms = paste0(c("~",aliasterms),collapse="")

  return(as.formula(aliasterms))
}
