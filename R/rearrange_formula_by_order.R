#'@title Rearrange formula by order
#'
#'@description Rearrange higher order arithmatic terms to end of formula
#'@param model Base model
#'@return Rearranged model
#'@keywords internal
rearrange_formula_by_order = function(model) {
  model_terms = attr(terms.formula(model),"term.labels")
  modelorder = attr(terms.formula(model),"order")
  higherorderterms = grepl("^I\\(.+\\)$",x=attr(terms.formula(model),"term.labels"),perl=TRUE)
  entries_move_end = (modelorder == 1 & higherorderterms)
  formula(paste0(c("~",paste0(c(model_terms[!entries_move_end],model_terms[entries_move_end]),collapse = " + ")),collapse=""))
}
