#'@title Rearrange formula by order
#'
#'@description Rearrange higher order arithmatic terms to end of formula
#'@param model Base model
#'@return Rearranged model
#'@keywords internal
rearrange_formula_by_order = function(model, data) {
  interceptterm = attr(terms.formula(model, data = data), "intercept") == 1
  model_terms = attr(terms.formula(model, data = data), "term.labels")
  modelorder = attr(terms.formula(model, data = data), "order")
  higherorderterms = grepl(
    "^I\\(.+\\)$",
    x = attr(terms.formula(model, data = data), "term.labels"),
    perl = TRUE
  )
  entries_move_end = (modelorder == 1 & higherorderterms)
  rearranged_model = formula(paste0(
    c(
      "~",
      paste0(
        c(model_terms[!entries_move_end], model_terms[entries_move_end]),
        collapse = " + "
      )
    ),
    collapse = ""
  ))
  if (!interceptterm) {
    rearranged_model = update.formula(rearranged_model, ~ . + -1)
  }
  rearranged_model
}
