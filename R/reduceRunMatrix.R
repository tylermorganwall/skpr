#'@title Remove columns not in model
#'
#'@description Remove columns not in model
#'
#'@return The reduced model matrix.
#'@keywords internal
reduceRunMatrix = function(RunMatrix, model, first_run = TRUE) {
  order_vector = attr(terms(model), "order")
  orderone = attr(terms(model), "term.labels")[order_vector == 1]
  factor_matrix = attr(terms(model), "factors")
  factor_matrix_first_order = factor_matrix[, order_vector == 1, drop = FALSE]
  detect_linear_term = function(x) {
    any(x == 1)
  }
  has_linear_term = apply(factor_matrix_first_order, 1, detect_linear_term)
  interaction_only_term = any(!has_linear_term)
  if (interaction_only_term && first_run) {
    warning(
      "Interaction-only term in formula detected: it is rare that an interaction term should be present in a model without the corresponding main effect term(s)."
    )
  }
  #This removes I(x^n) terms from the basic
  nohighorder = rownames(factor_matrix)[
    !unlist(lapply(
      pattern = "^",
      FUN = grepl,
      X = rownames(factor_matrix),
      fixed = TRUE
    ))
  ]
  nohighorder = gsub("`", "", nohighorder, fixed = TRUE)
  reducerm = RunMatrix[, nohighorder, drop = FALSE]

  if (
    length(as.character(model)) == 2 &&
      (as.character(model)[2] == "." || as.character(model)[2] == "quad(.)") ||
      model == as.formula("~.*.")
  ) {
    return(reducerm)
  }
  for (var in colnames(reducerm)) {
    if (!(var %in% all.vars(model))) reducerm[var] = NULL
  }
  return(reducerm)
}
