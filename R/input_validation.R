#'@title Check Model Formula Validity
#'
#'@keywords internal
check_model_validity = function(model) {
  if (length(model) != 2) {
    stop(sprintf(
      "Model (`%s`) should not have a left-hand side (here, given as `%s`). Only include the right-hand side of the formula.",
      as.character(model),
      as.character(model[2])
    ))
  }
}
