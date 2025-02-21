#'@title Normalize Design
#'
#'@description Normalizes the numeric columns in the design to -1 to 1. This is important to do if your model has interaction or polynomial terms,
#'as these terms can introduce multi-collinearity and standardizing the numeric columns can reduce this problem.
#'
#'@param design The design matrix.
#'@param augmented Default `NULL`. If
#'@return Normalized run matrix
#'@keywords internal
normalize_design = function(design, augmented = NULL) {
  if (!is.null(augmented)) {
    for (column in 1:ncol(design)) {
      if (is.numeric(design[, column])) {
        midvalue = mean(c(
          max(c(design[, column], augmented[, column])),
          min(c(design[, column], augmented[, column]))
        ))
        design[, column] = (design[, column] - midvalue) /
          (max(c(design[, column], augmented[, column])) - midvalue)
      }
    }
  } else {
    for (column in 1:ncol(design)) {
      if (is.numeric(design[, column])) {
        midvalue = mean(c(max(design[, column]), min(design[, column])))
        design[, column] = (design[, column] - midvalue) /
          (max(design[, column]) - midvalue)
      }
    }
  }
  return(design)
}
