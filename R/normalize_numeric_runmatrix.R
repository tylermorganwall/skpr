#'@title Normalize Run Matrix
#'
#'@description Normalizes the numeric columns in the runmatrix
#'@param RunMatrix The run matrix
#'@return Normalized run matrix
#'@keywords internal
normalize_numeric_runmatrix = function(RunMatrix) {
  for (column in 1:ncol(RunMatrix)) {
    if (is.numeric(RunMatrix[, column])) {
      midvalue = mean(c(max(RunMatrix[, column]), min(RunMatrix[, column])))
      RunMatrix[, column] = (RunMatrix[, column] - midvalue) / (max(RunMatrix[, column]) - midvalue)
    }
  }
  return(RunMatrix)
}
