#'@title Normalize Design
#'
#'@description Normalizes the numeric columns in the design to -1 to 1. This is important to do if your model has interaction or polynomial terms,
#'as these terms can introduce multi-collinearity and standardizing the numeric columns can reduce this problem.
#'
#'@param design The design matrix.
#'@param augmented Default `NULL`. If augmenting an existing design, this should be the pre-existing design. The column types must match design
#'
#'@return Normalized design matrix
#'@export
#'@examples
#'#Normalize a design
#'if(skpr:::run_documentation()) {
#'cand_set = expand.grid(temp = c(100,300,500),
#'                       altitude = c(10000,20000),
#'                       offset = seq(-10,-5,by=1),
#'                       type = c("A","B", "C"))
#'design = gen_design(cand_set, ~., 24)
#'
#'#Un-normalized design
#'design
#'}
#'if(skpr:::run_documentation()) {
#'#Normalized design
#'normalize_design(design)
#'}
normalize_design = function(design, augmented = NULL) {
  if (!is.null(augmented)) {
    all_equal_classes = all(identical(
      lapply(design, class),
      unlist(lapply(augmented, class))
    ))
    if (!all_equal_classes) {
      stop(
        "Design to be augmented and new design must have identical column classes"
      )
    }
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
