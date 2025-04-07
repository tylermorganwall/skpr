#'@title Calculates parameter power
#'
#'@description Calculates parameter power
#'
#'@param RunMatrix The run matrix
#'@param levelvector The number of levels in each parameter (1st is always the intercept)
#'@param anticoef The anticipated coefficients
#'@param alpha the specified type-I error
#'@param vinv The V inverse matrix
#'@return The parameter power for the parameters
#'@keywords internal
parameterpower = function(
  RunMatrix,
  levelvector = NULL,
  anticoef,
  alpha,
  vinv = NULL,
  degrees = NULL,
  parameter_names
) {
  #Generating the parameter isolating vectors
  q = vector("list", dim(attr(RunMatrix, "model.matrix"))[2])
  for (i in 1:length(q)) {
    vec = rep(0, dim(attr(RunMatrix, "model.matrix"))[2])
    vec[i] = 1
    q[[i]] = t(vec)
  }
  if (is.null(degrees)) {
    degrees = rep(
      dim(attr(RunMatrix, "model.matrix"))[1] -
        dim(attr(RunMatrix, "model.matrix"))[2],
      length(q)
    )
  } else {
    degrees_long = rep(0, length(q))
    for (i in 1:length(parameter_names)) {
      for (term in parameter_names[[i]]) {
        degrees_long[which(
          term == colnames(attr(RunMatrix, "model.matrix"))
        )] = degrees[i]
      }
    }
    degrees_long[is.na(degrees_long)] = dim(attr(RunMatrix, "model.matrix"))[1] -
      dim(attr(RunMatrix, "model.matrix"))[2]
    degrees = degrees_long
  }

  power = c(length(q))
  for (j in 1:length(q)) {
    if (degrees[j] != 0) {
      power[j] = calculatepower(
        attr(RunMatrix, "model.matrix"),
        q[[j]],
        calcnoncentralparam(
          attr(RunMatrix, "model.matrix"),
          q[[j]],
          anticoef,
          vinv = vinv
        ),
        alpha,
        degrees[j]
      )
    } else {
      power[j] = NA
    }
  }
  return(power)
}
