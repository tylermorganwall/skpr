#'@title Calculate Effect Power
#'
#'@description Calculates the effect power given the anticipated coefficients and the type-I error
#'
#'
#'@param RunMatrix The model matrix
#'@param levelvector The number of levels in each parameter (1st is always the intercept)
#'@param anticoef The anticipated coefficients
#'@param alpha the specified type-I error
#'@param vinv The V inverse matrix
#'@param degrees Degrees of freedom
#'@return The effect power for the parameters
#'@keywords internal
effectpower = function(
  RunMatrix,
  levelvector,
  anticoef,
  alpha,
  vinv = NULL,
  degrees = NULL
) {
  L = replicate(length(levelvector), matrix(NA, nrow = 0, ncol = 0))

  g = priorlevels(levelvector)

  for (i in 1:(length(g) - 1)) {
    L[[i]] = genparammatrix(
      dim(attr(RunMatrix, "model.matrix"))[2],
      levelvector[i],
      g[i]
    )
  }
  if (is.null(degrees)) {
    degrees = rep(
      dim(attr(RunMatrix, "model.matrix"))[1] -
        dim(attr(RunMatrix, "model.matrix"))[2],
      length(L)
    )
  } else {
    degrees[is.na(degrees)] = dim(attr(RunMatrix, "model.matrix"))[1] -
      dim(attr(RunMatrix, "model.matrix"))[2]
  }
  power = c(length(L))
  for (j in 1:length(L)) {
    if (degrees[j] != 0) {
      power[j] = calculatepower(
        attr(RunMatrix, "model.matrix"),
        L[[j]],
        calcnoncentralparam(
          attr(RunMatrix, "model.matrix"),
          L[[j]],
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
