#'@title Prior levels
#'
#'@description Returns number of levels prior to each parameter
#'
#'@param Levelvector Returns a vector consisting of the number
#'@keywords internal
#'@return Returns a vector consisting of the number
#'of levels preceeding each parameter (including the intercept)
priorlevels = function(levelvector) {
  sum = 0
  priors = c(0)
  for (x in 1:length(levelvector)) {
    sum = sum + levelvector[x]
    priors = c(priors,sum)
  }
  return(priors)
}
