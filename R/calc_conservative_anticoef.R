#'@title Calculate Conservative Anticipated Coefficients
#'
#'@description Calculate Conservative Anticipated Coefficients
#'@param results The power results matrix
#'@param effectsize The effectsize.
#'@return Vector of conservative anticipated coefficients
#'@keywords internal
calc_conservative_anticoef = function(results, effectsize) {
  groupvars = attr(attr(results, "model_matrix"), "assign")
  uniquevars = unique(groupvars)
  orderedunique = uniquevars[order(uniquevars)]
  parresults = results[results$type == "parameter.power", ]
  parresults$variable = groupvars
  conservative_anticoef = c()

  for (var in orderedunique) {
    powers = parresults$power[parresults$variable == var]
    if (length(powers) == 1) {
      conservative_anticoef = c(conservative_anticoef, 1)
    }
    if (length(powers) > 1) {
      if (length(which(abs(powers - min(powers)) < 1E-10)) == 1) {
        coefvec = rep(0, length(powers))
        coefvec[which.min(powers)] = 1
        conservative_anticoef = c(conservative_anticoef, coefvec)
      }
      if (length(which(abs(powers - min(powers)) < 1E-10)) > 1) {
        numberofequal = length(which(abs(powers - min(powers)) < 1E-10))
        exponents = 1:numberofequal + 1
        values = rep(-1, numberofequal)^exponents
        if (numberofequal > 2) {
          values[3:length(values)] = 0
        }
        coefvec = rep(0, length(powers))
        coefvec[which(abs(powers - min(powers)) < 1E-10)] = values
        conservative_anticoef = c(conservative_anticoef, coefvec)
      }
    }
  }
  conservative_anticoef = conservative_anticoef * effectsize / 2
  return(conservative_anticoef)
}
