#'@title Calculate CI bounds on Monte Carlo
#'
#'@description Calculates CI bounds for Monte Carlo power
#'@return Power data.frame with conf intervals
#'@keywords internal
add_ci_bounds_mc_power = function(power_results, nsim, conf = 0.95) {
  get_lcbs = function(power) {
    return(unlist(lapply(
      power,
      \(x) (binom.test(x * nsim, n = nsim, conf.level = conf)[["conf.int"]][1])
    )))
  }
  get_ucbs = function(power) {
    return(unlist(lapply(
      power,
      \(x) (binom.test(x * nsim, n = nsim, conf.level = conf)[["conf.int"]][2])
    )))
  }
  lcb = get_lcbs(power_results[["power"]])
  ucb = get_ucbs(power_results[["power"]])
  power_results$power_lcb = lcb
  power_results$power_ucb = ucb
  return(power_results)
}
