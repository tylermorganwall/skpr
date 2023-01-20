#'@title Get optimality values
#'
#'@description Returns a list of optimality values (or one value in particular).
#'
#'Note: The choice of contrast will effect the `G` efficiency value, and `gen_design()`
#'and `eval_design()` by default set different contrasts (`contr.simplex` vs `contr.sum`).
#'
#'@param output The output of either gen_design or eval_design/eval_design_mc.
#'@param optimality Default `NULL`. Return just the specific optimality requested.
#'@return A dataframe of optimality conditions. `D`, `A`, and `G` are efficiencies (value is out of 100).
#'`T` is the trace of the information matrix, `E` is the minimum eigenvalue of the information matrix,
#'`I` is the average prediction variance, and `Alias` is the trace of the alias matrix.
#'@export
#'@examples
#'# We can extract the optimality of a design from either the output of `gen_design()`
#'# or the output of `eval_design()`
#'
#'factorialcoffee = expand.grid(cost = c(1, 2),
#'                              type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
#'                              size = as.factor(c("Short", "Grande", "Venti")))
#'
#'designcoffee = gen_design(factorialcoffee, ~cost + size + type, trials = 29,
#'                          optimality = "D", repeats = 100)
#'
#'#Extract a list of all attributes
#'get_optimality(designcoffee)
#'
#'#Get just one attribute
#'get_optimality(designcoffee,"D")
#'
#'# Extract from `eval_design()` output
#'power_output = eval_design(designcoffee, model = ~cost + size + type,
#'                           alpha = 0.05, detailedoutput = TRUE)
#'
#'get_optimality(power_output)
get_optimality = function(output, optimality = NULL) {
  if(is.null(attr(output, "D"))) attr(output, "D") = NA
  if(is.null(attr(output, "A"))) attr(output, "A") = NA
  if(!is.null(attr(output, "augmented"))) {
    if(!attr(output, "augmented") && !attr(output, "splitplot") ) {
      attr(output, "G") = calculate_gefficiency(output, calculation_type = "random",
                                                randsearches = 1000)
    } else {
      attr(output, "G") = "Not Computed"
    }
  } else {
    if(!attr(output, "blocking")) {
      attr(output, "G") = calculate_gefficiency(output, calculation_type = "random",
                                                randsearches = 1000)
    } else {
      attr(output, "G") = "Not Computed"
    }
  }
  if(is.null(attr(output, "G"))) attr(output, "G") = "Not Computed"
  if(is.null(attr(output, "T"))) attr(output, "T") = NA
  if(is.null(attr(output, "E"))) attr(output, "E") = NA
  if(is.null(attr(output, "variance.matrix"))) attr(output, "variance.matrix") = NA
  if(is.null(attr(output, "I"))) attr(output, "I") = NA
  if(is.null(attr(output, "trA"))) attr(output, "trA") = NA
  optimality_df = data.frame(D=attr(output, "D"),
                             I=attr(output, "I"),
                             A=attr(output, "A"),
                             G=attr(output, "G"),
                             T=attr(output, "T"),
                             E=attr(output, "E"),
                             Alias = attr(output, "trA"))
  if(!is.null(optimality)) {
    if(optimality == "alias") {
      optimality = "Alias"
    }
    if(optimality == "D-Efficiency") {
      optimality = "D"
    }
    if(optimality == "A-Efficiency") {
      optimality = "A"
    }
    if(!optimality %in% c("D","A","I","G","E","T","Alias")) {
      stop("skpr: Optimality `",optimality,"` not in ", paste0(c("D","A","I","G","E","T","Alias"),collapse=", "))
    }
    return(optimality_df[optimality])
  }
  return(optimality_df)
}
