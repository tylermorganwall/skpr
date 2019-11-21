#'@title Get optimality values
#'
#'@description Returns a list of optimality values (or one value in particular)
#'
#'@param output The output of either gen_design or eval_design/eval_design_mc
#'@param optimality Default `NULL`. Return just the specific optimality requested.
#'@return A dataframe of optimality conditions. `D`, `A`, and `G` are efficiencies (value is out of 100).
#'`T` is the trace of the information matrix, `E` is the minimum eigenvalue of the information matrix,
#'`I` is the average prediction variance, and `Alias` is the trace of the alias matrix.
#'@export
#'@examples
#'#We can pass either the output of gen_design or eval_design to plot_correlations
get_optimality = function(output, optimality = NULL) {
  if(is.null(attr(output, "D-Efficiency"))) attr(output, "D-Efficiency") = NA
  if(is.null(attr(output, "G"))) attr(output, "G") = NA
  if(is.null(attr(output, "T"))) attr(output, "T") = NA
  if(is.null(attr(output, "E"))) attr(output, "E") = NA
  if(is.null(attr(output, "variance.matrix"))) attr(output, "variance.matrix") = NA
  if(is.null(attr(output, "I"))) attr(output, "I") = NA
  if(is.null(attr(output, "trA"))) attr(output, "trA") = NA
  optimality_df = data.frame(D=attr(output, "D-Efficiency"),
                             I=attr(output, "I"),
                             A=attr(output, "A-Efficiency"),
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
      stop("Optimality `",optimality,"` not in ", paste0(c("D","A","I","G","E","T","Alias"),collapse=", "))
    }
    return(optimality_df[optimality])
  }
  return(optimality_df)
}
