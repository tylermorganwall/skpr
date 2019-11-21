#'@title Get attribute values
#'
#'@description Returns one or more of underlying attributes used in design generation/evaluation
#'
#'@param output The output of either `gen_design()` or `eval_design()`/`eval_design_mc()``
#'@param attr Default `NULL`. Return just the specific value requested.
#'Potential values are `model.matrix` for model used, `moments.matrix`, `variance.matrix`, `alias.matrix`,
#'`correlation.matrix`, and `model` for the model used in the evaluation/generation of the design.
#'@param round Default `TRUE`. Rounds off values smaller than the magnitude `1e-15`` in the `correlation.matrix` and `alias.matrix` matrix attributes.
#'@return A list of attributes.
#'@export
#'@examples
#'#We can pass either the output of gen_design or eval_design to plot_correlations
get_attribute = function(output, attr = NULL, round = TRUE) {
  attr_list = list()
  attr_list[["model.matrix"]] = attr(output,"model.matrix")
  attr_list[["moments.matrix"]] = attr(output,"moments.matrix")
  attr_list[["variance.matrix"]] = attr(output,"variance.matrix")
  attr_list[["alias.matrix"]] = attr(output,"alias.matrix")
  attr_list[["correlation.matrix"]] = attr(output,"correlation.matrix")
  attr_list[["model"]] = attr(output,"generating.model")
  if(round) {
    attr_list[["correlation.matrix"]][abs(attr_list[["correlation.matrix"]]) < 1e-15] = 0
    attr_list[["alias.matrix"]][abs(attr_list[["alias.matrix"]]) < 1e-15] = 0
  }

  if(!is.null(attr)) {
    if(!attr %in% c("model.matrix","moments.matrix","variance.matrix","alias.matrix","correlation.matrix","model")) {
      stop("attribute `",attr,"` not in ",
           paste0(c("model.matrix","moments.matrix","variance.matrix","alias.matrix","correlation.matrix","model"),collapse=", "))
    }
    return(attr_list[[attr]])
  }
  attr_list
}
