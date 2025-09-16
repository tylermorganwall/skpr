#'@title Get attribute values
#'
#'@description Returns one or more of underlying attributes used in design generation/evaluation
#'
#'@param output The output of either [gen_design()], [eval_design()], or [eval_design_mc()].
#'@param attr Default `NULL`. Return just the specific value requested.
#'Potential values are `model_matrix` for model used, `moments.matrix`, `variance.matrix`, `alias.matrix`,
#'`correlation.matrix`, and `model` for the model used in the evaluation/generation of the design.
#'@param round Default `TRUE`. Rounds off values smaller than the magnitude `1e-15`` in the `correlation.matrix` and `alias.matrix` matrix attributes.
#'@return A list of attributes.
#'@export
#'@examples
#'# We can extract the attributes of a design from either the output of `gen_design()`
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
#'get_attribute(designcoffee)
#'
#'#Get just one attribute
#'get_attribute(designcoffee,"model_matrix")
#'
#'# Extract from `eval_design()` output
#'power_output = eval_design(designcoffee, model = ~cost + size + type,
#'                           alpha = 0.05, detailedoutput = TRUE)
#'
#'get_attribute(power_output,"correlation.matrix")
get_attribute = function(output, attr = NULL, round = TRUE) {
  attr_list = list()
  attr_list[["model_matrix"]] = attr(output, "model_matrix")
  attr_list[["moments.matrix"]] = attr(output, "moments.matrix")
  attr_list[["variance.matrix"]] = attr(output, "variance.matrix")
  if (!is.null(attr(output, "alias.matrix"))) {
    attr_list[["alias.matrix"]] = attr(output, "alias.matrix")
  }
  attr_list[["correlation.matrix"]] = attr(output, "correlation.matrix")
  attr_list[["model"]] = attr(output, "generating_model")
  if (round) {
    attr_list[["correlation.matrix"]][
      abs(attr_list[["correlation.matrix"]]) < 1e-15
    ] = 0
    if (!is.null(attr_list[["alias.matrix"]])) {
      attr_list[["alias.matrix"]][abs(attr_list[["alias.matrix"]]) < 1e-15] = 0
    }
  }

  if (!is.null(attr) && !is.null(attr_list[[attr]])) {
    if (
      !attr %in%
        c(
          "model_matrix",
          "moments.matrix",
          "variance.matrix",
          "alias.matrix",
          "correlation.matrix",
          "model"
        )
    ) {
      stop(
        "skpr: attribute `",
        attr,
        "` not in ",
        paste0(
          c(
            "model_matrix",
            "moments.matrix",
            "variance.matrix",
            "alias.matrix",
            "correlation.matrix",
            "model"
          ),
          collapse = ", "
        )
      )
    }
    return(attr_list[[attr]])
  }
  attr_list
}
