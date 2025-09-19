#'@title Calculate G Efficiency
#'
#'@description Either calculates G-Efficiency by Monte Carlo sampling from the design space (ignoring constraints),
#'searching for the maximum point (slower but higher quality), or by using a user-specified candidate set
#'for the design space (fastest).
#'@return Normalized run matrix
#'@keywords internal
calculate_gefficiency = function(
  design,
  calculation_type = "random",
  randsearches = 10000,
  design_space_mm = NULL
) {
  if (!is.null(attr(design, "runmatrix"))) {
    run_matrix = attr(design, "runmatrix")
  } else {
    run_matrix = design
  }
  if (is.null(randsearches)) {
    randsearches = 10000
  }
  model = attr(design, "generating_model")
  variables = all.vars(model)
  designmm = attr(design, "model_matrix")
  modelentries = names(calculate_level_vector(
    run_matrix,
    model,
    FALSE
  ))
  model_entries_mul = gsub(":", "*", modelentries)
  model_entries_mul[1] = 1
  factorvars = attr(design, "contrastslist")
  variables = variables[!variables %in% names(factorvars)]
  rand_vector = function(factorlist, model_entries_mul) {
    fulloutput = unlist(lapply(
      model_entries_mul,
      \(x) eval(parse(text = x), envir = factorlist)
    ))
    matrix(fulloutput, 1, length(fulloutput))
  }
  invXtX = solve(t(designmm) %*% designmm)
  calculate_optimality_for_point = function(x, infval = FALSE) {
    fulllist = list()
    if (any(x > 1) || any(x < -1)) {
      if (infval) {
        return(Inf)
      } else {
        return(10000)
      }
    }
    for (i in seq_len(length(x))) {
      fulllist[[i]] = x[i]
    }
    names(fulllist) = variables
    for (i in seq_len(length(names(factorvars)))) {
      numberlevels = length(levels(run_matrix[[names(factorvars)[i]]]))
      fulllist[[names(factorvars)[i]]] = factorvars[[names(factorvars)[i]]](
        numberlevels
      )[sample(seq_len(numberlevels), 1), , drop = TRUE]
    }

    mc_mm = rand_vector(fulllist, model_entries_mul)
    return(
      as.numeric(mc_mm %*% invXtX %*% t(mc_mm))
    )
  }
  modelentries = names(calculate_level_vector(
    run_matrix,
    get_attribute(design, "model"),
    FALSE
  ))
  factorvars = attr(design, "contrastslist")
  variables = all.vars(get_attribute(design, "model"))
  variables = variables[!variables %in% names(factorvars)]

  #First run edges
  candset = attr(design, "candidate_set")
  for (i in seq_len(ncol(candset))) {
    if (is.numeric(candset[[i]])) {
      num_col = candset[[i]]
      min_max = range(candset)
      is_exterior = num_col %in% min_max
      candset = candset[is_exterior, ]
    }
  }
  contrastslist = attr(design, "contrastslist")
  candset_mm = model.matrix(model, data = candset, contrasts = contrastslist)
  max_spv_verts = max(candset_mm %*% invXtX %*% t(candset_mm))

  if (calculation_type == "random") {
    vals = list()
    for (i in seq_len(randsearches)) {
      vals[[i]] = calculate_optimality_for_point(
        2 * runif(length(variables)) - 1
      )
    }
    max_interior_val = max(unlist(vals))
    max_val_overall = max(max_spv_verts, max_interior_val)
    return(100 * ncol(designmm) / (nrow(designmm) * max_val_overall))
  }

  if (calculation_type == "optim") {
    vals = list()
    for (i in seq_len(randsearches)) {
      vals[[i]] = optim(
        2 * runif(length(variables)) - 1,
        calculate_optimality_for_point,
        method = "SANN"
      )$value
    }
    return(100 * ncol(designmm) / (nrow(designmm) * max(unlist(vals))))
  }
  if (calculation_type == "custom" && !is.null(design_space_mm)) {
    return(GEfficiency(designmm, design_space_mm))
  }
}
