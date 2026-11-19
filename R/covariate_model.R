#' Resolve the fitted terms for a fixed-covariate design
#'
#' @param model Formula for the planned fit.
#' @param data Treatment and fixed columns in returned design order for dot expansion.
#' @return An R terms object with the fitted terms and their original factor coding.
#' @keywords internal
#' @noRd
covariate_design_terms = function(model, data) {
  fitted = terms(model, data = data, simplify = TRUE)
  factors = attr(fitted, "factors")
  if (!length(factors)) return(fitted)

  # terms() retains subtracted variables in its model frame. The exchange
  # matrices need only the variables with coefficients and any explicit offsets.
  offsets = attr(fitted, "offset")
  unused = setdiff(which(rowSums(factors) == 0L), c(attr(fitted, "response"), offsets))
  if (length(unused)) {
    attr(fitted, "variables") = attr(fitted, "variables")[-(unused + 1L)]
    attr(fitted, "factors") = factors[-unused, , drop = FALSE]
    if (length(offsets)) {
      attr(fitted, "offset") = match(offsets, seq_len(nrow(factors))[-unused])
    }
  }
  return(fitted)
}

#' Build a fitted model matrix for observed or predicted runs
#'
#' @param model Resolved fitted terms.
#' @param points Data frame of complete treatment assignments.
#' @param reference Original ranges and levels for the treatment and fixed columns.
#' @param contrastslist Named contrasts for categorical predictors.
#' @return Model matrix in the same coding as the exchange search.
#' @keywords internal
#' @noRd
covariate_assignment_matrix = function(model, points, reference, contrastslist) {
  variables = intersect(names(points), all.vars(model))
  points = normalize_prediction_points(points[variables], reference, model)
  coding = contrastslist[intersect(names(contrastslist), variables)]
  return(model.matrix(model, points, contrasts.arg = coding))
}

#' Encode a model for the fixed-column exchange
#'
#' Splits the normalized design into the columns fixed at each run and the
#' columns the optimizer can exchange. Treatments share one candidate matrix
#' unless a model term also depends on a covariate. In that case, only distinct
#' profiles of the covariates in the mixed terms need separate candidate rows.
#'
#' @param model Expanded generating or alias formula.
#' @param treatments Treatment candidate or integration grid on its original scale.
#' @param covariates Run-ordered covariates on their original scale.
#' @param contrastslist Named contrast specifications for categorical variables.
#' @param reference Default `treatments`. Treatment ranges and factor levels used to code the design.
#' @return Fixed and exchangeable model matrices, zero-based candidate offsets
#'   by run, column names, and fitted terms for coding new prediction rows.
#' @keywords internal
#' @noRd
covariate_model = function(model, treatments, covariates, contrastslist, reference = treatments) {
  variables = all.vars(model)
  treatment_names = intersect(setdiff(variables, names(covariates)), names(reference))
  if (!all(treatment_names %in% names(treatments)) || !nrow(treatments)) {
    stop("skpr: The treatment grid must contain candidate points for all treatment factors in the model.")
  }
  treatments = normalize_prediction_points(treatments[treatment_names], reference, model)
  covariates = covariates[intersect(names(covariates), variables)]
  covariates = normalize_prediction_points(covariates, covariates, model)
  term_vars = lapply(attr(terms(model), "term.labels"), function(term) {
    return(all.vars(str2lang(term)))
  })
  uses_treatment = vapply(term_vars, function(vars) {
    return(any(vars %in% names(treatments)))
  }, logical(1))
  mixed_vars = intersect(unlist(term_vars[uses_treatment]), names(covariates))

  # Group on integer level codes so labels containing separators stay distinct.
  group = rep.int(1L, nrow(covariates))
  if (length(mixed_vars)) {
    codes = lapply(covariates[mixed_vars], function(x) return(match(x, unique(x))))
    key = do.call(paste, c(codes, sep = "/"))
    group = match(key, unique(key))
  }

  # Evaluate single-source expressions on all their observations. Keep the
  # fitted prediction call so poly(position, 2) uses this basis on a new grid.
  model = terms(model)
  prediction = attr(model, "predvars")
  if (is.null(prediction)) prediction = attr(model, "variables")
  prediction_model = model
  prediction_calls = prediction
  expressions = as.list(attr(model, "variables"))[-1L]
  for (i in which(vapply(expressions, is.call, logical(1)))) {
    expr = expressions[[i]]
    variables = all.vars(expr)
    label = rownames(attr(model, "factors"))[i]
    if (!any(variables %in% names(treatments))) {
      value = eval(prediction[[i + 1L]], covariates, environment(model))
      covariates[[label]] = value
    } else if (!any(variables %in% names(covariates))) {
      value = eval(prediction[[i + 1L]], treatments, environment(model))
      treatments[[label]] = value
    } else {
      next
    }
    prediction_calls[[i + 1L]] = makepredictcall(value, expr)
    prediction[[i + 1L]] = as.name(label)
  }
  attr(model, "predvars") = prediction
  attr(prediction_model, "predvars") = prediction_calls
  contrastslist = contrastslist[intersect(names(contrastslist), all.vars(model))]
  fixed = model.matrix(model, cbind(
    covariates, treatments[rep(1L, nrow(covariates)), , drop = FALSE]
  ), contrasts.arg = contrastslist)
  # assign == 0 is the intercept, which is fixed along with pure covariate terms.
  varies = c(FALSE, uses_treatment)[attr(fixed, "assign") + 1L]
  candidate_matrices = lapply(which(!duplicated(group)), function(i) {
    data = cbind(covariates[rep(i, nrow(treatments)), , drop = FALSE], treatments)
    return(model.matrix(model, data, contrasts.arg = contrastslist)[, varies, drop = FALSE])
  })
  return(list(
    fixed = fixed[, !varies, drop = FALSE],
    candidates = do.call(rbind, candidate_matrices),
    offsets = (group - 1L) * nrow(treatments),
    count = nrow(treatments),
    columns = colnames(fixed),
    categorical_columns = categorical_model_columns(model, fixed),
    prediction_model = prediction_model
  ))
}

#' Average prediction moments over the fixed runs and treatment candidates
#'
#' Forms the prediction-region moment matrix used by I-optimality. Each fixed
#' run has its observed weight and each treatment point has equal weight.
#'
#' @param parts Fixed-column model encoding from covariate_model.
#' @return Moment matrix in the optimizer's fixed-then-exchangeable column order.
#' @keywords internal
#' @noRd
covariate_moments = function(parts) {
  n_candidates = parts$count
  n_runs = nrow(parts$fixed)
  fixed_moment = crossprod(parts$fixed) / n_runs
  mixed_moment = matrix(0, ncol(parts$fixed), ncol(parts$candidates))
  treatment_moment = matrix(0, ncol(parts$candidates), ncol(parts$candidates))
  for (offset in unique(parts$offsets)) {
    runs = parts$offsets == offset
    candidates = parts$candidates[offset + seq_len(n_candidates), , drop = FALSE]
    mixed_moment = mixed_moment + tcrossprod(
      colSums(parts$fixed[runs, , drop = FALSE]), colMeans(candidates)
    ) / n_runs
    treatment_moment = treatment_moment + crossprod(candidates) *
      (sum(runs) / (n_runs * n_candidates))
  }
  moment = rbind(cbind(fixed_moment, mixed_moment), cbind(t(mixed_moment), treatment_moment))
  columns = c(colnames(parts$fixed), colnames(parts$candidates))
  dimnames(moment) = list(columns, columns)
  return(zero_categorical_moments(moment, parts$categorical_columns))
}

#' Start one fixed-column exchange search
#'
#' Samples treatment indices for one restart of either a split-plot or a
#' covariate design. Serial and parallel searches use the same row mapping.
#'
#' @param inputs Prepared arguments for genSplitPlotOptimalDesign.
#' @return The optimizer's treatment indices, model matrix and criterion.
#' @keywords internal
#' @noRd
search_fixed_columns = function(inputs) {
  runs = nrow(inputs$blockeddesign)
  covariate_rows = inputs$covariateRows
  count = nrow(inputs$candidatelist)
  offsets = 0L
  alias_offsets = 0L
  if (!is.null(covariate_rows)) {
    count = covariate_rows$candidate_count
    offsets = covariate_rows$offsets
    alias_offsets = covariate_rows$alias_offsets
  }
  indices = sample.int(count, runs, replace = runs > count)
  inputs$initialRows = indices
  inputs$initialdesign = inputs$candidatelist[indices + offsets, , drop = FALSE]
  inputs$aliasdesign = inputs$aliascandidatelist[indices + alias_offsets, , drop = FALSE]
  return(do.call(genSplitPlotOptimalDesign, inputs))
}

#' Evaluate prediction variance at admissible treatment assignments
#'
#' Reconstructs requested prediction points from the run-specific fixed columns
#' and the corresponding treatment rows, in the fitted model's column order.
#'
#' @param parts Model encoding from covariate_model.
#' @param inverse_information Inverse information matrix with model column names.
#' @param runs Covariate row indices, one per prediction point.
#' @param treatments Treatment row indices, one per prediction point.
#' @return Prediction variance at each requested point.
#' @keywords internal
#' @noRd
covariate_prediction_variance = function(parts, inverse_information, runs, treatments) {
  points = cbind(parts$fixed[runs, , drop = FALSE],
                 parts$candidates[parts$offsets[runs] + treatments, , drop = FALSE])
  points = points[, colnames(inverse_information), drop = FALSE]
  return(rowSums((points %*% inverse_information) * points))
}

#' Code prediction points on the scale of the design search
#'
#' Applies the reference ranges and categorical levels from the search or
#' evaluation before constructing model matrices for candidate and fixed rows.
#'
#' @param points Data frame of points to encode.
#' @param reference Named columns supplying the numeric ranges and factor levels.
#' @param model Default `NULL`. Supply the fixed-covariate model to preserve
#'   original units where its formula requires them. Ordinary searches scale all numeric inputs.
#' @return Scaled numeric and factor columns in their input order.
#' @keywords internal
#' @noRd
normalize_prediction_points = function(points, reference, model = NULL) {
  original_scale = character()
  if (!is.null(model)) {
    fitted = terms(model)
    # Centering without an intercept or a lower-order term can introduce a
    # different fitted effect. R marks incomplete interaction margins with 2.
    if (!attr(fitted, "intercept") || any(attr(fitted, "factors") == 2L)) {
      original_scale = all.vars(fitted)
    } else {
      expressions = as.list(attr(fitted, "variables"))[-1L]
      original_scale = unlist(lapply(Filter(is.call, expressions), all.vars), use.names = FALSE)
    }
  }
  for (name in names(points)) {
    values = reference[[name]]
    if (is.numeric(values)) {
      if (!is.numeric(points[[name]])) stop("skpr: Expected numeric values for `", name, "`.")
      if (!(name %in% original_scale)) {
        limits = range(values)
        midpoint = mean(limits)
        points[[name]] = (points[[name]] - midpoint) / (limits[2L] - midpoint)
      }
      if (any(!is.finite(points[[name]]))) stop("skpr: Invalid numeric values for `", name, "`.")
    } else {
      if (!is.factor(values)) values = factor(values)
      points[[name]] = factor(points[[name]], levels = levels(values))
      if (anyNA(points[[name]])) stop("skpr: Missing or unknown levels for `", name, "`.")
    }
  }
  return(points)
}

# Adapt the public unblocked callback to the existing fixed-column callback.
covariate_custom_criterion = function(callback, columns) {
  force(callback)
  force(columns)
  function(currentDesign, vInv) callback(currentDesign[, columns, drop = FALSE])
}
