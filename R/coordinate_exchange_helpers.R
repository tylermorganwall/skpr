#' Build a model-matrix generator for coordinate exchange
#'
#' The returned function maps an (m x q) numeric matrix of factor values/codes to an
#' (m x p) numeric model matrix via model.matrix(), using the same contrast mechanism
#' as gen_design().
#'
#' Discrete factors are represented as 0-based integer codes in the input matrix.
#' Numeric factors are represented as their numeric values.
#'
#' @param model Default `NULL`. Model formula for model.matrix().
#' @param factor_meta Default `NULL`. Named list of factor metadata. Each entry is a list:
#'   - kind: "discrete" or "numeric"
#'   - levels: for discrete, the level labels (character) in code order 0..L-1
#' @param contrasts_fun Default `contr.simplex`. Contrast generator function used for discrete factors.
#' @param drop_intercept Default `FALSE`. If TRUE, drops the intercept column from the model matrix.
#' @return A function(points_mat) -> model matrix.
#' @keywords internal
skpr_ce_make_modelmatrix_fn = function(
  model,
  factor_meta,
  contrasts_fun = contr.simplex,
  drop_intercept = FALSE
) {
  if (is.null(model)) {
    stop("skpr_ce_make_modelmatrix_fn: model is required.")
  }
  if (is.null(factor_meta) || length(factor_meta) == 0) {
    stop("skpr_ce_make_modelmatrix_fn: factor_meta is required.")
  }

  factor_names = names(factor_meta)
  if (is.null(factor_names) || any(factor_names == "")) {
    stop("skpr_ce_make_modelmatrix_fn: factor_meta must be named.")
  }

  # Build contrasts.arg like gen_design(): list of contrast functions keyed by factor name.
  contrasts_arg = list()
  for (nm in factor_names) {
    meta = factor_meta[[nm]]
    if (is.null(meta$kind)) {
      stop("skpr_ce_make_modelmatrix_fn: factor_meta entries require kind.")
    }
    if (meta$kind == "discrete") {
      contrasts_arg[[nm]] = contrasts_fun
    }
  }
  if (length(contrasts_arg) == 0) {
    contrasts_arg = NULL
  }

  function(points_mat) {
    if (!is.matrix(points_mat)) {
      points_mat = as.matrix(points_mat)
    }
    if (ncol(points_mat) != length(factor_names)) {
      stop(
        "skpr_ce_make_modelmatrix_fn: points_mat ncol does not match factor_meta length."
      )
    }

    df = as.data.frame(points_mat, stringsAsFactors = FALSE)
    names(df) = factor_names

    for (nm in factor_names) {
      meta = factor_meta[[nm]]
      if (meta$kind == "discrete") {
        if (is.null(meta$levels)) {
          stop(
            "skpr_ce_make_modelmatrix_fn: discrete factor missing levels: ",
            nm
          )
        }
        lev = as.character(meta$levels)
        idx = as.integer(df[[nm]]) + 1L
        if (any(is.na(idx))) {
          stop("skpr_ce_make_modelmatrix_fn: NA code in discrete factor: ", nm)
        }
        if (any(idx < 1L | idx > length(lev))) {
          stop(
            "skpr_ce_make_modelmatrix_fn: code out of range for factor: ",
            nm
          )
        }
        df[[nm]] = factor(lev[idx], levels = lev)
      } else if (meta$kind == "numeric") {
        df[[nm]] = as.numeric(df[[nm]])
      } else {
        stop("skpr_ce_make_modelmatrix_fn: unknown kind for factor: ", nm)
      }
    }

    mm = if (is.null(contrasts_arg)) {
      model.matrix(model, data = df)
    } else {
      suppressWarnings(model.matrix(
        model,
        data = df,
        contrasts.arg = contrasts_arg
      ))
    }

    if (drop_intercept) {
      mm = mm[, -1, drop = FALSE]
    }

    if (any(!is.finite(mm))) {
      stop(
        "skpr_ce_make_modelmatrix_fn: model.matrix produced non-finite entries (NA/NaN/Inf)."
      )
    }

    mm
  }
}

skpr_ce_auto_coordinate_groups = function(
  factor_names,
  constraints_ir = NULL
) {
  q = length(factor_names)
  if (q == 0L) {
    return(list())
  }
  if (
    is.null(factor_names) || any(is.na(factor_names)) || any(factor_names == "")
  ) {
    stop(
      "skpr_ce_auto_coordinate_groups: factor_names must be non-empty names."
    )
  }

  parent = seq_len(q)
  find = function(x) {
    while (parent[[x]] != x) {
      parent[[x]] <<- parent[[parent[[x]]]]
      x = parent[[x]]
    }
    x
  }
  union = function(a, b) {
    ra = find(a)
    rb = find(b)
    if (ra != rb) {
      parent[[rb]] <<- ra
    }
  }

  edges = if (is.null(constraints_ir)) {
    list()
  } else {
    constraint_ir_support_edges(constraints_ir)
  }

  for (edge in edges) {
    edge = sort(unique(as.integer(edge)))
    if (length(edge) <= 1L) {
      next
    }
    if (any(is.na(edge)) || any(edge < 1L | edge > q)) {
      stop(
        "skpr_ce_auto_coordinate_groups: constraint support index out of range."
      )
    }
    for (k in seq.int(2L, length(edge))) {
      union(edge[[1L]], edge[[k]])
    }
  }

  roots = vapply(seq_len(q), find, integer(1))
  groups = split(seq_len(q), roots)
  groups = lapply(groups, as.integer)
  groups = groups[order(vapply(groups, function(g) g[[1L]], integer(1)))]
  unname(groups)
}

skpr_ce_validate_manual_coordinate_groups = function(
  coordinate_groups,
  factor_names
) {
  if (!is.list(coordinate_groups)) {
    stop(
      "skpr: advancedoptions$coordinate_groups must be a list of character vectors."
    )
  }
  if (
    is.null(factor_names) || any(is.na(factor_names)) || any(factor_names == "")
  ) {
    stop(
      "skpr_ce_validate_manual_coordinate_groups: factor_names must be non-empty names."
    )
  }

  seen = character()
  out = vector("list", length(coordinate_groups))
  for (i in seq_along(coordinate_groups)) {
    group = coordinate_groups[[i]]
    if (!is.character(group)) {
      stop(
        "skpr: advancedoptions$coordinate_groups must be a list of character vectors."
      )
    }
    if (length(group) == 0L) {
      stop(
        "skpr: advancedoptions$coordinate_groups cannot contain empty groups."
      )
    }
    if (any(is.na(group)) || any(group == "")) {
      stop(
        "skpr: advancedoptions$coordinate_groups contains invalid factor names."
      )
    }
    unknown = setdiff(group, factor_names)
    if (length(unknown) > 0L) {
      stop(
        "skpr: advancedoptions$coordinate_groups contains unknown factor(s): ",
        paste(unknown, collapse = ", ")
      )
    }
    dup_within = unique(group[duplicated(group)])
    if (length(dup_within) > 0L) {
      stop(
        "skpr: advancedoptions$coordinate_groups repeats factor(s) within a group: ",
        paste(dup_within, collapse = ", ")
      )
    }
    dup_across = intersect(group, seen)
    if (length(dup_across) > 0L) {
      stop(
        "skpr: advancedoptions$coordinate_groups assigns factor(s) to multiple groups: ",
        paste(dup_across, collapse = ", ")
      )
    }
    seen = c(seen, group)
    out[[i]] = group
  }

  omitted = factor_names[!(factor_names %in% seen)]
  if (length(omitted) > 0L) {
    out = c(out, as.list(omitted))
  }

  out
}

skpr_ce_resolve_coordinate_groups = function(
  factor_names,
  constraints_ir = NULL,
  coordinate_groups = NULL
) {
  if (is.null(coordinate_groups)) {
    groups = if (is.null(constraints_ir)) {
      as.list(seq_along(factor_names))
    } else {
      skpr_ce_auto_coordinate_groups(factor_names, constraints_ir)
    }
  } else if (identical(coordinate_groups, "auto")) {
    groups = skpr_ce_auto_coordinate_groups(factor_names, constraints_ir)
  } else if (is.list(coordinate_groups)) {
    group_names = skpr_ce_validate_manual_coordinate_groups(
      coordinate_groups,
      factor_names
    )
    groups = lapply(group_names, function(g) match(g, factor_names))
  } else {
    stop(
      "skpr: advancedoptions$coordinate_groups must be NULL, 'auto', or a list of character vectors."
    )
  }

  group_names = lapply(groups, function(g) factor_names[g])
  list(
    coordinate_group_names = group_names,
    coordinate_groups = lapply(groups, as.integer)
  )
}

#' Infer factor metadata and levels from a candidate set (skpr-style)
#'
#' Discrete columns are factors/characters. Numeric columns are numeric.
#' Discrete levels are taken from factor levels (preferred) or sorted unique character values.
#'
#' For coordinate exchange, discrete factors are encoded as 0..L-1 codes. The returned
#' factor_levels list uses those codes for discrete factors.
#'
#' @param candidateset Default `NULL`. data.frame of candidate points (as in gen_design()).
#' @param factor_names Default `NULL`. Optional character vector to restrict/reorder factors.
#' @return List with factor_meta and factor_levels.
#' @keywords internal
skpr_ce_infer_factor_space = function(candidateset, factor_names = NULL) {
  if (!is.data.frame(candidateset)) {
    stop("skpr_ce_infer_factor_space: candidateset must be a data.frame.")
  }

  if (is.null(factor_names)) {
    factor_names = names(candidateset)
  } else {
    if (!all(factor_names %in% names(candidateset))) {
      stop(
        "skpr_ce_infer_factor_space: factor_names not all present in candidateset."
      )
    }
  }

  factor_meta = list()
  factor_levels = list()

  for (nm in factor_names) {
    x = candidateset[[nm]]
    if (inherits(x, c("factor", "character"))) {
      lev = if (is.factor(x)) levels(x) else sort(unique(as.character(x)))
      factor_meta[[nm]] = list(kind = "discrete", levels = as.character(lev))
      factor_levels[[nm]] = seq(0, length(lev) - 1)
    } else {
      factor_meta[[nm]] = list(kind = "numeric")
      factor_levels[[nm]] = sort(unique(as.numeric(x)))
    }
  }

  list(factor_meta = factor_meta, factor_levels = factor_levels)
}

#' Encode a data.frame of runs into coordinate-exchange point encoding
#'
#' Discrete factors become 0-based integer codes in code order defined by factor_meta$levels.
#' Numeric factors remain numeric.
#'
#' @param df Default `NULL`. data.frame with the same columns as factor_meta.
#' @param factor_meta Default `NULL`. From skpr_ce_infer_factor_space() or user-supplied.
#' @return Numeric matrix (n x q) suitable for coordinate exchange.
#' @keywords internal
skpr_ce_encode_points = function(df, factor_meta) {
  if (!is.data.frame(df)) {
    stop("skpr_ce_encode_points: df must be a data.frame.")
  }
  factor_names = names(factor_meta)
  if (is.null(factor_names) || any(factor_names == "")) {
    stop("skpr_ce_encode_points: factor_meta must be named.")
  }
  if (!all(factor_names %in% names(df))) {
    stop("skpr_ce_encode_points: df missing required columns.")
  }

  n = nrow(df)
  q = length(factor_names)
  out = matrix(0, nrow = n, ncol = q)
  colnames(out) = factor_names

  for (j in seq_len(q)) {
    nm = factor_names[j]
    meta = factor_meta[[nm]]

    if (meta$kind == "discrete") {
      lev = as.character(meta$levels)
      x = df[[nm]]
      x = if (is.factor(x)) as.character(x) else as.character(x)
      pos = match(x, lev)
      if (any(is.na(pos))) {
        stop("skpr_ce_encode_points: df has unknown level(s) for factor: ", nm)
      }
      out[, j] = pos - 1L
    } else if (meta$kind == "numeric") {
      out[, j] = as.numeric(df[[nm]])
    } else {
      stop("skpr_ce_encode_points: unknown kind for factor: ", nm)
    }
  }

  out
}

#' Decode coordinate-exchange points into a design data.frame
#'
#' Inverse of skpr_ce_encode_points(). Discrete factors are converted from
#' 0-based codes back to factors with the original levels from factor_meta.
#' Numeric factors are returned as numeric.
#'
#' @param points_mat Default `NULL`. Numeric matrix (n x q) of encoded points.
#' @param factor_meta Default `NULL`. Named metadata list as used by encode.
#' @param level_codes Default `NULL`. Optional integer matrix of exact level codes.
#' @param factor_levels_original Default `NULL`. Optional original-unit level table.
#' @return A data.frame with factor/numeric columns restored.
#' @keywords internal
skpr_ce_decode_points = function(
  points_mat,
  factor_meta,
  level_codes = NULL,
  factor_levels_original = NULL
) {
  if (!is.matrix(points_mat)) {
    points_mat = as.matrix(points_mat)
  }
  factor_names = names(factor_meta)
  if (is.null(factor_names) || any(factor_names == "")) {
    stop("skpr_ce_decode_points: factor_meta must be named.")
  }
  if (ncol(points_mat) != length(factor_names)) {
    stop(
      "skpr_ce_decode_points: points_mat ncol does not match factor_meta length."
    )
  }
  if (!is.null(level_codes)) {
    level_codes = as.matrix(level_codes)
    if (!is.integer(level_codes)) {
      storage.mode(level_codes) = "integer"
    }
    if (!identical(dim(level_codes), dim(points_mat)) || anyNA(level_codes)) {
      stop("skpr_ce_decode_points: level_codes dimensions are invalid.")
    }
  }
  if (!is.null(factor_levels_original)) {
    if (
      !is.list(factor_levels_original) ||
        !all(factor_names %in% names(factor_levels_original))
    ) {
      stop(
        "skpr_ce_decode_points: factor_levels_original must cover every factor."
      )
    }
  }

  df = data.frame(matrix(nrow = nrow(points_mat), ncol = length(factor_names)))
  names(df) = factor_names

  for (j in seq_along(factor_names)) {
    nm = factor_names[[j]]
    meta = factor_meta[[nm]]
    x = points_mat[, j]
    code = if (is.null(level_codes)) as.integer(round(x)) else level_codes[, j]
    if (meta$kind == "discrete") {
      lev = as.character(meta$levels)
      if (any(is.na(code)) || any(code < 0L | code >= length(lev))) {
        stop("skpr_ce_decode_points: out-of-range code for factor: ", nm)
      }
      df[[nm]] = factor(lev[code + 1L], levels = lev)
    } else if (meta$kind == "numeric") {
      if (is.null(factor_levels_original)) {
        df[[nm]] = as.numeric(x)
      } else {
        lev = as.numeric(factor_levels_original[[nm]])
        if (any(!is.finite(lev)) || any(code < 0L | code >= length(lev))) {
          stop(
            "skpr_ce_decode_points: invalid numeric level code for factor: ",
            nm
          )
        }
        df[[nm]] = lev[code + 1L]
      }
    } else {
      stop("skpr_ce_decode_points: unknown kind for factor: ", nm)
    }
  }

  df
}

#' High-level preparation for constrained coordinate exchange
#'
#' Returns factor metadata, levels, and modelmatrix_fn ready to pass into
#' genOptimalDesignCoordinateExchangeConstrained().
#'
#' @param candidateset Default `NULL`. data.frame candidate set (skpr style).
#' @param model Default `NULL`. Model formula.
#' @param contrasts_fun Default `contr.simplex`. Contrast generator function.
#' @return List with factor_meta, factor_levels, and modelmatrix_fn.
#' @keywords internal
skpr_ce_prepare = function(
  candidateset,
  model,
  contrasts_fun = contr.simplex
) {
  space = skpr_ce_infer_factor_space(candidateset)
  factor_meta = space$factor_meta
  factor_levels = space$factor_levels

  modelmatrix_fn = skpr_ce_make_modelmatrix_fn(
    model = model,
    factor_meta = factor_meta,
    contrasts_fun = contrasts_fun,
    drop_intercept = FALSE
  )

  list(
    factor_meta = factor_meta,
    factor_levels = factor_levels,
    modelmatrix_fn = modelmatrix_fn
  )
}
