# Coordinate exchange: factor-space preparation and the R/C++ model boundary.
#
# This file translates between user-facing data frames, the numeric point
# matrices searched by C++, and model-matrix rows used to score a design.
# It also turns constraint dependencies or manual factor groups into coordinate
# groups. gen_design.R orchestrates normalization, constraint compilation, and
# repeats; the search itself is in the C++ coordinate-exchange engine.
#
# Data flow:
#   candidate data -> infer factor metadata and search grids
#   model + metadata + final grids -> R model.matrix -> small shared lookup tables
#   constraint IR or manual groups -> one-based factor-index groups for C++
#   initial runs -> encode numeric values / zero-based discrete codes -> C++
#   returned points + exact level codes -> decode factors and original-unit values
#   returned model values -> independent R verification before repeat selection
#
# Each table stores columns with the same raw-factor dependencies. Exchange groups
# reference their affected tables; cross-group interactions use current fixed
# levels as additional lookup keys. Models exceeding table limits use the callback.
#
# Numeric points retain the scale passed to these helpers (gen_design() supplies
# normalized values). Discrete point entries are zero-based positions in the
# metadata's level labels. Returned level_codes index every factor's grid,
# including numeric factors, so original values can be recovered without rounding
# a normalized value back to a label. Metadata, grids, and matrices must agree on
# factor order; original and search grids must also agree on level-code order.

# Model-matrix callback --------------------------------------------------------
# Capture the factor schema and contrast matrices once; reconstruct a data frame
# with that schema for each batch received from C++. During search, a batch holds
# one group's alternatives for one run. Later groups use earlier accepted moves;
# interactions are evaluated with the other factors held at their current values.

#' Validate the pointwise formula contract for coordinate exchange
#' @param model Model formula.
#' @param factor_names Names of the factors supplied in each point.
#' @return Formula with scalar coefficients frozen and calls bound to base functions.
#' @keywords internal
skpr_ce_validate_model = function(model, factor_names) {
  # Restrict evaluated calls to known pointwise base functions. Merely testing a
  # few batches cannot establish this property for an arbitrary R function.
  pointwise = c(
    "I",
    "(",
    "+",
    "-",
    "*",
    "/",
    "^",
    ":",
    "%%",
    "%/%",
    "==",
    "!=",
    "<",
    "<=",
    ">",
    ">=",
    "!",
    "&",
    "|",
    "%in%",
    "c",
    "abs",
    "sign",
    "sqrt",
    "exp",
    "expm1",
    "log",
    "log10",
    "log2",
    "log1p",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "atan2",
    "sinh",
    "cosh",
    "tanh",
    "asinh",
    "acosh",
    "atanh",
    "floor",
    "ceiling",
    "trunc",
    "round",
    "signif",
    "pmin",
    "pmax",
    "ifelse"
  )
  fail = function() {
    stop(
      "skpr: Coordinate exchange requires a fixed pointwise model. ",
      "Batch-dependent or unsupported functions are not allowed; use base ",
      "arithmetic/pointwise transforms, explicit fixed coefficients, or point exchange.",
      call. = FALSE
    )
  }
  formula_operators = c("(", "+", "-", "*", "/", "^", ":")
  walk = function(node, constant_vector = FALSE, formula_term = TRUE) {
    if (is.symbol(node)) {
      name = as.character(node)
      if (name %in% c(factor_names, ".")) {
        if (constant_vector) {
          fail()
        }
        return(node)
      }
      value = get0(name, envir = environment(model), inherits = TRUE)
      if (!is.atomic(value) || length(value) != 1L) {
        fail()
      }
      return(value)
    }
    if (!is.call(node)) {
      if (!is.atomic(node) || (!constant_vector && length(node) != 1L)) {
        fail()
      }
      return(node)
    }
    head = node[[1L]]
    qualified = is.call(head) &&
      identical(head[[1L]], as.name("::")) &&
      identical(head[[2L]], as.name("base"))
    if (qualified) {
      head = head[[3L]]
    }
    if (!is.symbol(head)) {
      fail()
    }
    name = as.character(head)
    if (!(name %in% pointwise) || (name == "c" && !constant_vector)) {
      fail()
    }
    # Colon denotes interactions in formula syntax, but constructs a sequence
    # from the first input values when evaluated inside a transform such as I().
    if (name == ":" && !formula_term && !constant_vector) {
      fail()
    }
    resolved = get(
      name,
      envir = if (qualified) baseenv() else environment(model),
      mode = "function",
      inherits = TRUE
    )
    if (!identical(resolved, get(name, envir = baseenv(), mode = "function"))) {
      fail()
    }
    node[[1L]] = head
    for (i in seq_along(node)[-1L]) {
      node[[i]] = walk(
        node[[i]],
        constant_vector || (name == "%in%" && i == 3L),
        formula_term && name %in% formula_operators
      )
    }
    node
  }
  model[[length(model)]] = walk(model[[length(model)]])
  # Freeze external scalar coefficients and bind validated calls to base functions.
  # Constant vectors are allowed only as membership sets, never recycled by row.
  environment(model) = baseenv()
  model
}

#' Build a model-matrix generator for coordinate exchange
#'
#' The returned function maps an (m x q) numeric matrix of factor values/codes to an
#' (m x p) numeric model matrix via model.matrix(), using the same contrast mechanism
#' as gen_design(). Formula calls must be supported pointwise base functions;
#' scalar coefficients, terms, and contrast matrices are fixed at preparation.
#'
#' Discrete factors are represented as 0-based integer codes in the input matrix.
#' Numeric factors are represented as their numeric values.
#'
#' @param model Model formula for model.matrix().
#' @param factor_meta Named list of factor metadata. Each entry is a list:
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

  model = skpr_ce_validate_model(model, factor_names)

  # Freeze contrast matrices once so batch size and mutable contrast functions
  # cannot change the coding between callback invocations.
  contrasts_arg = list()
  for (nm in factor_names) {
    meta = factor_meta[[nm]]
    if (is.null(meta$kind)) {
      stop("skpr_ce_make_modelmatrix_fn: factor_meta entries require kind.")
    }
    if (meta$kind == "discrete") {
      contrasts_arg[[nm]] = contrasts_fun(length(meta$levels))
    }
  }
  if (length(contrasts_arg) == 0) {
    contrasts_arg = NULL
  }

  schema = as.data.frame(setNames(
    rep(list(numeric()), length(factor_names)),
    factor_names
  ))
  model_terms = terms(model, data = schema)
  callback = function(points_mat, .table = FALSE) {
    if (!is.matrix(points_mat)) {
      points_mat = as.matrix(points_mat)
    }
    if (ncol(points_mat) != length(factor_names)) {
      stop(
        "skpr_ce_make_modelmatrix_fn: points_mat ncol does not match factor_meta length."
      )
    }

    # Reintroduce names and full factor levels, even when this batch contains
    # only a subset of levels, so the model columns keep a consistent schema.
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

    # Formula calls were validated as pointwise above, and terms/contrasts are
    # shared by every batch. C++ also checks the final matrix against fresh output.
    # Table preparation must retain rows with undefined expressions. A value in
    # an unrelated column may be undefined at the chosen reference settings.
    data = if (.table) {
      model.frame(model_terms, data = df, na.action = na.pass)
    } else {
      df
    }
    mm = if (is.null(contrasts_arg)) {
      model.matrix(model_terms, data = data)
    } else {
      suppressWarnings(model.matrix(
        model_terms,
        data = data,
        contrasts.arg = contrasts_arg
      ))
    }

    if (drop_intercept) {
      assignment = attr(mm, "assign")[-1L]
      mm = mm[, -1, drop = FALSE]
      attr(mm, "assign") = assignment
    }

    if (!.table && any(!is.finite(mm))) {
      stop(
        "skpr_ce_make_modelmatrix_fn: model.matrix produced non-finite entries (NA/NaN/Inf)."
      )
    }

    mm
  }
  attr(callback, "ce_terms") = model_terms
  callback
}

# Precomputed model tables ----------------------------------------------------
# R evaluates the complete model on small grids for each term's dependencies.
# C++ only indexes the resulting values. Tables spanning multiple exchange groups
# use the latest fixed level codes; they never change the exchange grouping.

#' Precompute coordinate-exchange model values on local factor grids
#' @param modelmatrix_fn Validated callback from skpr_ce_make_modelmatrix_fn.
#' @param factor_levels Named search-scale level grids in factor order.
#' @param evaluator Default `"auto"`. Use tables when possible, require `"tables"`, or force `"r"`.
#' @param max_bytes Default `33554432`. Maximum total numeric table storage.
#' @param max_rows Default `65536`. Maximum combinations in any one table.
#' @return Tables, evaluator choice, fallback reason, and preparation diagnostics.
#' @keywords internal
skpr_ce_prepare_model_tables = function(
  modelmatrix_fn,
  factor_levels,
  evaluator = "auto",
  max_bytes = 33554432,
  max_rows = 65536
) {
  if (
    length(evaluator) != 1L ||
      is.na(evaluator) ||
      !(evaluator %in% c("auto", "tables", "r"))
  ) {
    stop("skpr: ce_model_evaluator must be 'auto', 'tables', or 'r'.")
  }
  for (value in list(max_bytes, max_rows)) {
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        !is.finite(value) ||
        value < 1
    ) {
      stop("skpr: CE model table limits must be positive finite numbers.")
    }
  }
  start = proc.time()[[3L]]
  calls = 0L
  build = function() {
    model_terms = attr(modelmatrix_fn, "ce_terms")
    if (is.null(model_terms) || is.null(names(factor_levels))) {
      stop(
        "table preparation requires a validated model and named factor grids"
      )
    }
    factor_names = names(factor_levels)
    if (any(lengths(factor_levels) == 0L)) {
      stop("empty factor grid")
    }
    base = vapply(factor_levels, function(x) x[[1L]], numeric(1))
    evaluate = function(points) {
      calls <<- calls + 1L
      suppressWarnings(modelmatrix_fn(points, .table = TRUE))
    }
    probe = evaluate(matrix(base, nrow = 1L))
    assignment = attr(probe, "assign")
    if (
      nrow(probe) != 1L || !ncol(probe) || length(assignment) != ncol(probe)
    ) {
      stop("model schema cannot be tabulated")
    }
    variables = as.list(attr(model_terms, "variables"))[-1L]
    incidence = attr(model_terms, "factors")
    dependencies = lapply(assignment, function(term) {
      if (term == 0L) {
        return(integer())
      }
      used = which(incidence[, term] != 0L)
      referenced = unique(unlist(lapply(variables[used], all.vars)))
      which(factor_names %in% referenced)
    })
    keys = vapply(dependencies, paste, character(1), collapse = ",")
    columns = split(seq_along(keys), keys)
    row_counts = vapply(
      columns,
      function(cols) {
        prod(lengths(factor_levels[dependencies[[cols[[1L]]]]]))
      },
      numeric(1)
    )
    bytes = 8 * sum(row_counts * lengths(columns))
    if (
      !is.finite(bytes) ||
        bytes > max_bytes ||
        any(row_counts > min(max_rows, .Machine$integer.max))
    ) {
      stop("model tables exceed the configured storage or row limit")
    }
    blocks = lapply(seq_along(columns), function(b) {
      cols = columns[[b]]
      support = dependencies[[cols[[1L]]]]
      sizes = lengths(factor_levels[support])
      strides = cumprod(c(1, sizes))[seq_along(sizes)]
      values = matrix(NA_real_, row_counts[[b]], length(cols))
      # Bound the temporary full model matrix, independently of stored table size.
      chunk_size = max(
        1L,
        min(4096L, floor(1048576 / max(ncol(probe), length(base))))
      )
      for (first in seq.int(1, row_counts[[b]], by = chunk_size)) {
        rows = seq.int(first, min(first + chunk_size - 1L, row_counts[[b]]))
        points = matrix(rep(base, each = length(rows)), ncol = length(base))
        for (j in seq_along(support)) {
          code = ((rows - 1) %/% strides[[j]]) %% sizes[[j]]
          points[, support[[j]]] = factor_levels[[support[[j]]]][code + 1L]
        }
        mm = evaluate(points)
        if (
          nrow(mm) != length(rows) ||
            !identical(colnames(mm), colnames(probe)) ||
            !identical(attr(mm, "assign"), assignment)
        ) {
          stop("model schema changed during table preparation")
        }
        values[rows, ] = mm[, cols, drop = FALSE]
      }
      list(
        factors = as.integer(support - 1L),
        columns = as.integer(cols - 1L),
        values = values
      )
    })
    list(
      version = 1L,
      factor_levels = unname(factor_levels),
      columns = colnames(probe),
      blocks = blocks,
      bytes = bytes
    )
  }
  result = if (evaluator == "r") NULL else tryCatch(build(), error = identity)
  reason = if (inherits(result, "error")) conditionMessage(result) else NULL
  if (!is.null(reason) && evaluator == "tables") {
    stop("skpr: Cannot precompute CE model tables: ", reason, call. = FALSE)
  }
  if (!is.null(reason)) {
    result = NULL
  }
  list(
    tables = result,
    evaluator = if (is.null(result)) "r" else "tables",
    fallback_reason = reason,
    preparation_seconds = proc.time()[[3L]] - start,
    preparation_calls = calls
  )
}

# Repeat execution -------------------------------------------------------------

#' Run one coordinate-exchange repeat with consistent error capture
#' @param repeat_index Repeat number, including for failure diagnostics.
#' @param config Prepared grids, model callback, constraints, and search controls.
#' @keywords internal
skpr_ce_run_repeat = function(repeat_index, config) {
  tryCatch(
    {
      if (repeat_index %in% config$controls$.ce_test_fail_repeats) {
        stop("skpr: injected coordinate-exchange repeat failure for testing.")
      }
      points = matrix(0, config$trials, length(config$factor_levels))
      for (j in seq_along(config$factor_levels)) {
        levels = config$factor_levels[[j]]
        points[, j] = levels[sample.int(
          length(levels),
          config$trials,
          replace = TRUE
        )]
      }
      if (!is.null(config$augment_points)) {
        points[seq_len(nrow(config$augment_points)), ] = config$augment_points
      }
      result = genOptimalDesignCoordinateExchangeConstrained(
        points = points,
        factor_levels = config$factor_levels,
        modelmatrix_fn = config$modelmatrix_fn,
        coordinate_groups = config$coordinate_groups,
        constraints_ir = config$constraints_ir,
        tolerance = config$tolerance,
        kexchange = config$kexchange,
        augmentedrows = config$augmentedrows,
        max_iter = config$controls$ce_max_iter,
        recompute_every = config$controls$ce_recompute_every,
        repair_stuck_limit = config$controls$ce_repair_stuck_limit,
        repair_max_tries = config$controls$ce_repair_max_tries,
        coordinate_group_max_candidates = config$controls$coordinate_group_max_candidates,
        model_tables = config$model_plan$tables
      )
      # Independent R audit happens after native search, before repeat selection.
      # The C++ lookup evaluator never calls R, including initialization/repair.
      if (!is.null(config$model_plan$tables)) {
        verified = config$modelmatrix_fn(result$points)
        if (
          !identical(dim(verified), dim(result$model_matrix)) ||
            !isTRUE(all.equal(
              as.vector(verified),
              as.vector(result$model_matrix),
              tolerance = 1e-12
            ))
        ) {
          stop(
            "skpr: precomputed CE model values disagree with model.matrix()."
          )
        }
      }
      result$diagnostics$model_verification_calls = as.integer(
        !is.null(config$model_plan$tables)
      )
      result$diagnostics$model_preparation_seconds = config$model_plan$preparation_seconds
      result$diagnostics$model_preparation_calls = config$model_plan$preparation_calls
      result$diagnostics$model_fallback_reason = config$model_plan$fallback_reason
      list(
        indices = seq_len(config$trials),
        model_matrix = result$model_matrix,
        criterion = if (is.finite(result$criterion)) {
          result$criterion
        } else {
          NA_real_
        },
        points = result$points,
        level_codes = result$level_codes,
        diagnostics = result$diagnostics
      )
    },
    error = identity
  )
}

# Coordinate groups ------------------------------------------------------------
# A group is a set of factors C++ may change jointly in a single run. Build
# connected components from the compiler's support edges using union/find;
# factors untouched by those edges remain singleton groups.
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

# Manual groups use factor names. Reject ambiguous assignments, then append
# omitted factors as singleton groups so every factor is searched exactly once.
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

# Resolve the public option into both readable names for reporting and one-based
# integer indices for the C++ boundary (which converts them to zero-based).
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

# Factor schema and point encoding ---------------------------------------------
# Keep labels in metadata and use numeric matrices at the C++ boundary. Numeric
# grids are sorted; factor label ordering is preserved when levels are declared.

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

# Encoding maps discrete labels to their metadata positions and copies numeric
# values as supplied. It does not perform normalization or numeric grid snapping.

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

# Decoding can either preserve the search-scale numeric points or, when exact
# level codes and an original grid are supplied, recover original-unit values.
# gen_design() uses both forms for model diagnostics and the returned run table.

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

# Convenience preparation ------------------------------------------------------
# Bundle the schema, grids, and callback for direct engine callers. The public
# gen_design() path performs additional normalization, overrides, constraint
# compilation, and group resolution around the same lower-level helpers.

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
