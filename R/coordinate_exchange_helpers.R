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

#' Detect model-matrix columns that depend on each factor
#'
#' Uses deterministic probing: for each factor j, vary factor j across all of its
#' allowed CE levels and compare model-matrix columns over a deterministic covering
#' set of the other factors. Small grids use exhaustive Cartesian coverage. Larger
#' grids use a reduced covering set built from reference levels, edge levels, and
#' pairwise/all-high combinations so interaction columns are not masked by a single
#' zero/reference baseline. No RNG state is read or modified.
#'
#' @param modelmatrix_fn Default `NULL`. Function from skpr_ce_make_modelmatrix_fn().
#' @param factor_levels Default `NULL`. List of length q; each element numeric vector of allowed values/codes.
#' @param ndetect Deprecated compatibility argument; ignored.
#' @param tol_col Default `1e-12`. Column range threshold to call "varies".
#' @param seed Deprecated compatibility argument; ignored.
#' @param max_exhaustive_rows Default `5000L`. Maximum number of covering rows for exhaustive deterministic coverage.
#' @param max_reduced_rows Default `512L`. Maximum number of covering rows for reduced Cartesian coverage.
#' @param max_levels_per_factor Default `4L`. Maximum levels per factor retained before reducing to edge/reference levels.
#' @return List of length q; each element is an integer vector of 1-based model-matrix columns depending on that factor.
skpr_ce_detect_factor_columns = function(
	modelmatrix_fn,
	factor_levels,
	ndetect = 12L,
	tol_col = 1e-12,
	seed = 123,
	max_exhaustive_rows = 5000L,
	max_reduced_rows = 512L,
	max_levels_per_factor = 4L
) {
	if (!is.function(modelmatrix_fn)) {
		stop("skpr_ce_detect_factor_columns: modelmatrix_fn must be a function.")
	}
	if (!is.list(factor_levels) || length(factor_levels) == 0) {
		stop(
			"skpr_ce_detect_factor_columns: factor_levels must be a non-empty list."
		)
	}

	make_grid = function(levels_list) {
		if (length(levels_list) == 0) {
			return(matrix(numeric(0), nrow = 1, ncol = 0))
		}
		grid = expand.grid(
			levels_list,
			KEEP.OUT.ATTRS = FALSE,
			stringsAsFactors = FALSE
		)
		as.matrix(grid)
	}

	make_reduced_levels = function(lev) {
		lev = unique(as.numeric(lev))
		if (length(lev) <= max_levels_per_factor) {
			return(lev)
		}
		mid = lev[ceiling(length(lev) / 2)]
		unique(c(lev[[1]], mid, lev[[length(lev)]]))
	}

	make_cover = function(j) {
		other = setdiff(seq_len(q), j)
		if (length(other) == 0) {
			return(matrix(numeric(0), nrow = 1, ncol = 0))
		}

		other_levels = factor_levels[other]
		total_rows = prod(vapply(other_levels, length, numeric(1)))
		if (is.finite(total_rows) && total_rows <= max_exhaustive_rows) {
			return(make_grid(other_levels))
		}

		reduced_levels = lapply(other_levels, make_reduced_levels)
		reduced_rows = prod(vapply(reduced_levels, length, numeric(1)))
		if (is.finite(reduced_rows) && reduced_rows <= max_reduced_rows) {
			return(make_grid(reduced_levels))
		}

		ref = vapply(reduced_levels, function(lev) lev[[1]], numeric(1))
		rows = list(ref)

		for (a in seq_along(other)) {
			for (val in reduced_levels[[a]]) {
				row = ref
				row[[a]] = val
				rows[[length(rows) + 1L]] = row
			}
		}

		if (length(other) > 1) {
			for (a in seq_len(length(other) - 1L)) {
				for (b in seq.int(a + 1L, length(other))) {
					for (va in reduced_levels[[a]]) {
						for (vb in reduced_levels[[b]]) {
							row = ref
							row[[a]] = va
							row[[b]] = vb
							rows[[length(rows) + 1L]] = row
						}
					}
				}
			}
		}

		all_high = vapply(
			reduced_levels,
			function(lev) lev[[length(lev)]],
			numeric(1)
		)
		rows[[length(rows) + 1L]] = all_high
		unique(do.call(rbind, rows))
	}

	q = length(factor_levels)
	base = vapply(factor_levels, function(lev) lev[[1]], numeric(1))
	p = ncol(modelmatrix_fn(matrix(base, nrow = 1)))

	cols_by_factor = vector("list", q)

	for (j in seq_len(q)) {
		depends = rep(FALSE, p)
		levj = factor_levels[[j]]
		Lj = length(levj)
		if (Lj <= 1) {
			cols_by_factor[[j]] = integer(0)
			next
		}

		cover = make_cover(j)
		other = setdiff(seq_len(q), j)
		for (t in seq_len(nrow(cover))) {
			x0 = base
			if (length(other) > 0) {
				x0[other] = cover[t, ]
			}
			cand = matrix(rep(x0, each = Lj), nrow = Lj)
			cand[, j] = levj

			mm = modelmatrix_fn(cand)
			if (ncol(mm) != p) {
				stop(
					"skpr_ce_detect_factor_columns: modelmatrix_fn returned inconsistent ncol()."
				)
			}

			varies = apply(mm, 2, function(z) (max(z) - min(z)) > tol_col)
			depends = depends | varies
		}

		cols_by_factor[[j]] = which(depends)
	}

	names(cols_by_factor) = names(factor_levels)
	cols_by_factor
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
#' @return A data.frame with factor/numeric columns restored.
skpr_ce_decode_points = function(points_mat, factor_meta) {
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

	df = data.frame(matrix(nrow = nrow(points_mat), ncol = length(factor_names)))
	names(df) = factor_names

	for (j in seq_along(factor_names)) {
		nm = factor_names[[j]]
		meta = factor_meta[[nm]]
		x = points_mat[, j]
		if (meta$kind == "discrete") {
			lev = as.character(meta$levels)
			code = as.integer(round(x))
			if (any(is.na(code)) || any(code < 0L | code >= length(lev))) {
				stop("skpr_ce_decode_points: out-of-range code for factor: ", nm)
			}
			df[[nm]] = factor(lev[code + 1L], levels = lev)
		} else if (meta$kind == "numeric") {
			df[[nm]] = as.numeric(x)
		} else {
			stop("skpr_ce_decode_points: unknown kind for factor: ", nm)
		}
	}

	df
}

#' High-level preparation for constrained coordinate exchange
#'
#' Returns modelmatrix_fn and factor_columns ready to pass into
#' genOptimalDesignCoordinateExchangeConstrained().
#'
#' @param candidateset Default `NULL`. data.frame candidate set (skpr style).
#' @param model Default `NULL`. Model formula.
#' @param contrasts_fun Default `contr.simplex`. Contrast generator function.
#' @param ndetect Default `12L`. Baseline count for factor column detection.
#' @param tol_col Default `1e-12`. Column range threshold.
#' @param seed Default `123`. RNG seed.
#' @return List with factor_meta, factor_levels, modelmatrix_fn, factor_columns.
skpr_ce_prepare = function(
	candidateset,
	model,
	contrasts_fun = contr.simplex,
	ndetect = 12L,
	tol_col = 1e-12,
	seed = 123
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

	factor_columns = skpr_ce_detect_factor_columns(
		modelmatrix_fn = modelmatrix_fn,
		factor_levels = factor_levels,
		ndetect = ndetect,
		tol_col = tol_col,
		seed = seed
	)

	list(
		factor_meta = factor_meta,
		factor_levels = factor_levels,
		modelmatrix_fn = modelmatrix_fn,
		factor_columns = factor_columns
	)
}
