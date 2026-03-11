make_level_pos_matrix = function(points, factor_levels) {
	q = ncol(points)
	out = matrix(0L, nrow = nrow(points), ncol = q)
	for (j in seq_len(q)) {
		lev = as.numeric(factor_levels[[j]])
		out[, j] = match(points[, j], lev) - 1L
	}
	storage.mode(out) = "integer"
	out
}

build_constraint_affine_map = function(
	original_df,
	normalized_df,
	factor_meta,
	tol = 1e-10
) {
	offset = rep(0, length(factor_meta))
	scale = rep(1, length(factor_meta))
	names(offset) = names(factor_meta)
	names(scale) = names(factor_meta)

	for (nm in names(factor_meta)) {
		if (factor_meta[[nm]]$kind != "numeric") {
			next
		}
		norm_range = range(as.numeric(normalized_df[[nm]]), na.rm = TRUE)
		orig_range = range(as.numeric(original_df[[nm]]), na.rm = TRUE)
		norm_span = diff(norm_range)
		if (!is.finite(norm_span) || abs(norm_span) <= tol) {
			scale[[nm]] = 0
			offset[[nm]] = mean(orig_range)
		} else {
			s = diff(orig_range) / norm_span
			scale[[nm]] = s
			offset[[nm]] = orig_range[[1]] - s * norm_range[[1]]
		}
	}

	list(offset = unname(as.numeric(offset)), scale = unname(as.numeric(scale)))
}

forbidden_rows_mask = function(df, forbidden_tables) {
	mask = rep(FALSE, nrow(df))
	for (tab in forbidden_tables) {
		tab_df = as.data.frame(tab, stringsAsFactors = FALSE)
		if (nrow(tab_df) == 0 || ncol(tab_df) == 0) {
			next
		}
		for (i in seq_len(nrow(tab_df))) {
			row_match = rep(TRUE, nrow(df))
			for (nm in names(tab_df)) {
				rhs = tab_df[[nm]][i]
				lhs = df[[nm]]
				if (is.factor(lhs) || is.character(lhs)) {
					row_match = row_match & (as.character(lhs) == as.character(rhs))
				} else {
					row_match = row_match & (as.numeric(lhs) == as.numeric(rhs))
				}
			}
			mask = mask | row_match
		}
	}
	mask
}

test_that("compile_constraints + allowed matches brute force on small grid", {
	cand = expand.grid(
		x1 = c(-1, 0, 1),
		x2 = c(-1, 1),
		f1 = factor(c("A", "B")),
		f2 = factor(c("L", "M")),
		KEEP.OUT.ATTRS = FALSE,
		stringsAsFactors = FALSE
	)
	cand$f1 = factor(cand$f1)
	cand$f2 = factor(cand$f2)

	space = skpr_ce_infer_factor_space(cand)
	factor_meta = space$factor_meta
	factor_levels = space$factor_levels
	points = skpr_ce_encode_points(cand, factor_meta)
	level_pos = make_level_pos_matrix(points, factor_levels)

	filter_expr = quote((x1 >= 0 & f1 %in% c("A")) | !(f1 == "B" & f2 == "M"))
	forbidden = list(
		data.frame(f1 = "A", f2 = "L", stringsAsFactors = FALSE),
		data.frame(x1 = 1, x2 = -1)
	)
	ir = compile_constraints(
		filter_expr = filter_expr,
		forbidden_tuples = forbidden,
		factor_meta = factor_meta,
		factor_levels = factor_levels
	)

	allowed_cpp = as.logical(skpr_constraints_allowed(points, level_pos, ir))
	allowed_r = eval(filter_expr, cand, parent.frame()) &
		!forbidden_rows_mask(
			cand,
			forbidden
		)
	expect_identical(allowed_cpp, as.logical(allowed_r))
})

test_that("allowed_change matches brute-force reevaluation", {
	cand = expand.grid(
		x1 = c(-1, 0, 1),
		x2 = c(-1, 1),
		f1 = factor(c("A", "B")),
		f2 = factor(c("L", "M")),
		KEEP.OUT.ATTRS = FALSE,
		stringsAsFactors = FALSE
	)
	cand$f1 = factor(cand$f1)
	cand$f2 = factor(cand$f2)

	space = skpr_ce_infer_factor_space(cand)
	factor_meta = space$factor_meta
	factor_levels = space$factor_levels
	points = skpr_ce_encode_points(cand, factor_meta)
	level_pos = make_level_pos_matrix(points, factor_levels)

	ir = compile_constraints(
		filter_expr = quote((x1 + x2 <= 1) & !(f1 == "B" & f2 == "M")),
		forbidden_tuples = list(data.frame(
			f1 = "A",
			f2 = "L",
			stringsAsFactors = FALSE
		)),
		factor_meta = factor_meta,
		factor_levels = factor_levels
	)

	for (i in seq_len(nrow(points))) {
		row_values = points[i, ]
		row_codes = level_pos[i, ]
		for (j in seq_len(ncol(points))) {
			lev = as.numeric(factor_levels[[j]])
			for (k in seq_along(lev)) {
				new_value = lev[[k]]
				new_code = as.integer(k - 1L)

				ok_change = as.logical(skpr_constraints_allowed_change(
					row_values = row_values,
					row_codes = row_codes,
					constraints_ir = ir,
					var1 = j,
					new_value = new_value,
					new_code = new_code
				))[[1]]

				row_values2 = row_values
				row_codes2 = row_codes
				row_values2[[j]] = new_value
				row_codes2[[j]] = new_code
				ok_bruteforce = as.logical(skpr_constraints_allowed(
					points = matrix(row_values2, nrow = 1),
					level_pos = matrix(as.integer(row_codes2), nrow = 1),
					constraints_ir = ir
				))[[1]]

				expect_identical(ok_change, ok_bruteforce)
			}
		}
	}
})

test_that("original-scale constraints evaluate correctly on normalized CE points", {
	cand = expand.grid(
		x1 = c(-1, -0.5, 0, 0.5, 1),
		x2 = c(-1, 0, 1),
		f1 = factor(c("A", "B")),
		KEEP.OUT.ATTRS = FALSE,
		stringsAsFactors = FALSE
	)
	cand$f1 = factor(cand$f1)
	cand_norm = normalize_design(cand)

	space_norm = skpr_ce_infer_factor_space(cand_norm)
	space_orig = skpr_ce_infer_factor_space(cand)

	points_norm = skpr_ce_encode_points(cand_norm, space_norm$factor_meta)
	level_pos_norm = make_level_pos_matrix(points_norm, space_norm$factor_levels)

	filter_expr = quote((x1 + x2 <= -0.4) & !(f1 == "B" & x1 > 0))
	ir = compile_constraints(
		filter_expr = filter_expr,
		factor_meta = space_orig$factor_meta,
		factor_levels = space_orig$factor_levels
	)
	map = build_constraint_affine_map(cand, cand_norm, space_norm$factor_meta)
	ir$value_offset = map$offset
	ir$value_scale = map$scale

	allowed_cpp = as.logical(skpr_constraints_allowed(
		points_norm,
		level_pos_norm,
		ir
	))
	allowed_r = as.logical(eval(filter_expr, cand, parent.frame()))
	expect_identical(allowed_cpp, allowed_r)

	for (i in seq_len(min(nrow(points_norm), 8L))) {
		row_values = points_norm[i, ]
		row_codes = level_pos_norm[i, ]
		for (j in seq_len(ncol(points_norm))) {
			lev = as.numeric(space_norm$factor_levels[[j]])
			for (k in seq_along(lev)) {
				new_value = lev[[k]]
				new_code = as.integer(k - 1L)

				ok_change = as.logical(skpr_constraints_allowed_change(
					row_values = row_values,
					row_codes = row_codes,
					constraints_ir = ir,
					var1 = j,
					new_value = new_value,
					new_code = new_code
				))[[1]]

				row_values2 = row_values
				row_codes2 = row_codes
				row_values2[[j]] = new_value
				row_codes2[[j]] = new_code
				ok_bruteforce = as.logical(skpr_constraints_allowed(
					points = matrix(row_values2, nrow = 1),
					level_pos = matrix(as.integer(row_codes2), nrow = 1),
					constraints_ir = ir
				))[[1]]

				expect_identical(ok_change, ok_bruteforce)
			}
		}
	}
})

test_that("DNF expansion warning recommends forbidden tuple tables", {
	cand = expand.grid(
		x1 = c(-1, 0, 1),
		x2 = c(-1, 1),
		KEEP.OUT.ATTRS = FALSE
	)
	space = skpr_ce_infer_factor_space(cand)

	expect_warning(
		compile_filter_ir(
			filter_expr = quote(
				(x1 == -1 | x1 == 0 | x1 == 1) & (x2 == -1 | x2 == 1)
			),
			factor_meta = space$factor_meta,
			factor_levels = space$factor_levels,
			max_clauses = 10L
		),
		"consider forbidden tuple tables"
	)
})

# tests/testthat/test-constraints-random-hypercube.R

context("constraints randomized hypercube filtering")

make_df_from_points = function(points, factor_meta) {
	df = as.data.frame(points, stringsAsFactors = FALSE)
	names(df) = names(factor_meta)

	for (nm in names(factor_meta)) {
		meta = factor_meta[[nm]]
		if (meta$kind == "discrete") {
			lev = as.character(meta$levels)
			idx = as.integer(df[[nm]]) + 1L
			if (any(is.na(idx)) || any(idx < 1L | idx > length(lev))) {
				stop("make_df_from_points: discrete code out of range for factor: ", nm)
			}
			df[[nm]] = factor(lev[idx], levels = lev)
		} else if (meta$kind == "numeric") {
			df[[nm]] = as.numeric(df[[nm]])
		} else {
			stop("make_df_from_points: unknown kind for factor: ", nm)
		}
	}

	df
}

level_pos_from_points = function(points, factor_levels, tol = 1e-12) {
	q = ncol(points)
	n = nrow(points)
	out = matrix(0L, nrow = n, ncol = q)

	for (j in seq_len(q)) {
		lev = as.numeric(factor_levels[[j]])
		for (i in seq_len(n)) {
			d = abs(lev - points[i, j])
			k = which.min(d)
			if (length(k) != 1L || d[k] > tol) {
				stop(
					"level_pos_from_points: point value not in factor_levels within tol."
				)
			}
			out[i, j] = k - 1L
		}
	}

	out
}

eval_forbidden_table = function(df, forbidden, factor_meta) {
	if (is.null(forbidden) || nrow(forbidden) == 0) {
		return(rep(FALSE, nrow(df)))
	}

	forbidden = as.data.frame(forbidden, stringsAsFactors = FALSE)
	cols = names(forbidden)
	if (is.null(cols) || any(cols == "")) {
		stop("eval_forbidden_table: forbidden must have named columns.")
	}
	if (!all(cols %in% names(df))) {
		stop("eval_forbidden_table: forbidden columns not in df.")
	}

	hit = rep(FALSE, nrow(df))
	for (r in seq_len(nrow(forbidden))) {
		match_r = rep(TRUE, nrow(df))
		for (nm in cols) {
			meta = factor_meta[[nm]]
			v = forbidden[[nm]][r]

			if (meta$kind == "discrete") {
				v_chr = as.character(v)
				match_r = match_r & (as.character(df[[nm]]) == v_chr)
			} else {
				v_num = as.numeric(v)
				match_r = match_r & (as.numeric(df[[nm]]) == v_num)
			}
		}
		hit = hit | match_r
	}

	hit
}

sample_points_hypercube = function(n, factor_meta, factor_levels) {
	nm = names(factor_meta)
	q = length(nm)
	points = matrix(0, nrow = n, ncol = q)
	colnames(points) = nm

	for (j in seq_len(q)) {
		lev = factor_levels[[nm[j]]]
		points[, j] = sample(lev, n, replace = TRUE)
	}

	points
}

test_that("Random hypercube points: C++ filtering matches R evaluation", {
	set.seed(1)

	factor_meta = list(
		x1 = list(kind = "numeric"),
		x2 = list(kind = "numeric"),
		x3 = list(kind = "numeric"),
		A = list(kind = "discrete", levels = c("low", "mid", "high")),
		B = list(kind = "discrete", levels = c("x", "y"))
	)

	grid21 = seq(-1, 1, length.out = 21)
	factor_levels = list(
		x1 = grid21,
		x2 = grid21,
		x3 = grid21,
		A = 0:2,
		B = 0:1
	)

	# Thousands of points in a hypercube (sampled from level grids/codes)
	n = 8000
	points = sample_points_hypercube(n, factor_meta, factor_levels)

	# Constraints include:
	# - negated inequality (exercises invert_op via NNF)
	# - linear constraint
	# - membership
	# - discrete conjunction with negation
	# - disjunction
	c1 = sample(grid21, 1)
	c2 = sample(grid21, 1)

	filter_expr = substitute(
		(!(x1 <= c1) &
			(x1 + 2 * x2 - x3 <= 0.25) &
			(x2 %in% c(-1, 0, 1)) &
			!(A == "high" & B == "y")) |
			(x3 >= c2 & A != "low"),
		list(c1 = c1, c2 = c2)
	)

	# Forbidden tuple table (common user path)
	forbidden = data.frame(
		A = c("high", "low", "mid"),
		B = c("x", "y", "x"),
		stringsAsFactors = FALSE
	)

	ir = compile_constraints(
		filter_expr = filter_expr,
		forbidden_tuples = forbidden,
		factor_meta = factor_meta,
		factor_levels = factor_levels,
		tol = 1e-10
	)

	df = make_df_from_points(points, factor_meta)

	allowed_R_expr = eval(filter_expr, envir = df)
	forbidden_hit = eval_forbidden_table(df, forbidden, factor_meta)
	allowed_R = as.logical(allowed_R_expr) & !forbidden_hit

	level_pos = level_pos_from_points(points, factor_levels)
	allowed_cpp = skpr_constraints_allowed(points, level_pos, ir)

	expect_equal(as.logical(allowed_cpp), as.logical(allowed_R))

	# Directional safety check (your stated criterion):
	kept = points[as.logical(allowed_cpp), , drop = FALSE]
	if (nrow(kept) > 0) {
		df_kept = make_df_from_points(kept, factor_meta)
		ok_expr = eval(filter_expr, envir = df_kept)
		ok_forb = eval_forbidden_table(df_kept, forbidden, factor_meta)
		expect_true(all(as.logical(ok_expr) & !ok_forb))
	}
})

test_that("Many random constraint instances: C++ filtering matches R evaluation", {
	set.seed(2)

	factor_meta = list(
		x1 = list(kind = "numeric"),
		x2 = list(kind = "numeric"),
		x3 = list(kind = "numeric"),
		A = list(kind = "discrete", levels = c("low", "mid", "high"))
	)

	grid11 = seq(-1, 1, length.out = 11)
	factor_levels = list(
		x1 = grid11,
		x2 = grid11,
		x3 = grid11,
		A = 0:2
	)

	points = sample_points_hypercube(3000, factor_meta, factor_levels)
	df = make_df_from_points(points, factor_meta)
	level_pos = level_pos_from_points(points, factor_levels)

	n_cases = 25
	for (k in seq_len(n_cases)) {
		# Build a random constraint expression from supported primitives
		t1 = sample(grid11, 1)
		t2 = sample(grid11, 1)
		t3 = sample(grid11, 1)

		expr = substitute(
			(!(x1 < t1) & (x2 + x3 <= t2) & (A != "mid")) |
				((x1 >= t3) & !(A == "high") & (x2 %in% c(-1, 0, 1))),
			list(t1 = t1, t2 = t2, t3 = t3)
		)

		# Random forbidden tuples (2-4 rows) on discrete-only
		n_forb = sample(2:4, 1)
		forb = data.frame(
			A = sample(c("low", "mid", "high"), n_forb, replace = TRUE),
			stringsAsFactors = FALSE
		)

		ir = compile_constraints(
			filter_expr = expr,
			forbidden_tuples = forb,
			factor_meta = factor_meta,
			factor_levels = factor_levels,
			tol = 1e-10
		)

		allowed_R_expr = eval(expr, envir = df)
		forbidden_hit = eval_forbidden_table(df, forb, factor_meta)
		allowed_R = as.logical(allowed_R_expr) & !forbidden_hit

		allowed_cpp = skpr_constraints_allowed(points, level_pos, ir)

		expect_equal(as.logical(allowed_cpp), as.logical(allowed_R))
	}
})
