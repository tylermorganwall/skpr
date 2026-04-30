#' Compile a candidate-set filter expression to a C++ friendly IR
#'
#' Supported operators:
#' - Boolean: !, &, &&, |, ||, parentheses
#' - Comparisons: ==, !=, <, <=, >, >=
#' - Membership: %in% with c(...)
#' - Linear comparisons: (a1*x1 + a2*x2 + ... + c) relop rhs
#' - Special-case lowering: !(A == a & B == b & ...) becomes a single forbid atom
#'
#' Semantics: expression evaluates to TRUE for allowed points.
#' The parser converts expressions to DNF (disjunctive normal form). If expansion reaches
#' `max_clauses`, compilation errors; if it gets large but remains under the cap, a warning
#' is emitted suggesting forbidden tuple tables.
#'
#' @param filter_expr Default `TRUE`. R expression (language object) or TRUE/FALSE.
#' @param factor_meta Default `NULL`. Named list of factor metadata; each entry list(kind = 'discrete'|'numeric', levels = ...).
#' @param factor_levels Default `NULL`. List of allowed values per factor; used to snap numeric constants for %in% and ==/!=.
#' @param tol Default `1e-10`. Snapping tolerance for numeric constants.
#' @param max_clauses Default `2048L`. Maximum allowed DNF clause count.
#' @return List IR for ConstraintSet.
compile_filter_ir = function(
	filter_expr = TRUE,
	factor_meta = NULL,
	factor_levels = NULL,
	tol = 1e-10,
	max_clauses = 2048L
) {
	if (isTRUE(filter_expr) || isFALSE(filter_expr)) {
		if (is.null(factor_meta) || length(factor_meta) == 0) {
			stop(
				"compile_filter_ir: factor_meta required when filter_expr is TRUE/FALSE."
			)
		}
		q = length(factor_meta)
		factor_kind = vapply(
			factor_meta,
			function(m) if (m$kind == "numeric") 1L else 0L,
			integer(1)
		)
		L = vapply(
			seq_along(factor_meta),
			function(j) {
				m = factor_meta[[j]]
				if (m$kind == "discrete") {
					return(length(m$levels))
				}
				if (!is.null(factor_levels)) {
					return(length(factor_levels[[names(factor_meta)[j]]]))
				}
				0L
			},
			integer(1)
		)

		clause_ptr = if (isTRUE(filter_expr)) c(0L, 0L) else c(0L)

		return(list(
			version = 1L,
			q = q,
			factor_kind = as.integer(factor_kind),
			L = as.integer(L),
			clause_ptr = clause_ptr,
			clause_atom = integer(),
			atom_type = integer(),
			atom_payload_idx = integer(),
			cmp_var = integer(),
			cmp_op = integer(),
			cmp_value = numeric(),
			in_var = integer(),
			in_neg = integer(),
			in_ptr = c(0L),
			in_values = numeric(),
			lin_op = integer(),
			lin_rhs = numeric(),
			lin_const = numeric(),
			lin_ptr = c(0L),
			lin_idx = integer(),
			lin_coef = numeric(),
			forb_ptr = c(0L),
			forb_idx = integer(),
			forb_value = numeric(),
			forbidden_tables = list()
		))
	}

	if (is.null(factor_meta) || length(factor_meta) == 0) {
		stop("compile_filter_ir: factor_meta must be a non-empty named list.")
	}

	factor_names = names(factor_meta)
	if (is.null(factor_names) || any(factor_names == "")) {
		stop("compile_filter_ir: factor_meta must be named.")
	}

	q = length(factor_meta)
	name_to_idx = setNames(seq_len(q) - 1L, factor_names)

	factor_kind = integer(q)
	L = integer(q)
	level_maps = vector("list", q)
	for (j in seq_len(q)) {
		meta = factor_meta[[j]]
		if (is.null(meta$kind) || !(meta$kind %in% c("discrete", "numeric"))) {
			stop(
				"compile_filter_ir: each factor_meta entry must have kind = 'discrete' or 'numeric'."
			)
		}
		if (meta$kind == "discrete") {
			if (is.null(meta$levels)) {
				stop("compile_filter_ir: discrete factors require levels.")
			}
			lev = as.character(meta$levels)
			L[j] = length(lev)
			factor_kind[j] = 0L
			level_maps[[j]] = setNames(seq_along(lev) - 1L, lev)
		} else {
			factor_kind[j] = 1L
			if (!is.null(factor_levels)) {
				if (is.null(factor_levels[[factor_names[j]]])) {
					stop(
						"compile_filter_ir: factor_levels missing entry for numeric factor."
					)
				}
				L[j] = length(factor_levels[[factor_names[j]]])
			} else {
				L[j] = 0L
			}
			level_maps[[j]] = NULL
		}
	}

	op_code = c("==" = 1L, "!=" = 2L, "<" = 3L, "<=" = 4L, ">" = 5L, ">=" = 6L)

	invert_op = function(code) {
		if (code == 1L) {
			return(2L)
		} # == -> !=
		if (code == 2L) {
			return(1L)
		} # != -> ==
		if (code == 3L) {
			return(6L)
		} # <  -> >=
		if (code == 4L) {
			return(5L)
		} # <= -> >
		if (code == 5L) {
			return(4L)
		} # >  -> <=
		if (code == 6L) {
			return(3L)
		} # >= -> <
		stop("invert_op: invalid code.")
	}

	swap_op = function(code) {
		if (code == 1L) {
			return(1L)
		}
		if (code == 2L) {
			return(2L)
		}
		if (code == 3L) {
			return(5L)
		}
		if (code == 4L) {
			return(6L)
		}
		if (code == 5L) {
			return(3L)
		}
		if (code == 6L) {
			return(4L)
		}
		stop("swap_op: invalid code.")
	}

	is_sym = function(x) is.symbol(x) && length(x) == 1L
	sym_name = function(x) as.character(x)

	num_scalar_value = function(x) {
		if (is.numeric(x) && length(x) == 1L && is.finite(x)) {
			return(as.numeric(x))
		}
		if (is.call(x) && length(x) == 2L) {
			fn = sym_name(x[[1L]])
			if (
				fn %in%
					c("+", "-") &&
					is.numeric(x[[2L]]) &&
					length(x[[2L]]) == 1L &&
					is.finite(x[[2L]])
			) {
				v = as.numeric(x[[2L]])
				return(if (fn == "-") -v else v)
			}
		}
		NULL
	}

	is_num_scalar = function(x) !is.null(num_scalar_value(x))
	is_chr_scalar = function(x) is.character(x) && length(x) == 1L
	is_c_call = function(x) is.call(x) && identical(x[[1L]], as.name("c"))
	as_num_lit = function(x) {
		out = try(num_scalar_value(x), silent = TRUE)
		if (inherits(out, "try-error")) {
			return(NULL)
		}
		if (!is.numeric(out) || length(out) != 1L || !is.finite(out)) {
			return(NULL)
		}
		as.numeric(out)
	}
	snap_numeric = function(var_idx0, value) {
		num_value = num_scalar_value(value)
		if (is.null(num_value)) {
			stop("compile_filter_ir: numeric factors require numeric constants.")
		}
		if (is.null(factor_levels)) {
			return(num_value)
		}
		j = var_idx0 + 1L
		nm = factor_names[j]
		lev = factor_levels[[nm]]
		if (is.null(lev)) {
			return(num_value)
		}
		lev = as.numeric(lev)
		d = abs(lev - num_value)
		k = which.min(d)
		if (length(k) == 0) {
			stop("compile_filter_ir: failed to snap numeric constant.")
		}
		if (d[k] > tol) {
			stop(
				"compile_filter_ir: numeric constant does not match any allowed level (within tol) for factor '",
				nm,
				"'."
			)
		}
		lev[k]
	}

	map_const = function(var_idx0, value) {
		j = var_idx0 + 1L
		if (factor_kind[j] == 0L) {
			if (is_chr_scalar(value)) {
				mp = level_maps[[j]]
				if (is.null(mp[[value]])) {
					stop(
						"compile_filter_ir: unknown level '",
						value,
						"' for factor '",
						factor_names[j],
						"'."
					)
				}
				return(as.numeric(mp[[value]]))
			}
			if (is_num_scalar(value)) {
				num_value = num_scalar_value(value)
				code = as.integer(num_value)
				if (abs(num_value - code) > 0) {
					stop("compile_filter_ir: discrete coded values must be integers.")
				}
				if (code < 0L || code >= L[j]) {
					stop("compile_filter_ir: discrete code out of range.")
				}
				return(as.numeric(code))
			}
			stop("compile_filter_ir: invalid constant type for discrete factor.")
		}
		if (!is_num_scalar(value)) {
			stop("compile_filter_ir: numeric factors require numeric constants.")
		}
		snap_numeric(var_idx0, value)
	}

	parse_lin = function(node) {
		if (is_num_scalar(node)) {
			return(list(constant = num_scalar_value(node), coef = numeric()))
		}
		if (is_sym(node)) {
			nm = sym_name(node)
			if (is.null(name_to_idx[[nm]])) {
				stop("compile_filter_ir: unknown symbol in linear expression: ", nm)
			}
			return(list(constant = 0, coef = setNames(1.0, nm)))
		}
		if (!is.call(node)) {
			stop("compile_filter_ir: invalid linear expression.")
		}
		fn = sym_name(node[[1L]])

		if (fn == "(") {
			return(parse_lin(node[[2L]]))
		}

		if (fn == "-" && length(node) == 2L) {
			inner = parse_lin(node[[2L]])
			inner$constant = -inner$constant
			if (length(inner$coef) > 0) {
				for (nm in names(inner$coef)) {
					inner$coef[[nm]] = -inner$coef[[nm]]
				}
			}
			return(inner)
		}

		if (fn %in% c("+", "-")) {
			a = parse_lin(node[[2L]])
			b = parse_lin(node[[3L]])
			coef = a$coef
			if (length(b$coef) > 0) {
				for (nm in names(b$coef)) {
					current = coef[nm]
					if (length(current) == 0 || is.na(current)) {
						current = 0
					}
					coef[nm] = current + if (fn == "+") b$coef[[nm]] else -b$coef[[nm]]
				}
			}
			return(list(
				constant = a$constant + if (fn == "+") b$constant else -b$constant,
				coef = coef
			))
		}

		if (fn == "*") {
			left = node[[2L]]
			right = node[[3L]]
			if (is_num_scalar(left) && is_sym(right)) {
				nm = sym_name(right)
				return(list(constant = 0, coef = setNames(num_scalar_value(left), nm)))
			}
			if (is_sym(left) && is_num_scalar(right)) {
				nm = sym_name(left)
				return(list(constant = 0, coef = setNames(num_scalar_value(right), nm)))
			}
			stop(
				"compile_filter_ir: linear term must be NUM*VAR or VAR*NUM. Rewrite 2*(x1+x2) as 2*x1+2*x2."
			)
		}

		stop("compile_filter_ir: unsupported operator in linear expression: ", fn)
	}

	make_atom_cmp = function(var_nm, op_str, const_val) {
		var_idx0 = name_to_idx[[var_nm]]
		if (is.null(var_idx0)) {
			stop("compile_filter_ir: unknown variable: ", var_nm)
		}
		code = op_code[[op_str]]
		if (is.null(code)) {
			stop("compile_filter_ir: unsupported comparison: ", op_str)
		}

		j = var_idx0 + 1L
		if (factor_kind[j] == 0L && !(op_str %in% c("==", "!="))) {
			stop(
				"compile_filter_ir: ordering comparisons not allowed on discrete factor: ",
				var_nm
			)
		}

		value = map_const(var_idx0, const_val)
		list(type = "cmp", var = var_idx0, op = code, value = value)
	}

	make_atom_in = function(var_nm, values, negated = FALSE) {
		var_idx0 = name_to_idx[[var_nm]]
		if (is.null(var_idx0)) {
			stop("compile_filter_ir: unknown variable: ", var_nm)
		}
		if (length(values) == 0) {
			stop("compile_filter_ir: %in% requires at least one value.")
		}
		vals = vapply(values, function(v) map_const(var_idx0, v), numeric(1))
		list(type = "in", var = var_idx0, negated = isTRUE(negated), values = vals)
	}

	make_atom_lin = function(lin_node, op_str, rhs_num) {
		code = op_code[[op_str]]
		if (is.null(code)) {
			stop("compile_filter_ir: unsupported linear comparison: ", op_str)
		}
		if (!is_num_scalar(rhs_num)) {
			stop("compile_filter_ir: linear rhs must be numeric scalar.")
		}
		rhs_val = num_scalar_value(rhs_num)

		lin = parse_lin(lin_node)
		if (length(lin$coef) == 0) {
			stop(
				"compile_filter_ir: linear expression must involve at least one variable."
			)
		}

		idx0 = integer(0)
		coef = numeric(0)
		for (nm in names(lin$coef)) {
			var_idx0 = name_to_idx[[nm]]
			if (is.null(var_idx0)) {
				stop("compile_filter_ir: unknown variable in linear expression: ", nm)
			}
			jj = var_idx0 + 1L
			if (factor_kind[jj] != 1L) {
				stop(
					"compile_filter_ir: linear constraints require numeric factors; got: ",
					nm
				)
			}
			idx0 = c(idx0, var_idx0)
			coef = c(coef, as.numeric(lin$coef[[nm]]))
		}

		ord = order(idx0)
		idx0 = idx0[ord]
		coef = coef[ord]

		list(
			type = "lin",
			op = code,
			rhs = rhs_val,
			constant = as.numeric(lin$constant),
			idx = idx0,
			coef = coef
		)
	}

	parse_expr = function(node) {
		if (isTRUE(node)) {
			return(list(type = "true"))
		}
		if (isFALSE(node)) {
			return(list(type = "false"))
		}

		if (is_sym(node)) {
			nm = sym_name(node)
			if (nm %in% c("TRUE", "T")) {
				return(list(type = "true"))
			}
			if (nm %in% c("FALSE", "F")) {
				return(list(type = "false"))
			}
			stop("compile_filter_ir: bare symbol not allowed: ", nm)
		}

		if (!is.call(node)) {
			stop("compile_filter_ir: unsupported expression node.")
		}

		fn = sym_name(node[[1L]])

		# Parentheses are represented as calls to "(" in R language objects.
		# Unwrap and continue parsing the inner expression.
		if (fn == "(") {
			return(parse_expr(node[[2L]]))
		}

		if (fn %in% c("&", "&&", "|", "||")) {
			kids = lapply(as.list(node)[-1L], parse_expr)
			return(list(
				type = if (fn %in% c("&", "&&")) "and" else "or",
				kids = kids
			))
		}

		if (fn == "!") {
			return(list(type = "not", kid = parse_expr(node[[2L]])))
		}

		if (fn == "%in%") {
			lhs = node[[2L]]
			rhs = node[[3L]]
			if (!is_sym(lhs)) {
				stop("compile_filter_ir: lhs of %in% must be a variable name.")
			}
			if (!is_c_call(rhs)) {
				stop("compile_filter_ir: rhs of %in% must be c(...).")
			}
			vals = as.list(rhs)[-1L]
			vals = lapply(vals, function(v) {
				num = as_num_lit(v)
				if (!is.null(num)) {
					return(num)
				}
				if (is_chr_scalar(v)) {
					return(v)
				}
				stop("compile_filter_ir: unsupported value in c(...).")
			})
			return(list(
				type = "atom",
				atom = make_atom_in(sym_name(lhs), vals, negated = FALSE)
			))
		}

		if (fn %in% names(op_code)) {
			lhs = node[[2L]]
			rhs = node[[3L]]
			op_str = fn

			lhs_num = as_num_lit(lhs)
			rhs_num = as_num_lit(rhs)
			if (!is.null(rhs_num)) {
				lin_try = try(parse_lin(lhs), silent = TRUE)
				if (!inherits(lin_try, "try-error") && length(lin_try$coef) > 0) {
					return(list(
						type = "atom",
						atom = make_atom_lin(lhs, op_str, rhs_num)
					))
				}
			}
			if (!is.null(lhs_num)) {
				lin_try = try(parse_lin(rhs), silent = TRUE)
				if (!inherits(lin_try, "try-error") && length(lin_try$coef) > 0) {
					code_swapped = swap_op(op_code[[op_str]])
					op_str2 = names(op_code)[match(code_swapped, op_code)]
					return(list(
						type = "atom",
						atom = make_atom_lin(rhs, op_str2, lhs_num)
					))
				}
			}

			if (is_sym(lhs) && (!is.null(rhs_num) || is_chr_scalar(rhs))) {
				return(list(
					type = "atom",
					atom = make_atom_cmp(
						sym_name(lhs),
						op_str,
						if (!is.null(rhs_num)) rhs_num else rhs
					)
				))
			}
			if (is_sym(rhs) && (!is.null(lhs_num) || is_chr_scalar(lhs))) {
				code_swapped = swap_op(op_code[[op_str]])
				op_str2 = names(op_code)[match(code_swapped, op_code)]
				return(list(
					type = "atom",
					atom = make_atom_cmp(
						sym_name(rhs),
						op_str2,
						if (!is.null(lhs_num)) lhs_num else lhs
					)
				))
			}

			stop("compile_filter_ir: unsupported comparison form.")
		}

		stop("compile_filter_ir: unsupported operator: ", fn)
	}

	to_nnf = function(ast, neg = FALSE) {
		if (ast$type == "true") {
			return(if (neg) list(type = "false") else ast)
		}
		if (ast$type == "false") {
			return(if (neg) list(type = "true") else ast)
		}

		if (ast$type == "atom") {
			at = ast$atom
			if (!neg) {
				return(ast)
			}

			if (at$type == "cmp") {
				at$op = invert_op(at$op)
				return(list(type = "atom", atom = at))
			}
			if (at$type == "in") {
				at$negated = !isTRUE(at$negated)
				return(list(type = "atom", atom = at))
			}
			if (at$type == "lin") {
				at$op = invert_op(at$op)
				return(list(type = "atom", atom = at))
			}
			stop("compile_filter_ir: unsupported atom for negation.")
		}

		if (ast$type == "not") {
			return(to_nnf(ast$kid, !neg))
		}

		if (ast$type %in% c("and", "or")) {
			kids = ast$kids

			if (neg && ast$type == "and") {
				pairs_idx = integer(0)
				pairs_val = numeric(0)
				ok = TRUE
				for (k in seq_along(kids)) {
					kid = kids[[k]]
					if (
						kid$type != "atom" || kid$atom$type != "cmp" || kid$atom$op != 1L
					) {
						ok = FALSE
						break
					}
					pairs_idx = c(pairs_idx, kid$atom$var)
					pairs_val = c(pairs_val, kid$atom$value)
				}
				if (ok) {
					ord = order(pairs_idx)
					return(list(
						type = "atom",
						atom = list(
							type = "forbid",
							idx = pairs_idx[ord],
							value = pairs_val[ord]
						)
					))
				}
			}

			if (!neg) {
				return(list(type = ast$type, kids = lapply(kids, to_nnf, neg = FALSE)))
			}

			swapped = if (ast$type == "and") "or" else "and"
			return(list(type = swapped, kids = lapply(kids, to_nnf, neg = TRUE)))
		}

		stop("compile_filter_ir: internal error in NNF.")
	}

	to_dnf = function(ast) {
		if (ast$type == "true") {
			return(list(list()))
		}
		if (ast$type == "false") {
			return(list())
		}
		if (ast$type == "atom") {
			return(list(list(ast$atom)))
		}

		if (ast$type == "or") {
			out = list()
			for (k in seq_along(ast$kids)) {
				out = c(out, to_dnf(ast$kids[[k]]))
				if (length(out) > max_clauses) {
					stop(
						"compile_filter_ir: DNF clause explosion; simplify or use forbidden tuple tables."
					)
				}
			}
			return(out)
		}

		if (ast$type == "and") {
			clauses = list(list())
			for (k in seq_along(ast$kids)) {
				rhs = to_dnf(ast$kids[[k]])
				if (length(rhs) == 0) {
					return(list())
				}
				new_clauses = list()
				for (a in seq_along(clauses)) {
					for (b in seq_along(rhs)) {
						new_clauses[[length(new_clauses) + 1L]] = c(clauses[[a]], rhs[[b]])
						if (length(new_clauses) > max_clauses) {
							stop(
								"compile_filter_ir: DNF clause explosion; simplify or use forbidden tuple tables."
							)
						}
					}
				}
				clauses = new_clauses
			}
			return(clauses)
		}

		stop("compile_filter_ir: internal error in DNF.")
	}

	atom_key = function(a) {
		if (a$type == "cmp") {
			return(paste0(
				"cmp|",
				a$var,
				"|",
				a$op,
				"|",
				format(a$value, digits = 17)
			))
		}
		if (a$type == "in") {
			vals = sort(a$values)
			return(paste0(
				"in|",
				a$var,
				"|",
				as.integer(a$negated),
				"|",
				paste(format(vals, digits = 17), collapse = ",")
			))
		}
		if (a$type == "lin") {
			return(paste0(
				"lin|",
				a$op,
				"|",
				format(a$rhs, digits = 17),
				"|",
				format(a$constant, digits = 17),
				"|",
				paste(a$idx, collapse = ","),
				"|",
				paste(format(a$coef, digits = 17), collapse = ",")
			))
		}
		if (a$type == "forbid") {
			return(paste0(
				"forbid|",
				paste(a$idx, collapse = ","),
				"|",
				paste(format(a$value, digits = 17), collapse = ",")
			))
		}
		stop("compile_filter_ir: unknown atom type.")
	}

	ast0 = parse_expr(filter_expr)
	nnf = to_nnf(ast0, neg = FALSE)
	clauses_atoms = to_dnf(nnf)

	if (length(clauses_atoms) > (max_clauses %/% 2)) {
		warning(
			"compile_filter_ir: expanded to ",
			length(clauses_atoms),
			" DNF clauses; consider forbidden tuple tables or simplifying the expression."
		)
	}

	atom_id_env = new.env(parent = emptyenv())
	atom_type = integer(0)
	atom_payload_idx = integer(0)

	cmp_var = integer(0)
	cmp_op = integer(0)
	cmp_value = numeric(0)
	in_var = integer(0)
	in_neg = integer(0)
	in_ptr = 0L
	in_values = numeric(0)
	lin_op = integer(0)
	lin_rhs = numeric(0)
	lin_const = numeric(0)
	lin_ptr = 0L
	lin_idx = integer(0)
	lin_coef = numeric(0)
	forb_ptr = 0L
	forb_idx = integer(0)
	forb_value = numeric(0)

	get_atom_id = function(a) {
		key = atom_key(a)
		existing = atom_id_env[[key]]
		if (!is.null(existing)) {
			return(existing)
		}

		id = length(atom_type)

		if (a$type == "cmp") {
			payload = length(cmp_var)
			cmp_var <<- c(cmp_var, as.integer(a$var))
			cmp_op <<- c(cmp_op, as.integer(a$op))
			cmp_value <<- c(cmp_value, as.numeric(a$value))
			atom_type <<- c(atom_type, 1L)
			atom_payload_idx <<- c(atom_payload_idx, payload)
		} else if (a$type == "in") {
			payload = length(in_var)
			in_var <<- c(in_var, as.integer(a$var))
			in_neg <<- c(in_neg, as.integer(isTRUE(a$negated)))
			vals = sort(as.numeric(a$values))
			in_values <<- c(in_values, vals)
			in_ptr <<- c(in_ptr, length(in_values))
			atom_type <<- c(atom_type, 2L)
			atom_payload_idx <<- c(atom_payload_idx, payload)
		} else if (a$type == "lin") {
			payload = length(lin_op)
			lin_op <<- c(lin_op, as.integer(a$op))
			lin_rhs <<- c(lin_rhs, as.numeric(a$rhs))
			lin_const <<- c(lin_const, as.numeric(a$constant))
			lin_idx <<- c(lin_idx, as.integer(a$idx))
			lin_coef <<- c(lin_coef, as.numeric(a$coef))
			lin_ptr <<- c(lin_ptr, length(lin_idx))
			atom_type <<- c(atom_type, 3L)
			atom_payload_idx <<- c(atom_payload_idx, payload)
		} else if (a$type == "forbid") {
			payload = (length(forb_ptr) - 1L)
			forb_idx <<- c(forb_idx, as.integer(a$idx))
			forb_value <<- c(forb_value, as.numeric(a$value))
			forb_ptr <<- c(forb_ptr, length(forb_idx))
			atom_type <<- c(atom_type, 4L)
			atom_payload_idx <<- c(atom_payload_idx, payload)
		} else {
			stop("compile_filter_ir: unsupported atom type.")
		}

		atom_id_env[[key]] = id
		id
	}

	clause_ptr = integer(1)
	clause_ptr[1] = 0L
	clause_atom = integer(0)

	for (c in seq_along(clauses_atoms)) {
		atoms = clauses_atoms[[c]]
		if (length(atoms) > 0) {
			ids = vapply(atoms, get_atom_id, integer(1))
			ids = sort(unique(ids))
			clause_atom = c(clause_atom, ids)
		}
		clause_ptr = c(clause_ptr, length(clause_atom))
	}

	list(
		version = 1L,
		q = q,
		factor_kind = as.integer(factor_kind),
		L = as.integer(L),

		clause_ptr = as.integer(clause_ptr),
		clause_atom = as.integer(clause_atom),

		atom_type = as.integer(atom_type),
		atom_payload_idx = as.integer(atom_payload_idx),

		cmp_var = as.integer(cmp_var),
		cmp_op = as.integer(cmp_op),
		cmp_value = as.numeric(cmp_value),

		in_var = as.integer(in_var),
		in_neg = as.integer(in_neg),
		in_ptr = as.integer(in_ptr),
		in_values = as.numeric(in_values),

		lin_op = as.integer(lin_op),
		lin_rhs = as.numeric(lin_rhs),
		lin_const = as.numeric(lin_const),
		lin_ptr = as.integer(lin_ptr),
		lin_idx = as.integer(lin_idx),
		lin_coef = as.numeric(lin_coef),

		forb_ptr = as.integer(forb_ptr),
		forb_idx = as.integer(forb_idx),
		forb_value = as.numeric(forb_value),

		forbidden_tables = list()
	)
}

#' Compile a forbidden tuples table
#'
#' @param forbidden Default `NULL`. data.frame/matrix; named columns are factor names.
#' @param factor_meta Default `NULL`. Factor metadata.
#' @param factor_levels Default `NULL`. Allowed values per factor.
#' @param tol Default `1e-10`. Numeric snapping tolerance.
#' @return List with idx (0-based) and codes matrix (n x m) of level positions.
compile_forbidden_tuples = function(
	forbidden,
	factor_meta,
	factor_levels,
	tol = 1e-10
) {
	if (is.null(forbidden)) {
		return(NULL)
	}
	if (!is.data.frame(forbidden) && !is.matrix(forbidden)) {
		stop("compile_forbidden_tuples: forbidden must be data.frame or matrix.")
	}

	df = as.data.frame(forbidden, stringsAsFactors = FALSE)
	if (is.null(names(df)) || any(names(df) == "")) {
		stop("compile_forbidden_tuples: forbidden must have named columns.")
	}

	factor_names = names(factor_meta)
	name_to_idx = setNames(seq_along(factor_meta) - 1L, factor_names)

	cols = names(df)
	idx0 = unname(name_to_idx[cols])
	if (any(is.na(idx0))) {
		stop("compile_forbidden_tuples: forbidden contains unknown factor name(s).")
	}

	m = length(cols)
	n = nrow(df)
	codes = matrix(0L, nrow = n, ncol = m)

	for (j in seq_len(m)) {
		nm = cols[j]
		var_idx0 = idx0[j]
		meta = factor_meta[[var_idx0 + 1L]]
		lev = factor_levels[[nm]]
		if (is.null(lev)) {
			stop(
				"compile_forbidden_tuples: factor_levels missing entry for factor '",
				nm,
				"'."
			)
		}
		lev = as.numeric(lev)

		if (meta$kind == "discrete") {
			levels_chr = as.character(meta$levels)
			mp = setNames(seq_along(levels_chr) - 1L, levels_chr)
			for (i in seq_len(n)) {
				v = df[[j]][i]
				if (is.factor(v)) {
					v = as.character(v)
				}
				if (is.character(v)) {
					if (is.null(mp[[v]])) {
						stop(
							"compile_forbidden_tuples: unknown level '",
							v,
							"' for factor '",
							nm,
							"'."
						)
					}
					code = as.integer(mp[[v]])
				} else {
					if (!is.finite(v)) {
						stop(
							"compile_forbidden_tuples: non-finite value in forbidden table."
						)
					}
					code = as.integer(v)
				}
				pos = match(as.numeric(code), lev)
				if (is.na(pos)) {
					stop(
						"compile_forbidden_tuples: forbidden code not present in factor_levels for factor '",
						nm,
						"'."
					)
				}
				codes[i, j] = pos - 1L
			}
		} else {
			for (i in seq_len(n)) {
				v = as.numeric(df[[j]][i])
				if (!is.finite(v)) {
					stop(
						"compile_forbidden_tuples: non-finite numeric value in forbidden table."
					)
				}
				d = abs(lev - v)
				k = which.min(d)
				if (length(k) == 0 || d[k] > tol) {
					stop(
						"compile_forbidden_tuples: numeric value does not match any allowed level (within tol) for factor '",
						nm,
						"'."
					)
				}
				codes[i, j] = k - 1L
			}
		}
	}

	list(idx = as.integer(idx0), codes = codes)
}

#' Compile constraints (filter expression + forbidden tuples)
#'
#' @param filter_expr Default `TRUE`. Filter expression.
#' @param forbidden_tuples Default `NULL`. Forbidden tuples table or list of tables.
#' @param factor_meta Default `NULL`. Factor metadata.
#' @param factor_levels Default `NULL`. Allowed values per factor.
#' @param tol Default `1e-10`. Numeric snapping tolerance.
#' @return IR list suitable for ConstraintSet.
compile_constraints = function(
	filter_expr = TRUE,
	forbidden_tuples = NULL,
	factor_meta,
	factor_levels,
	tol = 1e-10
) {
	ir = compile_filter_ir(
		filter_expr,
		factor_meta = factor_meta,
		factor_levels = factor_levels,
		tol = tol
	)

	tabs = list()
	if (!is.null(forbidden_tuples)) {
		if (
			is.list(forbidden_tuples) &&
				!is.data.frame(forbidden_tuples) &&
				!is.matrix(forbidden_tuples)
		) {
			for (k in seq_along(forbidden_tuples)) {
				tab = compile_forbidden_tuples(
					forbidden_tuples[[k]],
					factor_meta,
					factor_levels,
					tol = tol
				)
				if (!is.null(tab)) tabs[[length(tabs) + 1L]] = tab
			}
		} else {
			tab = compile_forbidden_tuples(
				forbidden_tuples,
				factor_meta,
				factor_levels,
				tol = tol
			)
			if (!is.null(tab)) tabs[[1L]] = tab
		}
	}

	ir$forbidden_tables = tabs
	ir
}

constraint_ir_get_range = function(ptr, u) {
	start = ptr[[u]] + 1L
	end = ptr[[u + 1L]]
	if (end < start) integer() else seq.int(start, end)
}

constraint_ir_atom_supports = function(ir) {
	if (is.null(ir)) {
		return(list())
	}

	n_atoms = length(ir$atom_type)
	supports = vector("list", n_atoms)
	if (n_atoms == 0) {
		return(supports)
	}

	for (a in seq_len(n_atoms)) {
		type = ir$atom_type[[a]]
		payload_idx = ir$atom_payload_idx[[a]]
		u = payload_idx + 1L

		support = integer()
		if (type == 1L) {
			support = ir$cmp_var[[u]] + 1L
		} else if (type == 2L) {
			support = ir$in_var[[u]] + 1L
		} else if (type == 3L) {
			idx = constraint_ir_get_range(ir$lin_ptr, u)
			support = ir$lin_idx[idx] + 1L
		} else if (type == 4L) {
			idx = constraint_ir_get_range(ir$forb_ptr, u)
			support = ir$forb_idx[idx] + 1L
		} else {
			stop("constraint_ir_atom_supports: unknown atom_type.")
		}

		supports[[a]] = sort(unique(as.integer(support)))
	}

	supports
}

constraint_ir_support_edges = function(ir) {
	if (is.null(ir)) {
		return(list())
	}

	edges = list()
	add_edge = function(x) {
		x = sort(unique(as.integer(x)))
		if (length(x) > 1L) {
			edges[[length(edges) + 1L]] <<- x
		}
	}

	atom_supports = constraint_ir_atom_supports(ir)
	for (support in atom_supports) {
		add_edge(support)
	}

	if (!is.null(ir$forbidden_tables) && length(ir$forbidden_tables) > 0L) {
		for (tab in ir$forbidden_tables) {
			add_edge(tab$idx + 1L)
		}
	}

	n_clauses = length(ir$clause_ptr) - 1L
	if (n_clauses > 1L) {
		for (c in seq_len(n_clauses)) {
			idx = constraint_ir_get_range(ir$clause_ptr, c)
			atom_ids = ir$clause_atom[idx] + 1L
			clause_support = sort(unique(unlist(atom_supports[atom_ids])))
			add_edge(clause_support)
		}
	}

	edges
}
