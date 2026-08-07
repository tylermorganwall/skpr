make_requested_candidate_set = function() {
  cand = expand.grid(
    x1 = seq(-1, 1, by = 0.1),
    x2 = seq(-1, 1, by = 0.1),
    x3 = letters[1:3],
    x4 = letters[1:3],
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  cand$x3 = factor(cand$x3)
  cand$x4 = factor(cand$x4)
  cand
}

canonicalize_design = function(design_df) {
  out = data.frame(
    x1 = as.numeric(design_df$x1),
    x2 = as.numeric(design_df$x2),
    x3 = as.character(design_df$x3),
    x4 = as.character(design_df$x4),
    stringsAsFactors = FALSE
  )
  out = out[do.call(order, out), , drop = FALSE]
  rownames(out) = NULL
  out
}

expect_valid_ce_design = function(design, candidateset, constraints, info) {
  for (nm in names(candidateset)) {
    if (is.factor(candidateset[[nm]]) || is.character(candidateset[[nm]])) {
      expect_true(
        all(
          as.character(design[[nm]]) %in%
            as.character(unique(candidateset[[nm]]))
        ),
        info = paste(info, nm)
      )
    } else {
      expect_true(
        all(
          as.numeric(design[[nm]]) %in% as.numeric(unique(candidateset[[nm]]))
        ),
        info = paste(info, nm)
      )
    }
  }

  if (!is.null(constraints) && !is.null(constraints$filter_expr)) {
    ok = eval(constraints$filter_expr, envir = design, enclos = parent.frame())
    expect_true(all(as.logical(ok)), info = info)
  }

  mm = attr(design, "model_matrix")
  expect_true(is.finite(attr(design, "D")), info = info)
  expect_equal(qr(mm)$rank, ncol(mm), info = info)
}

run_point_vs_ce = function(
  candidateset,
  model,
  trials,
  repeats,
  ce_constraints = NULL,
  seed = 7
) {
  set.seed(seed)
  design_point = gen_design(
    candidateset = candidateset,
    model = model,
    trials = trials,
    repeats = repeats,
    optimality = "D",
    progress = FALSE
  )

  set.seed(seed)
  ce_adv = list(search_method = "coordinate_exchange")
  if (!is.null(ce_constraints)) {
    ce_adv$constraints = ce_constraints
  }
  design_ce = gen_design(
    candidateset = candidateset,
    model = model,
    trials = trials,
    repeats = repeats,
    optimality = "D",
    progress = FALSE,
    advancedoptions = ce_adv
  )

  list(point = design_point, ce = design_ce)
}

test_that("CE returns valid, competitive designs on requested-grid scenarios", {
  requested = make_requested_candidate_set()

  # Full-factorial subset derived from the requested grid.
  base = droplevels(subset(
    requested,
    x1 %in%
      c(-1, 1) &
      x2 %in% c(-1, 1) &
      x3 %in% c("a", "b") &
      x4 %in% c("a", "b")
  ))

  # Larger derived subset used for constrained/sparse scenarios.
  coarse = droplevels(subset(
    requested,
    x1 %in%
      c(-1, 0, 1) &
      x2 %in% c(-1, 0, 1) &
      x3 %in% c("a", "b") &
      x4 %in% c("a", "b")
  ))

  scenario_specs = list(
    list(
      name = "baseline-main-effects",
      candidates = base,
      model = ~ x1 + x2 + x3 + x4,
      trials = 16L,
      repeats = 40L,
      ce_constraints = NULL
    ),
    list(
      name = "baseline-interactions",
      candidates = base,
      model = ~ x1 * x2 + x3 * x4,
      trials = 16L,
      repeats = 40L,
      ce_constraints = NULL
    ),
    list(
      name = "linear-constraint",
      candidates = droplevels(subset(coarse, x1 + x2 <= 0 + 1e-12)),
      model = ~ x1 + x2 + x3 + x4,
      trials = 24L,
      repeats = 60L,
      ce_constraints = list(filter_expr = quote(x1 + x2 <= 0))
    ),
    list(
      name = "disallowed-combination",
      candidates = droplevels(subset(coarse, !(x3 == "a" & x4 == "b"))),
      model = ~ x1 + x2 + x3 + x4,
      trials = 12L,
      repeats = 120L,
      ce_constraints = list(filter_expr = quote(!(x3 == "a" & x4 == "b")))
    ),
    list(
      name = "linear-plus-disallowed-with-interaction",
      candidates = droplevels(subset(
        coarse,
        x1 + x2 <= 0 + 1e-12 & !(x3 == "a" & x4 == "b")
      )),
      model = ~ x1 * x2 + x3 + x4,
      trials = 12L,
      repeats = 120L,
      ce_constraints = list(
        filter_expr = quote((x1 + x2 <= 0) & !(x3 == "a" & x4 == "b"))
      )
    )
  )

  quality_ratios = numeric(length(scenario_specs))
  for (scenario_index in seq_along(scenario_specs)) {
    spec = scenario_specs[[scenario_index]]
    out = run_point_vs_ce(
      candidateset = spec$candidates,
      model = spec$model,
      trials = spec$trials,
      repeats = spec$repeats,
      ce_constraints = spec$ce_constraints,
      seed = 7
    )

    expect_valid_ce_design(
      out$ce,
      spec$candidates,
      spec$ce_constraints,
      spec$name
    )
    quality_ratio = attr(out$ce, "D") / attr(out$point, "D")
    quality_ratios[[scenario_index]] = quality_ratio
    expect_true(quality_ratio >= 0.95, info = spec$name)
  }
  expect_gte(median(quality_ratios), 0.99)
})
