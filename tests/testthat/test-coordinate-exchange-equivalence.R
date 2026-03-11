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

test_that("CE matches point-exchange on simple scenarios from requested grid", {
  requested = make_requested_candidate_set()

  # Full-factorial subset derived from the requested grid.
  base = droplevels(subset(
    requested,
    x1 %in% c(-1, 1) &
      x2 %in% c(-1, 1) &
      x3 %in% c("a", "b") &
      x4 %in% c("a", "b")
  ))

  # Larger derived subset used for constrained/sparse scenarios.
  coarse = droplevels(subset(
    requested,
    x1 %in% c(-1, 0, 1) &
      x2 %in% c(-1, 0, 1) &
      x3 %in% c("a", "b") &
      x4 %in% c("a", "b")
  ))

  scenario_specs = list(
    list(
      name = "baseline-main-effects",
      candidates = base,
      model = ~x1 + x2 + x3 + x4,
      trials = 16L,
      repeats = 40L,
      ce_constraints = NULL
    ),
    list(
      name = "baseline-interactions",
      candidates = base,
      model = ~x1 * x2 + x3 * x4,
      trials = 16L,
      repeats = 40L,
      ce_constraints = NULL
    ),
    list(
      name = "linear-constraint",
      candidates = droplevels(subset(coarse, x1 + x2 <= 0 + 1e-12)),
      model = ~x1 + x2 + x3 + x4,
      trials = 24L,
      repeats = 60L,
      ce_constraints = list(filter_expr = quote(x1 + x2 <= 0))
    ),
    list(
      name = "disallowed-combination",
      candidates = droplevels(subset(coarse, !(x3 == "a" & x4 == "b"))),
      model = ~x1 + x2 + x3 + x4,
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
      model = ~x1 * x2 + x3 + x4,
      trials = 12L,
      repeats = 120L,
      ce_constraints = list(
        filter_expr = quote((x1 + x2 <= 0) & !(x3 == "a" & x4 == "b"))
      )
    )
  )

  for (spec in scenario_specs) {
    out = run_point_vs_ce(
      candidateset = spec$candidates,
      model = spec$model,
      trials = spec$trials,
      repeats = spec$repeats,
      ce_constraints = spec$ce_constraints,
      seed = 7
    )

    expect_identical(
      canonicalize_design(out$point),
      canonicalize_design(out$ce),
      info = spec$name
    )
    expect_equal(
      attr(out$point, "D"),
      attr(out$ce, "D"),
      tolerance = 1e-10,
      info = spec$name
    )
  }
})
