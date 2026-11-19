test_that("covariates and zero-variance whole plots give the same cookie D optimum", {
  sugars = data.frame(sugar = factor(c("white", "brown")))
  chips = data.frame(chips = c(-1, 1))
  fixed = sugars[rep(1:2, each = 4), , drop = FALSE]
  rownames(fixed) = NULL
  model = ~sugar * chips

  set.seed(82)
  blocked = gen_design(merge(sugars, chips, by = NULL), model, trials = 8,
                       splitplotdesign = sugars, blocksizes = c(4, 4),
                       varianceratio = 0, repeats = 8)
  set.seed(82)
  independent = gen_design(chips, model, covariates = fixed, repeats = 8)

  expect_equal(unname(table(independent$sugar, independent$chips)),
               unname(table(blocked$sugar, blocked$chips)))
  expect_true(all(table(independent$sugar, independent$chips) == 2))
  expect_equal(get_optimality(independent, "D")$D, get_optimality(blocked, "D")$D)
  expect_identical(attr(independent, "variance.matrix"), diag(8))
  expect_false(attr(independent, "splitplot"))
})

test_that("D-optimal treatments balance the fixed plate margins", {
  treatment = data.frame(treatment = c(-1, 1))
  wells = data.frame(
    row = factor(rep(c("B", "A"), each = 4), levels = c("B", "A")),
    column = rep(1:4, 2),
    row.names = paste0("well", 8:1)
  )
  set.seed(82)
  design = gen_design(treatment, ~treatment + row + column, covariates = wells, repeats = 12,
                      randomized = FALSE, add_blocking_columns = TRUE)

  expect_identical(design[names(wells)], wells)
  expect_identical(names(design), c(names(wells), "treatment"))
  expect_false(attr(design, "blocking"))
  expect_false(attr(design, "splitplot"))
  expect_equal(attr(design, "variance.matrix"), diag(8))
  expect_equal(as.numeric(tapply(design$treatment, design$row, sum)), c(0, 0))
  expect_equal(sum(design$treatment * design$column), 0)

  # Enumerate every allocation on this plate to check the achieved optimum.
  fixed = model.matrix(~row + column, normalize_design(wells),
                       contrasts.arg = list(row = contr.simplex))
  allocations = expand.grid(rep(list(treatment$treatment), nrow(wells)))
  best_det = max(apply(allocations, 1, function(x) det(crossprod(cbind(fixed, x)))))
  X = attr(design, "model_matrix")
  expect_equal(det(crossprod(X)), best_det)
  expected_D = 100 * best_det^(1 / ncol(X)) / nrow(X)
  expect_equal(get_optimality(design, "D")$D, expected_D)
  expect_equal(max(attr(design, "optimalsearchvalues")), expected_D)
})

test_that("unused fixed columns are returned without affecting treatment assignment", {
  treatments = data.frame(treatment = c(-1, 1))
  wells = data.frame(row = factor(rep(c("A", "B"), 4)), column = rep(1:4, each = 2))
  tracking = transform(wells, well_id = sprintf("W%02d", 1:8), plate = "P1", note = NA_real_)
  for (model in list(~treatment, ~treatment + row)) {
    set.seed(82)
    expected = gen_design(treatments, model, covariates = wells, repeats = 3)
    set.seed(82)
    d = gen_design(treatments, model, covariates = tracking, repeats = 3)
    expect_identical(d[names(tracking)], tracking)
    expect_identical(d$treatment, expected$treatment)
    expect_identical(attr(d, "optimalsearchvalues"), attr(expected, "optimalsearchvalues"))
    expect_identical(colnames(attr(d, "model_matrix")), colnames(model.matrix(
      model, d, contrasts.arg = attr(d, "contrastslist")
    )))
  }
})

test_that("the search scores the terms and factor coding of the planned lm", {
  treatments = data.frame(treatment = c(-1, 0, 1))
  wells = data.frame(row = factor(rep(c("B", "A"), 12)), column = rep(c(-1, 0, 1), 8))
  cases = list(
    treatment = ~treatment,
    additive = ~treatment + row,
    dot = ~.,
    exclude = ~ . - column,
    interaction = ~treatment * row,
    interaction_only = ~0 + treatment:row,
    subtract_main = ~treatment * row - treatment,
    quadratic = ~I(treatment^2) + column + treatment
  )
  set.seed(83)
  for (rhs in cases) {
    d = gen_design(treatments, rhs, covariates = wells, contrasts = contr.treatment, repeats = 3)
    d$response = seq_len(nrow(d))
    # Keep the supplied RHS intact, including its dot, subtraction and term order.
    analysis = as.formula(call("~", quote(response), rhs[[2L]]), env = environment(rhs))
    X = model.matrix(lm(analysis, data = d))
    scored = attr(d, "model_matrix")
    expect_identical(colnames(scored), colnames(X))
    expect_equal(as.vector(scored), as.vector(X))
    expect_identical(attr(terms(attr(d, "generating_model")), "term.labels"),
                     attr(terms(analysis, data = d), "term.labels"))
  }
})

test_that("centering cannot introduce lower-order terms absent from the planned fit", {
  treatments = data.frame(dose = c(2, 4, 6))
  wells = data.frame(position = rep(c(10, 13, 15), 6), well_id = paste0("w", 1:18))
  square = function(x) x^2
  conversion = 2
  cases = list(~0 + dose, ~dose:position, ~dose + dose:position,
               ~0 + square(dose) + log(position), ~I(dose / conversion))
  set.seed(84)
  for (rhs in cases) {
    d = gen_design(treatments, rhs, covariates = wells, repeats = 2)
    d$response = seq_len(nrow(d))
    analysis = as.formula(call("~", quote(response), rhs[[2L]]), env = environment(rhs))
    X = model.matrix(lm(analysis, data = d))
    scored = attr(d, "model_matrix")
    expect_identical(colnames(scored), colnames(X))
    expect_equal(as.vector(scored), as.vector(X))
  }
})

test_that("transformations of fixed columns use the same observations as lm", {
  treatments = data.frame(dose = c(-1, 0, 1))
  wells = data.frame(position = rep(c(2, 3, 5, 8), 4))
  set.seed(86)
  d = gen_design(treatments, ~dose * poly(position, 2), covariates = wells, repeats = 2)
  d$response = seq_len(nrow(d))
  X = model.matrix(lm(response ~ dose * poly(position, 2), data = d))
  expect_identical(colnames(attr(d, "model_matrix")), colnames(X))
  expect_equal(as.vector(attr(d, "model_matrix")), as.vector(X))
  expect_error(gen_design(treatments, ~poly(dose, 2), covariates = wells, repeats = 1),
               "fixed treatment transformation")
})

test_that("mixed terms, factor coding and legal treatments follow each run", {
  treatments = expand.grid(dose = c(-1, 0, 1), drug = c("B", "A"))
  treatments = subset(treatments, !(dose == 1 & drug == "A"))
  fixed = data.frame(
    position = rep(c("left", "right", "middle"), 8),
    temperature = rep(c(10, 15, 13, 12), each = 6)
  )
  model = ~drug * position + dose * temperature + I(dose^2)
  options = list(contrasts = contr.sum, repeats = 3, progress = FALSE)
  set.seed(31)
  for (parallel in c(FALSE, TRUE)) {
    d = do.call(gen_design, c(list(
      candidateset = treatments, model = model, covariates = fixed,
      parallel = parallel, k = 8
    ), options))
    expect_identical(d[names(fixed)], fixed)
    expect_true(all(d$dose != 1 | d$drug != "A"))
    X = model.matrix(attr(d, "generating_model"), normalize_design(d),
                     contrasts.arg = list(position = contr.sum, drug = contr.sum))
    expect_identical(colnames(attr(d, "model_matrix")), colnames(X))
    expect_equal(as.vector(attr(d, "model_matrix")), as.vector(X))
  }
})

test_that("I-optimality averages over the observed joint covariate distribution", {
  treatments = data.frame(treatment = c(-1, 1))
  fixed = data.frame(position = c(-2, -2, -2, -2, 1, 1, 4, 4, 4, 4))
  model = ~treatment * position
  grid = data.frame(treatment = c(-1, 0, 1))
  set.seed(66)
  d = gen_design(treatments, model, covariates = fixed, optimality = "I",
                 high_resolution_candidate_set = grid, repeats = 4)
  region = expand.grid(treatment = grid$treatment,
                       position = normalize_design(fixed)$position)
  region_mm = model.matrix(model, region)
  expected_moment = crossprod(region_mm) / nrow(region_mm)
  X = attr(d, "model_matrix")
  expected_I = sum(diag(solve(crossprod(X), expected_moment)))
  expect_equal(attr(d, "moments.matrix"), expected_moment)
  expect_equal(get_optimality(d, "I")$I, expected_I)
  expect_equal(min(attr(d, "optimalsearchvalues")), expected_I)
})

test_that("a complete I grid supplies joint weights on the candidate scale", {
  candidates = expand.grid(z = 0:2, dose = c(10, 20, 30))
  schedule = data.frame(z = rep(c(0, 2), c(9, 4)))
  model = ~z * dose
  # A non-Cartesian prediction region inside the dose range. Three identical
  # rows count three times. z = 1 occurs in the region but never in the schedule.
  region = data.frame(z = c(0, 0, 0, 1, 2), dose = c(15, 15, 15, 21, 25))
  points = model.matrix(model, transform(region, z = z - 1, dose = (dose - 20) / 10))
  expected = crossprod(points) / nrow(points)
  set.seed(112)
  free = gen_design(candidates, model, 13, optimality = "I", repeats = 10,
                    high_resolution_candidate_set = region)
  fixed = gen_design(unique(candidates["dose"]), model, covariates = schedule, optimality = "I",
                     repeats = 10, high_resolution_candidate_set = region)
  expect_identical(fixed$z, schedule$z)
  for (d in list(free, fixed)) {
    expect_true(all(d$dose %in% candidates$dose))
    expect_equal(attr(d, "moments.matrix"), expected)
    score = sum(solve(crossprod(attr(d, "model_matrix"))) * expected)
    expect_equal(get_optimality(d, "I")$I, score)
  }
})

test_that("treatment grids cross the schedule and joint grids retain their factor coding", {
  treatments = data.frame(dose = c(-1, -0.5, 0, 0.25, 1))
  schedule = expand.grid(oven = factor(c("B", "A", "C")), position = c(-1, 1))
  schedule = schedule[c(1:6, 1:6, 1:4), ]
  model = ~dose * oven + position
  joint = merge(treatments, schedule, by = NULL)
  # A treatment contrast labels only two of three oven levels as coefficients.
  coding = list(oven = contr.treatment)
  points = model.matrix(model, joint, contrasts.arg = coding)
  expected = crossprod(points) / nrow(points)
  categorical = grepl("oven", colnames(points), fixed = TRUE)
  expected[outer(categorical, categorical, "|") & row(expected) != col(expected)] = 0
  # A differently ordered factor in the supplied grid still uses the search coding.
  joint$oven = factor(joint$oven, levels = rev(levels(schedule$oven)))
  for (prediction in list(NA, treatments, joint)) {
    set.seed(113)
    d = gen_design(treatments, model, covariates = schedule, contrasts = contr.treatment,
                   optimality = "I", repeats = 5, high_resolution_candidate_set = prediction)
    expect_equal(attr(d, "moments.matrix"), expected)
  }
  expect_error(gen_design(treatments, model, covariates = schedule, optimality = "I",
                          high_resolution_candidate_set = joint[c("dose", "oven")]),
               "Missing modeled columns.*position")
})

test_that("prediction uses the fitted polynomial basis of a fixed covariate", {
  treatments = data.frame(dose = c(-1, 1))
  fixed = data.frame(position = rep(c(2, 3, 5, 8), each = 4))
  model = ~dose * poly(position, 2)
  region = data.frame(position = c(3, 4), dose = c(-1, 1))
  set.seed(114)
  d = gen_design(treatments, model, covariates = fixed, optimality = "I", repeats = 5,
                 high_resolution_candidate_set = region)
  fitted = terms(model.frame(model, d))
  points = model.matrix(fitted, region)
  expect_equal(attr(d, "moments.matrix"), crossprod(points) / nrow(points))
})

test_that("invalid covariate layouts fail before searching", {
  trt = data.frame(x = c(-1, 1))
  fixed = data.frame(z = 1:6)
  expect_error(gen_design(trt, ~., 4, covariates = fixed), "one row per trial")
  expect_error(gen_design(trt, ~., 7, covariates = fixed), "one row per trial")
  expect_error(gen_design(trt, ~., 6, covariates = fixed$z), "data frame")
  expect_error(gen_design(trt, ~., 6, covariates = data.frame(x = 1:6)), "distinct")
  expect_error(gen_design(trt, ~., 6, covariates = transform(fixed, z = NA_real_)), "finite")
  expect_error(gen_design(trt, ~., 6, covariates = transform(fixed, z = 1)), "single value")
  expect_error(gen_design(trt, ~., 6, covariates = transform(fixed, w = -z)), "dependent")
  expect_error(gen_design(trt, ~., 6, covariates = fixed, blocksizes = 2), "unblocked")
  expect_error(gen_design(trt, ~., 6, covariates = fixed, splitplotdesign = fixed), "unblocked")
  expect_error(gen_design(trt, ~., 6, covariates = fixed, augmentdesign = trt), "unaugmented")

  set.seed(111)
  ordinary = gen_design(trt, ~x, 6, repeats = 1)
  set.seed(111)
  expect_equal(gen_design(trt, ~x, 6, repeats = 1, covariates = NULL), ordinary)
})
