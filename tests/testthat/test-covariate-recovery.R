# Fit the free design first. Give the second search only the fixed columns from
# its runs and the projected treatment candidate set, never the free assignments.
recover_design = function(grid, model, trials, fixed, criterion, prediction = grid, repeats = 150L) {
  controls = list(model = model, optimality = criterion, repeats = repeats, progress = FALSE)
  if (criterion == "I") controls$high_resolution_candidate_set = prediction
  set.seed(731 + trials)
  free = do.call(gen_design, c(list(candidateset = grid, trials = trials), controls))

  fixed_rows = free[fixed]
  treatments = unique(grid[setdiff(names(grid), fixed)])
  set.seed(912 + trials)
  constrained = do.call(gen_design, c(list(candidateset = treatments, covariates = fixed_rows), controls))
  expect_identical(constrained[fixed], fixed_rows)
  expect_false(attr(constrained, "splitplot"))

  for (design in list(free, constrained)) {
    expect_equal(nrow(design), trials)
    expect_equal(nrow(merge(design[names(grid)], unique(grid), by = names(grid))), trials)
  }
  list(free = free, constrained = constrained)
}

# Calculate both scores in a common parameterization directly from the runs.
# The same coding and prediction points are used on both sides of the comparison.
recovery_score = function(design, model, criterion, region, alias_model = NULL) {
  coding = lapply(region[vapply(region, is.factor, logical(1))], function(x) contr.simplex)
  # gen_design can reorder factor levels by replication. Use the prediction
  # grid's levels so each coefficient predicts the same level in X and points.
  for (name in names(coding)) {
    design[[name]] = factor(design[[name]], levels = levels(region[[name]]))
  }
  X = model.matrix(model, design, contrasts.arg = coding)
  information = crossprod(X)
  inverse = solve(information)
  if (criterion == "D") return(exp(as.numeric(determinant(information)$modulus) / ncol(X)) / nrow(X))
  if (criterion == "A") return(sum(diag(inverse)))
  if (criterion == "ALIAS") {
    Z = model.matrix(alias_model, design, contrasts.arg = coding)
    omitted = which(!attr(terms(alias_model), "term.labels") %in% attr(terms(model), "term.labels"))
    Z = Z[, attr(Z, "assign") %in% omitted, drop = FALSE]
    return(sum(qr.coef(qr(X), Z)^2))
  }
  points = model.matrix(model, region, contrasts.arg = coding)
  variances = rowSums((points %*% inverse) * points)
  if (criterion == "I") return(mean(variances))
  if (criterion == "G") return(max(variances))
  stop("Unknown criterion")
}

test_that("fixing either factor recovers replicated factorial designs", {
  grid = expand.grid(site = factor(c("P", "Q", "R")), dose = c(-1, 1))
  model = ~site * dose
  # Two equal-weight quadrature nodes integrate a linear dose model's squared
  # prediction terms exactly for the ordinary uniform interval on [-1, 1].
  nodes = data.frame(dose = c(-1, 1) / sqrt(3))
  for (criterion in c("D", "G", "I", "A")) {
    for (n in c(12L, 18L)) {
      fixed = if (n == 12L || criterion == "I") "site" else "dose"
      region = if (criterion == "I") expand.grid(site = levels(grid$site), dose = nodes$dose) else grid
      pair = recover_design(grid, model, n, fixed, criterion, region)
      score = recovery_score(pair$free, model, criterion, region)
      balanced = grid[rep(seq_len(nrow(grid)), each = n / 6), ]
      expect_equal(score, recovery_score(balanced, model, criterion, region))
      expect_equal(recovery_score(pair$constrained, model, criterion, region), score)
      if (criterion == "I") {
        expect_equal(get_optimality(pair$free, "I")$I, score)
        expect_equal(get_optimality(pair$constrained, "I")$I, score)
      }
      if (criterion == "G") expect_equal(score, 6 / n)
    }
  }
})

test_that("joint numeric and categorical covariates recover all two-factor interactions", {
  grid = expand.grid(site = factor(c("P", "Q")), room = c(-1, 1),
                     method = factor(c("X", "Y")), speed = c(-1, 1))
  model = ~(site + room + method + speed)^2
  for (criterion in c("D", "A")) {
    for (n in c(16L, 32L)) {
      pair = recover_design(grid, model, n, c("site", "room"), criterion)
      score = recovery_score(pair$free, model, criterion, grid)
      balanced = grid[rep(seq_len(nrow(grid)), each = n / 16), ]
      expect_equal(score, recovery_score(balanced, model, criterion, grid))
      expect_equal(recovery_score(pair$constrained, model, criterion, grid), score)
    }
  }
})

test_that("quadratic dose designs recover criterion-specific optimality", {
  grid = expand.grid(site = factor(c("P", "Q")), dose = seq(-1, 1, by = 0.25))
  model = ~site * dose + I(dose^2)
  # Equal-weight nodes with E[dose^2] = 1/3 and E[dose^4] = 1/5.
  nodes = sqrt(1 / 3 + c(-1, 1) * 2 / sqrt(45))
  prediction = data.frame(dose = c(-nodes, nodes))
  region = expand.grid(site = levels(grid$site), dose = prediction$dose)
  for (criterion in c("D", "I", "A")) {
    pair = recover_design(grid, model, 16L, "site", criterion, region, repeats = 750L)
    expected = integer(nrow(grid))
    expected[abs(grid$dose) == 1] = if (criterion == "D") 3L else 2L
    expected[grid$dose == 0] = if (criterion == "D") 2L else 4L
    score = recovery_score(pair$free, model, criterion, region)
    reference = grid[rep(seq_len(nrow(grid)), expected), ]
    expect_equal(score, recovery_score(reference, model, criterion, region))
    expect_equal(recovery_score(pair$constrained, model, criterion, region), score)
    if (criterion == "I") {
      expect_equal(get_optimality(pair$free, "I")$I, score)
      expect_equal(get_optimality(pair$constrained, "I")$I, score)
    }
  }
})

test_that("alias-optimal searches recover the same omitted-interaction bias", {
  grid = expand.grid(site = factor(c("P", "Q")), dose = c(-1, 1))
  model = ~site + dose
  for (n in c(7L, 8L, 12L)) {
    pair = recover_design(grid, model, n, "site", "ALIAS")
    alias_model = ~site * dose
    score = recovery_score(pair$free, model, "ALIAS", grid, alias_model)
    expect_equal(score, if (n == 7L) 0.12 else 0, tolerance = 1e-8)
    expect_equal(recovery_score(pair$constrained, model, "ALIAS", grid, alias_model),
                 score, tolerance = 1e-8)
    expect_gt(recovery_score(pair$constrained, model, "D", grid), 0.8)
  }
})

test_that("an alias-optimal design recovers two omitted fixed-by-free interactions", {
  grid = expand.grid(site = factor(c("P", "Q")), room = c(-1, 1), method = factor(c("X", "Y")))
  model = ~site * room + method
  alias_model = ~(site + room + method)^2
  for (n in c(8L, 24L)) {
    pair = recover_design(grid, model, n, c("site", "room"), "ALIAS")
    for (design in list(pair$free, pair$constrained)) {
      expect_equal(recovery_score(design, model, "ALIAS", grid, alias_model), 0, tolerance = 1e-8)
      expect_gt(recovery_score(design, model, "D", grid), 0.8)
    }
  }
})

# A three-component simplex lattice, crossed with two process settings.
# flour and sugar are independent coordinates; fat is 1 - flour - sugar.
mixture_grid = function() {
  recipes = subset(expand.grid(flour = 0:2 / 2, sugar = 0:2 / 2), flour + sugar <= 1)
  merge(recipes, data.frame(oven = c(0, 1)), by = NULL)
}

for (criterion in c("D", "G", "I", "A", "ALIAS")) {
  test_that(paste(criterion, "recovers the free mixture design with fixed process settings"), {
    grid = mixture_grid()
    model = ~(flour + sugar) * oven
    pair = recover_design(grid, model, 12L, "oven", criterion)
    vertices = with(grid, flour == 1 | sugar == 1 | (flour == 0 & sugar == 0))
    # flour:sugar is the only omitted two-factor interaction. Use the same
    # centered mixture coordinates as the exchange for A and alias scoring.
    scaled = function(d) transform(d, flour = 2 * flour - 1, sugar = 2 * sugar - 1, oven = 2 * oven - 1)
    alias_model = ~flour + sugar + flour:sugar
    score = function(d) recovery_score(scaled(d), model, criterion, scaled(grid), alias_model)
    reference = grid[rep(which(vertices), each = 2L), ]
    expect_equal(score(pair$free), score(reference))
    expect_equal(score(pair$constrained), score(pair$free))
    if (criterion == "I") {
      expect_equal(get_optimality(pair$free, "I")$I, score(pair$free))
      expect_equal(get_optimality(pair$constrained, "I")$I, score(pair$constrained))
    }
    if (criterion == "G") expect_equal(score(pair$free), 6 / 12)
  })
}

test_that("fixing complete compositions recovers a no-intercept mixture-process design", {
  grid = transform(mixture_grid(), fat = 1 - flour - sugar)
  # oven and the two component-by-oven terms span all three process slopes.
  model = ~0 + flour + sugar + fat + oven + flour:oven + sugar:oven
  pair = recover_design(grid, model, 18L, c("flour", "sugar", "fat"), "D")
  expect_equal(unname(rowSums(pair$constrained[c("flour", "sugar", "fat")])), rep(1, 18))
  vertices = do.call(pmax, grid[c("flour", "sugar", "fat")]) == 1
  reference = grid[rep(which(vertices), each = 3L), ]
  expect_equal(recovery_score(pair$free, model, "D", grid), recovery_score(reference, model, "D", grid))
  expect_equal(recovery_score(pair$constrained, model, "D", grid),
               recovery_score(pair$free, model, "D", grid))
})

test_that("37-run baking response surfaces recover D and A with two fixed factors", {
  # The scheduled oven and the protein content of its flour lot are known.
  # Protein levels are 10, 11, and 12 percent. Temperature spans 160 to 200 C.
  # Code both on [-1, 1] so A compares coefficient variances on the search scale.
  grid = expand.grid(oven = factor(c("deck", "convection")), protein = c(-1, 0, 1),
                     temperature = seq(-1, 1, by = 0.5), method = factor(c("cream", "melt")))
  model = ~oven * temperature + protein * method + protein:temperature +
    I(temperature^2) + I(protein^2)
  for (criterion in c("D", "A")) {
    pair = recover_design(grid, model, 37L, c("oven", "protein"), criterion, repeats = 250L)
    expect_equal(recovery_score(pair$constrained, model, criterion, grid),
                 recovery_score(pair$free, model, criterion, grid), tolerance = 1e-8)
  }
})

test_that("29-run mixing-method screening recovers alias optimality", {
  # Estimate a temperature slope for each oven and mixing method. The omitted
  # oven-by-method interaction can bias the fitted coefficients.
  grid = expand.grid(oven = factor(c("deck", "fan")), temperature = seq(-1, 1, by = 0.5),
                     method = factor(c("cream", "melt")))
  model = ~oven * temperature + method * temperature
  alias_model = ~(oven + temperature + method)^2
  pair = recover_design(grid, model, 29L, "oven", "ALIAS", repeats = 200L)
  score = recovery_score(pair$free, model, "ALIAS", grid, alias_model)
  expect_gt(score, 1e-8)
  expect_equal(recovery_score(pair$constrained, model, "ALIAS", grid, alias_model),
               score, tolerance = 1e-8)
  for (design in pair) expect_gt(recovery_score(design, model, "D", grid), 0.8)
})

# The third flour is rye, with proportion 1 - oat - wheat. Include all three
# vertices, the three binary-blend midpoints, and the three-flour centroid.
flour_blends = function() {
  edges = subset(expand.grid(oat = 0:2 / 2, wheat = 0:2 / 2), oat + wheat <= 1)
  rbind(edges, data.frame(oat = 1 / 3, wheat = 1 / 3))
}

test_that("41-run flour-blend designs recover worst-case prediction variance", {
  grid = merge(flour_blends(), data.frame(oven = factor(c("deck", "fan"))), by = NULL)
  model = ~(oat + wheat) * oven + I(oat^2) + I(wheat^2) + oat:wheat
  pair = recover_design(grid, model, 41L, "oven", "G", repeats = 250L)
  worst = recovery_score(pair$free, model, "G", grid)
  expect_equal(recovery_score(pair$constrained, model, "G", grid), worst, tolerance = 1e-8)
  p = ncol(model.matrix(model, grid))
  for (design in pair) expect_equal(get_optimality(design, "G")$G, 100 * p / (41 * worst))
})

for (categorical in c(FALSE, TRUE)) {
  setting = if (categorical) "oven type" else "bake temperature"
  test_that(paste("31-run flour-blend designs recover I with fixed", setting), {
    blends = flour_blends()
    oven = if (categorical) factor(c("deck", "fan")) else c(-1, 1)
    grid = merge(blends, data.frame(oven = oven), by = NULL)
    # Prediction demand is uniform over the seven blends, with the low/high
    # settings (or the two oven types) weighted 16:15. Compare the two
    # allocations over this same target population.
    region = merge(blends, data.frame(oven = rep(oven, c(16, 15))), by = NULL)
    model = ~oven * (oat + wheat + oat:wheat)
    pair = recover_design(grid, model, 31L, "oven", "I", prediction = region, repeats = 250L)
    # Both fractions span [0, 1] in the candidates. Use their search coding.
    coding = if (categorical) list(oven = contr.simplex) else NULL
    coded = function(d) {
      d = transform(d, oat = 2 * oat - 1, wheat = 2 * wheat - 1)
      if (categorical) d$oven = factor(d$oven, levels = levels(oven))
      return(model.matrix(model, d, contrasts.arg = coding))
    }
    points = coded(region)
    moment = crossprod(points) / nrow(points)
    if (categorical) {
      oven_terms = startsWith(colnames(points), "oven1")
      moment[outer(oven_terms, oven_terms, "|") & row(moment) != col(moment)] = 0
    }
    scores = vapply(pair, function(d) {
      expect_equal(attr(d, "moments.matrix"), moment)
      score = sum(solve(crossprod(coded(d))) * moment)
      expect_equal(get_optimality(d, "I")$I, score, tolerance = 1e-8)
      return(score)
    }, numeric(1))
    expect_equal(scores[["constrained"]], scores[["free"]], tolerance = 1e-8)
  })
}
