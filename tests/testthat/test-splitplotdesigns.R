library(skpr)
test_that("Split-plot design categorical factors are retained", {
  cand <- expand.grid(
    Zoom = c("None", "Optical", "Digital"),

    TargetType = c("Man Sized", "NATO Standard"),

    Orientation = c("Side", "Front/Back"),

    Movement = c("Stationary", "Moving"),

    Illum = c("Day", "Night", "Low Light")
  )

  htc <- gen_design(
    candidateset = cand,

    model = ~Zoom,

    trials = 10,

    repeats = 10
  )

  DOE <- gen_design(
    candidateset = cand,

    model = ~ (Illum + Zoom + TargetType + Orientation + Movement),

    trials = 60,

    splitplotdesign = htc,

    repeats = 1
  )
  for (col in seq_len(ncol(DOE))) {
    expect_equal(class(DOE[, col]), "factor")
  }
  DOE2 <- gen_design(
    candidateset = cand,

    model = ~ (Illum + Zoom),

    trials = 60,

    splitplotdesign = htc,

    repeats = 20
  )
  for (col in seq_len(ncol(DOE2))) {
    expect_equal(class(DOE2[, col]), "factor")
  }

  htc2 <- gen_design(
    candidateset = cand,

    model = ~ Illum + Zoom + Illum:Zoom,

    trials = 10,

    repeats = 10
  )
  DOE3 <- gen_design(
    candidateset = cand,

    model = ~ (Illum + Zoom + Illum:Zoom + TargetType + Orientation + Movement),

    trials = 60,

    splitplotdesign = htc2,

    repeats = 20
  )
  for (col in seq_len(ncol(DOE3))) {
    expect_equal(class(DOE3[, col]), "factor")
  }
  vhtc <- gen_design(
    candidateset = cand,

    model = ~Illum,

    trials = 10,

    repeats = 10
  )
  htc3 <- gen_design(
    candidateset = cand,
    model = ~ Illum + Zoom + Illum:Zoom,
    trials = 20,
    splitplotdesign = vhtc,
    repeats = 10
  )
  splitsplitdesign <- gen_design(
    candidateset = cand,
    model = ~ (Illum + Zoom + Illum:Zoom + TargetType + Orientation + Movement),
    trials = 60,
    splitplotdesign = htc3,
    repeats = 20
  )
  for (col in seq_len(ncol(splitsplitdesign))) {
    expect_equal(class(splitsplitdesign[, col]), "factor")
  }
})

test_that("JMP SPD import works", {
  jmp_design_htc = read.csv(testthat::test_path("JMP_design_htc.csv"))
  expect_error(eval_design(jmp_design_htc, ~., 0.2, blocking = TRUE))
  expect_warning(
    eval_design_mc(jmp_design_htc, ~., 0.2, blocking = TRUE, nsim = 100),
    "high_resolution_candidate_set"
  )

  jmp_design_vhtc = read.csv(testthat::test_path("JMP_design_vhtc.csv"))
  expect_warning(
    eval_design(jmp_design_vhtc, ~., 0.2, blocking = TRUE),
    "high_resolution_candidate_set"
  )
  expect_warning(
    eval_design_mc(jmp_design_vhtc, ~., 0.2, blocking = TRUE, nsim = 100),
    "high_resolution_candidate_set"
  )
})

test_that("Test various configurations of blocking columns", {
  candset = expand.grid(a = c(1, -1), b = c(1, -1), c = c(1, -1), d = c(1, -1))
  set.seed(1)
  expect_silent({
    gen_design(candset, ~a, 6) |>
      gen_design(candset, ~ a + b, trials = 12, splitplotdesign = _) |>
      gen_design(
        candset,
        ~ a + b + c,
        trials = 24,
        splitplotdesign = _
      ) -> rownames_spd
  })

  expect_warning(
    {
      gen_design(candset, ~a, 6) |>
        gen_design(candset, ~ a + b, trials = 13, splitplotdesign = _) |>
        gen_design(
          candset,
          ~ a + b + c,
          trials = 24,
          splitplotdesign = _
        ) -> rownames_spd_onerep
    },
    regexp = "replicate"
  )

  set.seed(1)
  expect_silent({
    gen_design(candset, ~a, 6) |>
      gen_design(candset, ~ a + b, trials = 12, splitplotdesign = _) |>
      gen_design(
        candset,
        ~ a + b + c,
        trials = 24,
        splitplotdesign = _,
        add_blocking_columns = TRUE
      ) -> cols_spd
  })

  expect_warning(
    {
      gen_design(candset, ~a, 6) |>
        gen_design(candset, ~ a + b, trials = 13, splitplotdesign = _) |>
        gen_design(
          candset,
          ~ a + b + c,
          trials = 24,
          splitplotdesign = _,
          add_blocking_columns = TRUE
        ) -> cols_spd_onerep
    },
    regexp = "replicate"
  )

  expect_silent({
    pow_rownames = eval_design(rownames_spd)
  })
  expect_silent({
    pow_rownames_onerep = eval_design(rownames_spd_onerep)
  })
  expect_warning(
    {
      pow_cols = eval_design(cols_spd)
    },
    regexp = "extra block column"
  )
  expect_warning(
    {
      pow_cols_onerep = eval_design(cols_spd_onerep)
    },
    regexp = "extra block column"
  )

  expect_equal(pow_rownames$power, pow_cols$power)
  expect_equal(pow_cols_onerep$power, pow_rownames_onerep$power)

  set.seed(1)
  expect_no_error({
    pow_rownames_mc = eval_design_mc(rownames_spd, nsim = 100)
  })
  expect_no_error({
    pow_rownames_onerep_mc = eval_design_mc(rownames_spd_onerep, nsim = 100)
  })
  set.seed(1)
  expect_warning(
    {
      pow_cols_mc = eval_design_mc(cols_spd, nsim = 100)
    },
    regexp = "extra block column"
  )
  expect_warning(
    {
      pow_cols_onerep_mc = eval_design_mc(cols_spd_onerep, nsim = 100)
    },
    regexp = "extra block column"
  )

  expect_equal(pow_rownames_mc$power, pow_cols_mc$power)
  expect_equal(pow_cols_onerep_mc$power, pow_rownames_onerep_mc$power)

  rownames_spd_copy = rownames_spd
  rownames_spd_copy$Block1 = rep(sample(1:8, 8), each = 3)
  rownames_spd_copy$Block2 = rep(sample(1:12, 12), each = 2)

  expect_warning(
    {
      eval_design(rownames_spd_copy, blocking = T)
    },
    regexp = "extra block columns"
  )
  expect_warning(
    {
      eval_design_mc(rownames_spd_copy, blocking = T, nsim = 100)
    },
    regexp = "extra block columns"
  )
})


test_that("Test moment matrix when using constrained split plot designs", {
  large_cs = expand.grid(
    A = c(-1, 1),
    B = c(-1, 1),
    C = c(-1, 1),
    D = c(-1, 1),
    E = c(-1, 1),
    F = c(-1, 1),
    G = c(-1, 1)
  )

  large_cs = large_cs[with(large_cs, (D + E + F + G) <= 2), ]

  whole_plots = gen_design(
    candidateset = large_cs,
    model = ~ A + B + C,
    trials = 4
  )

  testthat::expect_no_error(gen_design(
    candidateset = large_cs,
    model = ~ A + B + C * (D + E + F + G),
    trials = 31,
    splitplotdesign = whole_plots,
    optimality = "D",
    repeats = 20
  ))
})
