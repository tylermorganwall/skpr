context("I-optimality stuff")

set.seed(1)
library(testthat)

test_that("moment matrx self-consistent throughout ", {
  rm(list = ls(envir = skpr:::skpr_moment_matrix_cache), envir = (skpr:::skpr_moment_matrix_cache))

  cand_set_simple = expand.grid(
    X1 = c(-1, 1),
    X2 = c(-1, 1)
  )
  #Basic design
  get_from_gen_evals = function(cs, model, ...) {
    trials = ncol(model.matrix(model, data = cs)) + 5
    des = gen_design(candidateset = cs, model = model, trials = trials, ...)
    eval = eval_design(des, ...)
    eval_mc = eval_design_mc(des, nsim=1, ...)
    iopt_des = get_optimality(des, "I")
    iopt_eval = get_optimality(eval, "I")
    iopt_eval_mc = get_optimality(eval_mc, "I")
    stopifnot(as.logical(iopt_des == iopt_eval && iopt_eval == iopt_eval_mc))
  }
  expect_silent(get_from_gen_evals(cand_set_simple, ~.))
  expect_silent(get_from_gen_evals(cand_set_simple, ~.*.))
  # expect_true(length(skpr:::skpr_moment_matrix_cache) == 0)

  cand_set_cat = expand.grid(
    X1 = c(-1, 1),
    X2 = c(-1, 1),
    X3 = letters[1:3]
  )

  expect_silent(get_from_gen_evals(cand_set_cat, ~.))
  expect_silent(get_from_gen_evals(cand_set_cat, ~.*.))
  # expect_true(length(skpr:::skpr_moment_matrix_cache) == 0)

  cand_set_cat_high = expand.grid(
    X1 = seq(-1, 1, by=0.2),
    X2 = seq(-1, 1, by=0.2),
    X3 = letters[1:3]
  )
  expect_silent(get_from_gen_evals(cand_set_cat_high, ~.))
  expect_silent(get_from_gen_evals(cand_set_cat_high, ~.*.))
  # expect_true(length(skpr:::skpr_moment_matrix_cache) == 0)

  cand_set_cat_high_dc = expand.grid(
    X1 = seq(-1, 1, by=0.2),
    X2 = seq(-1, 1, by=0.2),
    X3 = letters[1:3]
  )

  cand_set_cat_high_dc = cand_set_cat_high_dc[cand_set_cat_high_dc$X1 + cand_set_cat_high_dc$X2 > 0,]
  expect_warning(get_from_gen_evals(cand_set_cat_high_dc, ~.), "convex hull")
  expect_warning(get_from_gen_evals(cand_set_cat_high_dc, ~.*.), "convex hull")
  if(length(skpr:::skpr_moment_matrix_cache) != 2) {
    testthat::testthat_print(as.list(skpr:::skpr_moment_matrix_cache))
  }
  # expect_true(length(skpr:::skpr_moment_matrix_cache) == 2)

  # Clear cache
  rm(list = ls(envir = skpr:::skpr_moment_matrix_cache), envir = (skpr:::skpr_moment_matrix_cache))

  cand_set_cat_highres_dc = expand.grid(
    X1 = seq(-1, 1, by=0.1),
    X2 = seq(-1, 1, by=0.1),
    X3 = letters[1:3]
  )

  expect_silent(get_from_gen_evals(cand_set_cat_high_dc, ~., high_resolution_candidate_set = cand_set_cat_highres_dc))
  expect_silent(get_from_gen_evals(cand_set_cat_high_dc, ~.*., high_resolution_candidate_set = cand_set_cat_highres_dc))
  expect_silent(get_from_gen_evals(cand_set_cat_high_dc, ~., high_resolution_candidate_set = cand_set_cat_highres_dc, moment_sample_density = 30))
  expect_silent(get_from_gen_evals(cand_set_cat_high_dc, ~.*., high_resolution_candidate_set = cand_set_cat_highres_dc, moment_sample_density = 30))

  #These shouldn't get saved on the cache since the user passed a high res candidate set
  # expect_true(length(skpr:::skpr_moment_matrix_cache) == 0)

  expect_silent(get_from_gen_evals(cand_set_cat_high_dc, ~., high_resolution_candidate_set = cand_set_cat_highres_dc))
  expect_silent(get_from_gen_evals(cand_set_cat_high_dc, ~.*., high_resolution_candidate_set = cand_set_cat_highres_dc))
  expect_silent(get_from_gen_evals(cand_set_cat_high_dc, ~., high_resolution_candidate_set = cand_set_cat_highres_dc, moment_sample_density = 30))
  expect_silent(get_from_gen_evals(cand_set_cat_high_dc, ~.*., high_resolution_candidate_set = cand_set_cat_highres_dc, moment_sample_density = 30))

  #These shouldn't get saved on the cache since the user passed a high res candidate set
  # expect_true(length(skpr:::skpr_moment_matrix_cache) == 0)

})


test_that("moment matrix comparison to JMP", {
  cand_set = expand.grid(
    X1 = seq(-1, 1, by = 1),
    X2 = seq(-1, 1, by = 1),
    X3 = letters[1:3]
  )


  #Basic I Opt
  des = gen_design(candidateset = cand_set, model = ~X1+X2, trials = 24)
  moment_matrix = attr(des, "moments.matrix")
  jmp_mm = matrix(c(1,0,0,
                    0,1/3,0,
                    0,0,1/3), ncol=3,nrow=3)
  expect_true(all(dim(moment_matrix) == dim(jmp_mm)))
  expect_true(all(jmp_mm - moment_matrix == 0))
})

test_that("moment matrix comparison to JMP - X1+X2 with X1+X2<=0", {
  # Extract JSL moment matrix
  jmp_mm = matrix(c(
    1, -0.32831622049743, -0.338008572648527,
    -0.32831622049743, 0.334521518563617, -0.00144258659922306,
    -0.338008572648527, -0.00144258659922306, 0.336950582592268
  ), ncol=3, nrow=3)

  # Generate design and compare
  cand_set = expand.grid(
    X1 = seq(-1, 1, by = 1),
    X2 = seq(-1, 1, by = 1)
  )
  cand_set_hr = expand.grid(
    X1 = seq(-1, 1, by = 0.1),
    X2 = seq(-1, 1, by = 0.1)
  )
  cand_set = cand_set[with(cand_set, X1 + X2 <= 0), ]
  cand_set_hr = cand_set_hr[with(cand_set_hr, X1 + X2 <= 0), ]


  expect_warning({des = gen_design(candidateset = cand_set, model = ~X1+X2, trials = 24)}, "convex hull")
  moment_matrix = attr(des, "moments.matrix")

  expect_true(all(dim(moment_matrix) == dim(jmp_mm)))
  expect_true(cor.test(jmp_mm, moment_matrix)$estimate > 0.99)

  expect_silent({des = gen_design(candidateset = cand_set, model = ~X1+X2, trials = 24, high_resolution_candidate_set = cand_set_hr)})
  moment_matrix = attr(des, "moments.matrix")

  expect_true(all(dim(moment_matrix) == dim(jmp_mm)))
  expect_true(cor.test(jmp_mm, moment_matrix)$estimate > 0.99)

})

test_that("moment matrix comparison to JMP - X1+X2 with X1+2*X2<=0", {
  # Extract JSL moment matrix
  jmp_mm = matrix(c(
    1, -0.171760227638198, -0.458439128798939,
    -0.171760227638198, 0.336147134443655, 0.00498358967306505,
    -0.458439128798939, 0.00498358967306505, 0.330233173034156
  ), ncol=3, nrow=3)

  # Generate design and compare
  cand_set = expand.grid(
    X1 = seq(-1, 1, by = 0.25),
    X2 = seq(-1, 1, by = 0.25)
  )
  cand_set_hr = expand.grid(
    X1 = seq(-1, 1, by = 0.01),
    X2 = seq(-1, 1, by = 0.01)
  )
  cand_set = cand_set[with(cand_set, X1 + 2*X2 <= 0), ]
  cand_set_hr = cand_set_hr[with(cand_set_hr, X1 + 2 * X2 <= 0), ]

  expect_warning({des = gen_design(candidateset = cand_set, model = ~X1+X2, trials = 24, moment_sample_density = 50)}, "convex hull")
  moment_matrix = attr(des, "moments.matrix")

  expect_true(all(dim(moment_matrix) == dim(jmp_mm)))
  expect_true(cor.test(jmp_mm, moment_matrix)$estimate > 0.95) # Bit worse due to cutoff

  expect_silent({des = gen_design(candidateset = cand_set, model = ~X1+X2, trials = 24, high_resolution_candidate_set = cand_set_hr)})
  moment_matrix = attr(des, "moments.matrix")

  expect_true(all(dim(moment_matrix) == dim(jmp_mm)))
  expect_true(cor.test(jmp_mm, moment_matrix)$estimate > 0.98)
  # JMP is way off here: you can numerically integrate this moments matrix in closed form and get -13/48 = 0.2708333
})

test_that("moment matrix comparison to JMP - quadratic with X1+X2<=0", {
  # Extract JSL moment matrix
  jmp_mm = matrix(c(
    1, -0.338719306634677, -0.330782978061471, 0.335680923025592, 0.00216200997703593, 0.331267965117126,
    -0.338719306634677, 0.335680923025592, 0.00216200997703593, -0.202151102526647, -0.0669484090461155, -0.0683972693867655,
    -0.330782978061471, 0.00216200997703593, 0.331267965117126, -0.0669484090461155, -0.0683972693867655, -0.198063750370433,
    0.335680923025592, -0.202151102526647, -0.0669484090461155, 0.201273135891785, 0.000803523962064587, 0.111145657615913,
    0.00216200997703593, -0.0669484090461155, -0.0683972693867655, 0.000803523962064587, 0.111145657615913, 0.00143857370729223,
    0.331267965117126, -0.0683972693867655, -0.198063750370433, 0.111145657615913, 0.00143857370729223, 0.197655382046274
  ), ncol=6, nrow=6)

  # Generate design and compare
  cand_set = expand.grid(
    X1 = seq(-1, 1, by = 1),
    X2 = seq(-1, 1, by = 1)
  )
  cand_set_hr = expand.grid(
    X1 = seq(-1, 1, by = 0.1),
    X2 = seq(-1, 1, by = 0.1)
  )
  cand_set = cand_set[with(cand_set, X1 + X2 <= 0), ]
  cand_set_hr = cand_set_hr[with(cand_set_hr, X1 + X2 <= 0), ]

  expect_warning({des = gen_design(candidateset = cand_set,
                   model = ~X1+X2+X1:X2+I(X1^2)+I(X2^2),
                   trials = 24)}, "convex hull")
  moment_matrix = attr(des, "moments.matrix")
  moment_matrix = moment_matrix[,c("(Intercept)", "X1", "X2", "I(X1^2)", "X1:X2", "I(X2^2)")]
  moment_matrix = moment_matrix[c("(Intercept)", "X1", "X2", "I(X1^2)", "X1:X2", "I(X2^2)"),]

  expect_true(all(dim(moment_matrix) == dim(jmp_mm)))
  expect_true(cor.test(jmp_mm, moment_matrix)$estimate > 0.99)

  expect_silent({des = gen_design(candidateset = cand_set, model = ~X1+X2+X1:X2+I(X1^2)+I(X2^2),
                                  trials = 24, high_resolution_candidate_set = cand_set_hr)})
  moment_matrix = attr(des, "moments.matrix")
  moment_matrix = moment_matrix[,c("(Intercept)", "X1", "X2", "I(X1^2)", "X1:X2", "I(X2^2)")]
  moment_matrix = moment_matrix[c("(Intercept)", "X1", "X2", "I(X1^2)", "X1:X2", "I(X2^2)"),]

  expect_true(all(dim(moment_matrix) == dim(jmp_mm)))
  expect_true(cor.test(jmp_mm, moment_matrix)$estimate > 0.99)
})

test_that("moment matrix comparison to JMP - quadratic with X1+2*X2<=0", {
  # Extract JSL moment matrix
  jmp_mm = matrix(c(
    1, -0.177031943959884, -0.454642581303287, 0.336836633050623, 0.000881157893811968, 0.332882739363757,
    -0.177031943959884, 0.336836633050623, 0.000881157893811968, -0.110132177375372, -0.138442400704047, -0.0102836410065994,
    -0.454642581303287, 0.000881157893811968, 0.332882739363757, -0.138442400704047, -0.0102836410065994, -0.2457758194042,
    0.336836633050623, -0.110132177375372, -0.138442400704047, 0.203951539516124, 0.00126659981125049, 0.110232838566565,
    0.000881157893811968, -0.138442400704047, -0.0102836410065994, 0.00126659981125049, 0.110232838566565, 0.000631016607699219,
    0.332882739363757, -0.0102836410065994, -0.2457758194042, 0.110232838566565, 0.000631016607699219, 0.199122123277355
  ), ncol=6, nrow=6)

  # Generate design and compare
  cand_set = expand.grid(
    X1 = seq(-1, 1, by = 0.01),
    X2 = seq(-1, 1, by = 0.01)
  )
  high_res_cand_set = expand.grid(
    X1 = seq(-1, 1, by = 0.01),
    X2 = seq(-1, 1, by = 0.01)
  )
  cand_set = cand_set[with(cand_set, X1 + 2*X2 <= 0), ]
  high_res_cand_set = high_res_cand_set[with(high_res_cand_set, X1 + 2*X2 <= 0), ]

  des = gen_design(candidateset = cand_set,
                   model = ~X1+X2+X1:X2+I(X1^2)+I(X2^2),
                   trials = 24, high_resolution_candidate_set = high_res_cand_set)
  moment_matrix = attr(des, "moments.matrix")

  moment_matrix = moment_matrix[,c("(Intercept)", "X1", "X2", "I(X1^2)", "X1:X2", "I(X2^2)")]
  moment_matrix = moment_matrix[c("(Intercept)", "X1", "X2", "I(X1^2)", "X1:X2", "I(X2^2)"),]

  expect_true(all(dim(moment_matrix) == dim(jmp_mm)))
  expect_true(cor.test(jmp_mm, moment_matrix)$estimate > 0.98)
})

test_that("moment matrix comparison to JMP - basic with categorical", {
  # Extract JSL moment matrix
  jmp_mm = matrix(c(
    1, 0, 0, 0, 0,
    0, 0.333333333333333, 0, 0, 0,
    0, 0, 0.333333333333333, 0, 0,
    0, 0, 0, 1, 0,
    0, 0, 0, 0, 1
  ), ncol=5, nrow=5)

  # Generate design and compare
  cand_set = expand.grid(
    X1 = seq(-1, 1, by = 1),
    X2 = seq(-1, 1, by = 1),
    X3 = letters[1:3]
  )

  des = gen_design(candidateset = cand_set, model = ~X1+X2+X3, trials = 24)
  moment_matrix = attr(des, "moments.matrix")

  expect_true(all(dim(moment_matrix) == dim(jmp_mm)))
  expect_true(cor.test(jmp_mm, moment_matrix)$estimate > 0.999)
})

test_that("moment matrix comparison to JMP - quadratic with categorical", {
  # Extract JSL moment matrix
  jmp_mm = matrix(c(
    1, 0, 0, 0, 0, 0.333333333333333, 0, 0.333333333333333, 0, 0, 0, 0,
    0, 0.333333333333333, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0.333333333333333, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
    0.333333333333333, 0, 0, 0, 0, 0.2, 0, 0.111111111111111, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0.111111111111111, 0, 0, 0, 0, 0,
    0.333333333333333, 0, 0, 0, 0, 0.111111111111111, 0, 0.2, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0.333333333333333, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0.333333333333333, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.333333333333333, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.333333333333333
  ), ncol=12, nrow=12)

  # Generate design and compare
  cand_set = expand.grid(
    X1 = seq(-1, 1, by = 1),
    X2 = seq(-1, 1, by = 1),
    X3 = letters[1:3]
  )

  des = gen_design(candidateset = cand_set,
                   model = ~(X1+X2+X3)^2+I(X1^2)+I(X2^2),
                   trials = 24)
  moment_matrix = attr(des, "moments.matrix")

  moment_matrix = moment_matrix[,c("(Intercept)", "X1", "X2", "X31","X32", "I(X1^2)", "X1:X2","I(X2^2)", "X1:X31", "X1:X32", "X2:X31", "X2:X32")]
  moment_matrix = moment_matrix[c("(Intercept)", "X1", "X2", "X31","X32", "I(X1^2)", "X1:X2","I(X2^2)", "X1:X31", "X1:X32", "X2:X31", "X2:X32"),]

  expect_true(all(dim(moment_matrix) == dim(jmp_mm)))
  expect_true(cor.test(jmp_mm, moment_matrix)$estimate > 0.999)
})

test_that("moment matrix comparison to JMP - quadratic categorical with X1+X2<=0", {
  # Extract JSL moment matrix
  jmp_mm = matrix(c(
    1, -0.331109218109805, -0.329937860105748, 0, 0, 0.33262206377614, -0.00301572610642962, 0.331861965851781, 0, 0, 0, 0,
    -0.331109218109805, 0.33262206377614, -0.00301572610642962, 0, 0, -0.200136216656638, -0.0648496773980609, -0.0655319129715283, 0, 0, 0, 0,
    -0.329937860105748, -0.00301572610642962, 0.331861965851781, 0, 0, -0.0648496773980609, -0.0655319129715283, -0.19722407542533, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
    0.33262206377614, -0.200136216656638, -0.0648496773980609, 0, 0, 0.201297231188111, -0.00138880135214787, 0.110563373767597, 0, 0, 0, 0,
    -0.00301572610642962, -0.0648496773980609, -0.0655319129715283, 0, 0, -0.00138880135214787, 0.110563373767597, -0.00166217607026958, 0, 0, 0, 0,
    0.331861965851781, -0.0655319129715283, -0.19722407542533, 0, 0, 0.110563373767597, -0.00166217607026958, 0.197738630611028, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0.33262206377614, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0.33262206377614, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.331861965851781, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.331861965851781
  ), ncol=12, nrow=12)

  # Generate design and compare
  cand_set = expand.grid(
    X1 = seq(-1, 1, by = 1),
    X2 = seq(-1, 1, by = 1),
    X3 = letters[1:3]
  )
  cand_set_highres = expand.grid(
    X1 = seq(-1, 1, by = 0.01),
    X2 = seq(-1, 1, by = 0.01),
    X3 = letters[1:3]
  )
  cand_set = cand_set[with(cand_set, X1 + X2 <= 0), ]
  cand_set_highres = cand_set_highres[with(cand_set_highres, X1 + X2 <= 0), ]

  expect_warning({des = gen_design(candidateset = cand_set,
                   model = ~(X1+X2+X3)^2+I(X1^2)+I(X2^2),
                   trials = 24)}, "convex hull")
  moment_matrix = attr(des, "moments.matrix")

  moment_matrix = moment_matrix[,c("(Intercept)", "X1", "X2", "X31","X32", "I(X1^2)", "X1:X2","I(X2^2)", "X1:X31", "X1:X32", "X2:X31", "X2:X32")]
  moment_matrix = moment_matrix[c("(Intercept)", "X1", "X2", "X31","X32", "I(X1^2)", "X1:X2","I(X2^2)", "X1:X31", "X1:X32", "X2:X31", "X2:X32"),]

  expect_true(all(dim(moment_matrix) == dim(jmp_mm)))
  expect_true(cor.test(jmp_mm, moment_matrix)$estimate > 0.999)

  expect_silent({des = gen_design(candidateset = cand_set,
                                   model = ~(X1+X2+X3)^2+I(X1^2)+I(X2^2),
                                   trials = 24, high_resolution_candidate_set = cand_set_highres)})
  moment_matrix = attr(des, "moments.matrix")

  moment_matrix = moment_matrix[,c("(Intercept)", "X1", "X2", "X31","X32", "I(X1^2)", "X1:X2","I(X2^2)", "X1:X31", "X1:X32", "X2:X31", "X2:X32")]
  moment_matrix = moment_matrix[c("(Intercept)", "X1", "X2", "X31","X32", "I(X1^2)", "X1:X2","I(X2^2)", "X1:X31", "X1:X32", "X2:X31", "X2:X32"),]

  expect_true(all(dim(moment_matrix) == dim(jmp_mm)))
  expect_true(cor.test(jmp_mm, moment_matrix)$estimate > 0.999)

})

test_that("moment matrix comparison to JMP - quadratic categorical with X1+2*X2<=0", {
  # Extract JSL moment matrix
  jmp_mm = matrix(c(
    1, -0.175168333352274, -0.450661581610448, 0, 0, 0.334323690708615, 0.00114462552499997, 0.329122761885771, 0, 0, 0, 0,
    -0.175168333352274, 0.334323690708615, 0.00114462552499997, 0, 0, -0.106002980815523, -0.138471727018157, -0.0104380233303779, 0, 0, 0, 0,
    -0.450661581610448, 0.00114462552499997, 0.329122761885771, 0, 0, -0.138471727018157, -0.0104380233303779, -0.243380424837416, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
    0.334323690708615, -0.106002980815523, -0.138471727018157, 0, 0, 0.199242518448438, 0.00125418129860776, 0.109491767253934, 0, 0, 0, 0,
    0.00114462552499997, -0.138471727018157, -0.0104380233303779, 0, 0, 0.00125418129860776, 0.109491767253934, 0.00155909337331514, 0, 0, 0, 0,
    0.329122761885771, -0.0104380233303779, -0.243380424837416, 0, 0, 0.109491767253934, 0.00155909337331514, 0.197421457145001, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0.334323690708615, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0.334323690708615, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.329122761885771, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.329122761885771
  ), ncol=12, nrow=12)

  # Generate design and compare
  cand_set = expand.grid(
    X1 = seq(-1, 1, by = 0.5),
    X2 = seq(-1, 1, by = 0.5),
    X3 = letters[1:3]
  )
  cand_set_highres = expand.grid(
    X1 = seq(-1, 1, by = 0.1),
    X2 = seq(-1, 1, by = 0.1),
    X3 = letters[1:3]
  )
  cand_set = cand_set[with(cand_set, X1 + 2*X2 <= 0), ]
  cand_set_highres = cand_set_highres[with(cand_set_highres, X1 + 2*X2 <= 0), ]

  expect_warning({des = gen_design(candidateset = cand_set,
                   model = ~(X1+X2+X3)^2+I(X1^2)+I(X2^2),
                   trials = 24)}, "convex hull")
  moment_matrix = attr(des, "moments.matrix")

  moment_matrix = moment_matrix[,c("(Intercept)", "X1", "X2", "X31","X32", "I(X1^2)", "X1:X2","I(X2^2)", "X1:X31", "X1:X32", "X2:X31", "X2:X32")]
  moment_matrix = moment_matrix[c("(Intercept)", "X1", "X2", "X31","X32", "I(X1^2)", "X1:X2","I(X2^2)", "X1:X31", "X1:X32", "X2:X31", "X2:X32"),]

  expect_true(all(dim(moment_matrix) == dim(jmp_mm)))
  expect_true(cor.test(jmp_mm, moment_matrix)$estimate > 0.98)

  expect_silent({des = gen_design(candidateset = cand_set,
                                   model = ~(X1+X2+X3)^2+I(X1^2)+I(X2^2),
                                   trials = 24, high_resolution_candidate_set = cand_set_highres)})
  moment_matrix = attr(des, "moments.matrix")

  moment_matrix = moment_matrix[,c("(Intercept)", "X1", "X2", "X31","X32", "I(X1^2)", "X1:X2","I(X2^2)", "X1:X31", "X1:X32", "X2:X31", "X2:X32")]
  moment_matrix = moment_matrix[c("(Intercept)", "X1", "X2", "X31","X32", "I(X1^2)", "X1:X2","I(X2^2)", "X1:X31", "X1:X32", "X2:X31", "X2:X32"),]

  expect_true(all(dim(moment_matrix) == dim(jmp_mm)))
  expect_true(cor.test(jmp_mm, moment_matrix)$estimate > 0.99)
})

#Finally, do one with an imbalance between the levels
