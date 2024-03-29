library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: apps", {
  options("in_skpr_test_environment" = TRUE)
  on.exit(options("in_skpr_test_environment" = NULL), add = TRUE)
  app <- AppDriver$new(app_dir = testthat::test_path("apps"),
    variant = platform_variant(), name = "apps", height = 923,
      width = 1619, timeout = 10*1000, wait = TRUE)
  app$set_inputs(setseed = TRUE)
  app$set_inputs(seed = 5)
  app$click("submitbutton")
  app$wait_for_value(output = "runmatrix")
  app$expect_values()
  app$set_inputs(numberfactors = 2)
  app$set_inputs(numberfactors = 3)
  app$set_inputs(numberfactors = 4)
  app$set_inputs(numberfactors = 5)
  app$wait_for_idle(duration = 5000)

  app$click("submitbutton")
  app$wait_for_idle()
  app$wait_for_value(output = "runmatrix")

  app$expect_values()
  app$set_window_size(width = 1445, height = 839, wait = TRUE)
  app$wait_for_idle(duration=1000)

  app$set_inputs(model = "~.  ")
  app$set_inputs(model = "~.  +")
  app$set_inputs(model = "~.  + ")
  app$set_inputs(model = "~.  + X")
  app$set_inputs(model = "~.  + X(")
  app$set_inputs(model = "~.  + ")
  app$set_inputs(model = "~.  + I(X")
  app$set_inputs(model = "~.  + I(X1")
  app$set_inputs(model = "~.  + I(X1^2")
  app$set_inputs(model = "~.  + I(X1^2)")
  app$set_inputs(model = "~.  + I(X1^2) + X1")
  app$set_inputs(model = "~.  + I(X1^2) + X1:X2")
  app$click("submitbutton")
  app$wait_for_value(output = "runmatrix")
  app$expect_values()
  app$set_inputs(factortype2 = "cat")
  app$click("submitbutton")
  app$expect_values()
  app$wait_for_value(output = "runmatrix")
  app$set_inputs(parallel = TRUE)
  app$click("submitbutton")
  app$expect_values()
  app$wait_for_value(output = "runmatrix")
  app$expect_values()

  app$set_inputs(results_panels = "eval")
  app$click("evalbutton")
  app$wait_for_value(output = "powerresults")

  app$expect_values()

  app$set_inputs(conservative = TRUE)
  app$click("evalbutton")
  app$wait_for_value(output = "powerresults")

  app$expect_values()

  app$set_inputs(evaltype = "glm")
  app$expect_values()

  app$set_inputs(adjust_alpha = TRUE)
  app$click("evalbutton")
  app$wait_for_value(output = "powerresultsglm")

  app$expect_values()

  app$set_inputs(parallel_eval_glm = TRUE)
  app$click("evalbutton")
  app$wait_for_value(output = "powerresultsglm")

  app$expect_values()

  app$set_inputs(glmfamily = "binomial")
  app$set_inputs(firth_correction = FALSE)
  app$set_inputs(adjust_alpha = FALSE)
  app$set_inputs(parallel_eval_glm = FALSE)
  app$click("evalbutton")
  app$wait_for_value(output = "powerresultsglm")
  app$expect_values()

  app$set_inputs(nsim = 100)
  app$set_inputs(firth_correction = TRUE)
  app$click("evalbutton")
  app$wait_for_value(output = "powerresultsglm")

  app$expect_values()

  app$set_inputs(adjust_alpha = TRUE)
  app$click("evalbutton")
  app$wait_for_value(output = "powerresultsglm")

  app$expect_values()

  app$set_inputs(parallel_eval_glm = TRUE)
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultsglm")

  app$expect_values()

  app$set_inputs(glmfamily = "poisson")
  app$set_inputs(adjust_alpha = FALSE)
  app$set_inputs(parallel_eval_glm = FALSE)
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultsglm")

  app$expect_values()


  app$set_inputs(adjust_alpha = TRUE)
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultsglm")

  app$expect_values()


  app$set_inputs(parallel_eval_glm = TRUE)
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultsglm")

  app$expect_values()


  app$set_inputs(glmfamily = "exponential")
  app$set_inputs(parallel_eval_glm = FALSE)
  app$set_inputs(adjust_alpha = FALSE)
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultsglm")

  app$expect_values()


  app$set_inputs(adjust_alpha = TRUE)
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultsglm")

  app$expect_values()


  app$set_inputs(parallel_eval_glm = TRUE)
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultsglm")

  app$expect_values()


  app$set_inputs(evaltype = "surv")
  app$set_inputs(censorpoint = 1)
  app$set_inputs(nsim_surv = 10000)
  app$set_inputs(nsim_surv = 100)
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultssurv")
  app$wait_for_idle()

  app$expect_values()


  app$set_inputs(censortype = "left")
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultssurv")

  app$expect_values()


  app$set_inputs(censortype = "right")
  app$set_inputs(parallel_eval_surv = TRUE)
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultssurv")

  app$expect_values()


  app$set_inputs(censortype = "left")
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultssurv")

  app$expect_values()


  app$set_inputs(distribution = "exponential")
  app$set_inputs(parallel_eval_surv = FALSE)
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultssurv")

  app$expect_values()


  app$set_inputs(censortype = "right")
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultssurv")

  app$expect_values()


  app$set_inputs(exphigh = 3)
  app$set_inputs(exphigh = 4)
  app$set_inputs(exphigh = 5)
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultssurv")

  app$expect_values()


  app$set_inputs(explow = 2)
  app$set_inputs(explow = 3)
  app$set_inputs(censorpoint = 2)
  app$set_inputs(censorpoint = 3)
  app$set_inputs(censorpoint = 4)
  app$set_inputs(censorpoint = 5)
  app$set_inputs(censorpoint = 6)
  app$set_inputs(censorpoint = 7)
  app$set_inputs(censorpoint = 8)
  app$set_inputs(censorpoint = 9)
  app$set_inputs(censorpoint = 10)
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultssurv")

  app$expect_values()


  app$set_inputs(alpha = 0.2)
  app$set_inputs(alpha = 0.21)
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultssurv")

  app$expect_values()


  app$set_inputs(parallel_eval_surv = TRUE)
  app$click("evalbutton", timeout_ = 20*1000)
  app$wait_for_value(output = "powerresultssurv")

  app$expect_values()


  app$set_inputs(results_panels = "code")
  app$set_inputs(distribution = "gaussian")
  app$set_inputs(distribution = "lognormal")
  app$set_inputs(distribution = "exponential")
  app$set_inputs(censortype = "left")
  app$set_inputs(colorblind = TRUE)
  app$set_inputs(colorblind = FALSE)
  app$set_inputs(explow = 2)
  app$set_inputs(explow = 1)
  app$set_inputs(evaltype = "glm")
  app$set_inputs(explow = 2)
  app$set_inputs(explow = 3)
  app$set_inputs(exphigh = 4)
  app$set_inputs(exphigh = 3)
  app$set_inputs(nsim = 101)
  app$set_inputs(nsim = 102)
  app$set_inputs(nsim = 103)
  app$set_inputs(nsim = 104)
  app$set_inputs(adjust_alpha = FALSE)
  app$set_inputs(adjust_alpha = TRUE)
  app$set_inputs(adjust_alpha = FALSE)
  app$set_inputs(adjust_alpha = TRUE)
  app$set_inputs(adjust_alpha = FALSE)
  app$set_inputs(parallel_eval_glm = FALSE)
  app$set_inputs(parallel_eval_glm = TRUE)
  app$set_inputs(alpha = 0.14)
  app$set_inputs(numberfactors = 6)
  app$set_inputs(numberfactors = 7)
  app$set_inputs(numberfactors = 8)
  app$expect_values()
  app$set_inputs(results_panels = "eval")
  app$set_inputs(results_panels = "design")
})
