library(lme4)

context("Run Examples")

set.seed(1)

test_that("eval_design example code runs without errors", {
  prev_options = options()
  options("skpr_progress" = FALSE)
  on.exit(options(prev_options), add = TRUE)

  #'#this can also be generated with expand.grid as:
  #'
  factorialdes <- expand.grid(A = c(1, -1), B = c(1, -1), C = c(1, -1))
  expect_silent({optdesign = gen_design(candidateset = factorialdes, model = ~A + B + C, trials = 17, optimality = "D", repeats = 100)})
  #'
  #'#Now evaluating that design (with default anticipated coefficients and a effectsize of 2):
  expect_silent(eval_design(design = optdesign, model = ~A + B + C, alpha = 0.2))
  #'
  #'#Evaluating a subset of the design (changing the power due to a different number of
  #'#degrees of freedom)
  expect_silent(eval_design(design = optdesign, model = ~A + C, alpha = 0.2))
  #'
  #'#Halving the signal-to-noise ratio by setting a different effectsize (default is 2):
  expect_silent(eval_design(design = optdesign, model = ~A + B + C, alpha = 0.2, effectsize = 1))

  #'#Trying with ~.*. operator
  expect_silent(eval_design(design = optdesign, model = ~. * ., alpha = 0.2, effectsize = 1))

  #'
  #'#With 3+ level categorical factors, the choice of anticipated coefficients directly changes the
  #'#final power calculation. For the most conservative power calculation, that involves
  #'#setting all anticipated coefficients in a factor to zero except for one. We can specify this
  #'#option with the "conservative" argument.
  #'
  expect_silent({factorialcoffee = expand.grid(caffeine = c(1, -1),
                                               cost = c(1, 2),
                                type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
                                size = as.factor(c("Short", "Grande", "Venti")))})
  #'
  expect_silent({designcoffee = gen_design(factorialcoffee, ~cost + size + type, trials = 29, optimality = "D", repeats = 100)})
  #'
  #'#Evaluate the design, with default anticipated coefficients (conservative is FALSE by default).
  expect_silent(eval_design(designcoffee, model = ~cost + size + type, alpha = 0.05))
  #'
  #'#Evaluate the design, with conservative anticipated coefficients:
  expect_silent(eval_design(designcoffee, model = ~cost + size + type, alpha = 0.05, conservative = TRUE))
  #'
  #'#which is the same as the following, but now explicitly entering the coefficients:
  expect_silent(eval_design(designcoffee, model = ~cost + size + type, alpha = 0.05, anticoef = c(1, 1, 1, 0, 0, 1, 0)))
  #'
  #'#If the first level in a factor is not the one that you want to set to one
  #'#in the conservative calculation, enter the anticipated coefficients in manually.
  expect_silent(eval_design(designcoffee, model = ~cost + size + type, alpha = 0.05, anticoef = c(1, 1, 0, 0, 1, 0, 1)))
  #'
  #'#You can also evaluate the design with higher order effects:
  expect_silent(eval_design(designcoffee, model = ~cost + size + type + cost * type, alpha = 0.05))
  #'
  #'#Blocked designs can also be evaluated by specifying the blocking model.
  #'
  #'#Generating blocked design
  expect_silent({coffeeblockdesign = gen_design(factorialcoffee, ~caffeine, trials = 12)})
  expect_silent({coffeefinaldesign = gen_design(factorialcoffee, model = ~caffeine + cost + size + type, trials = 36,
                                 splitplotdesign = coffeeblockdesign)})
  #'
  #'#Evaluating design
  expect_silent(eval_design(coffeefinaldesign,  model = ~cost + size + type + caffeine, alpha = 0.2, blocking = TRUE))
  #'
  #'#We can also evaluate the design with a custom ratio between the whole plot error to
  #'#the run-to-run error.
  expect_silent(eval_design(coffeefinaldesign,  model = ~cost + size + type + caffeine, alpha = 0.2, blocking = TRUE,
              varianceratio = 2))

  expect_silent(eval_design(coffeefinaldesign,  model = ~cost + size + type + caffeine, alpha = 0.2, blocking = TRUE,
                             varianceratio = c(2, 1)))
})

test_that("gen_design parallel example code runs without errors", {
  prev_options = options()
  options("skpr_progress" = FALSE)
  on.exit(options(prev_options), add = TRUE)
  skip_on_cran()
  set.seed(1)
  candlist3 = expand.grid(Location = as.character(c("East", "West")),
                          Climate = as.factor(c("Dry", "Wet", "Arid")),
                          Vineyard = as.factor(c("A", "B", "C", "D")),
                          Age = c(1, -1))

  options(cores = 2)
  gen_design(candlist3, ~Location, trials = 6, parallel = TRUE, progress = FALSE)
  expect_silent(gen_design(candlist3, ~Location, trials = 6, parallel = TRUE, progress = FALSE) -> temp)
  expect_silent(gen_design(candlist3, ~Location + Climate, trials = 12, splitplotdesign = temp, blocksizes = rep(2, 6), parallel = TRUE, progress = FALSE) -> temp)
  expect_silent(gen_design(candlist3, ~Location, trials = 6, parallel = TRUE, progress = FALSE) -> temp)
  expect_silent(gen_design(candlist3, ~Location + Climate, trials = 12, splitplotdesign = temp, blocksizes = rep(2, 6), parallel = TRUE, progress = FALSE) -> temp2)
  expect_silent(eval_design_mc(temp, ~., 0.2, nsim = 10, parallel = TRUE, progress = FALSE))
  expect_silent(eval_design_mc(temp2, ~., 0.2, nsim = 10, parallel = TRUE, progress = FALSE))
  expect_silent(eval_design_mc(temp, ~., 0.2, nsim = 10, glmfamily = "poisson", effectsize = c(1, 10), parallel = TRUE))
  expect_silent(eval_design_mc(temp2, ~., 0.2, nsim = 10, glmfamily = "poisson", effectsize = c(1, 10), parallel = TRUE))
  expect_warning(eval_design_mc(temp, ~., 0.2, nsim = 10, glmfamily = "poisson"), " This can lead to unrealistic effect sizes")


})


test_that("eval_design_mc example code runs without errors", {
  set.seed(123)
  prev_options = options()
  options("skpr_progress" = FALSE)
  on.exit(options(prev_options), add = TRUE)

  #'@examples #We first generate a full factorial design using expand.grid:
  factorialcoffee = expand.grid(cost = c(-1, 1),
                                type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
                                size = as.factor(c("Short", "Grande", "Venti")))
  expect_silent({
    designcoffee = gen_design(factorialcoffee, model = ~cost + type + size, trials = 21, optimality = "D")
  })
  expect_silent(eval_design(design = designcoffee, model = ~cost + type + size, 0.05))
  expect_silent(eval_design_mc(design = designcoffee, model = ~cost + type + size, alpha = 0.05, nsim = 100,
                               glmfamily = "gaussian"))
  expect_silent(eval_design_mc(design = designcoffee, model = ~cost + type + size, alpha = 0.05,
                               nsim = 100, glmfamily = "gaussian", effectsize = 1))
  expect_silent(eval_design_mc(design = designcoffee, model = ~cost + type, alpha = 0.05,
                 nsim = 100, glmfamily = "gaussian"))
  expect_silent(eval_design_mc(design = designcoffee, model = ~cost + type + size, 0.05,
                 nsim = 100, glmfamily = "gaussian"))
  expect_silent(eval_design_mc(design = designcoffee, model = ~cost + type + size + cost * type, 0.05,
                 nsim = 100, glmfamily = "gaussian"))
  #'\dontrun{eval_design_mc(design = designcoffee, model = ~cost + type + size, 0.05,
  #'               nsim = 10000, glmfamily = "gaussian", parallel = TRUE)}
  #'
  #'
  #'#Trying with ~.*. operator
  expect_silent(eval_design_mc(design = designcoffee, model = ~. * ., 0.05,
                               nsim = 100, glmfamily = "gaussian"))

  factorialcoffee = expand.grid(Temp = c(1, -1),
                                Store = as.factor(c("A", "B")),
                                cost = c(-1, 1),
                                type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
                                size = as.factor(c("Short", "Grande", "Venti")))

  vhtcdesign = gen_design(candidateset = factorialcoffee, model = ~Store, trials = 8)
  htcdesign = gen_design(candidateset = factorialcoffee, model = ~Store + Temp, trials = 24, splitplotdesign = vhtcdesign, blocksizes = 3)
  expect_silent({
    splitplotdesign = gen_design(candidateset = factorialcoffee, model = ~Store + Temp + cost + type + size, trials = 96,
                               splitplotdesign = htcdesign, blocksizes = 4, varianceratio = 3)
  })
  expect_silent(eval_design_mc(design = splitplotdesign, model = ~Store + Temp + cost + type + size, alpha = 0.05, blocking = TRUE,
                                nsim = 1, glmfamily = "gaussian", varianceratios = c(5, 4)))
  expect_silent(eval_design_mc(design = splitplotdesign, model = ~Store + Temp + cost + type + size, alpha = 0.05, blocking = TRUE,
                               nsim = 1, glmfamily = "gaussian", varianceratios = c(5, 4)))
  expect_error(eval_design_mc(design = splitplotdesign, model = ~Store + Temp + cost + type + size, alpha = 0.05, blocking = TRUE,
                                nsim = 1, glmfamily = "gaussian", varianceratios = c(5, 4, 2, 2)),"Wrong number of variance ratios specified.")
  expect_silent(eval_design_mc(design = splitplotdesign, model = ~Store + Temp + cost + type + size, alpha = 0.05, blocking = TRUE,
                                nsim = 1, glmfamily = "gaussian", varianceratios = c(5, 4, 2)))
  expect_silent(eval_design_mc(design = splitplotdesign, model = ~Store + Temp + cost + type + size, alpha = 0.05, blocking = TRUE,
                                nsim = 1, glmfamily = "gaussian", varianceratios = c(5, 4)))

  factorialbinom = expand.grid(a = c(-1, 1), b = c(-1, 1))
  expect_silent({
    designbinom = gen_design(factorialbinom, model = ~a + b, trials = 90, optimality = "D", repeats = 100)
  })
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "gaussian"))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "binomial"))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "poisson"))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "exponential"))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "binomial", advancedoptions = list(anovatest = "LR")))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "poisson", advancedoptions = list(anovatest = "LR")))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "exponential", advancedoptions = list(anovatest = "LR")))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "binomial", advancedoptions = list(anovatest = "F")))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "poisson", advancedoptions = list(anovatest = "F")))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "exponential", advancedoptions = list(anovatest = "F")))

  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "gaussian", advancedoptions = list(anovatype = "II")))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "binomial", advancedoptions = list(anovatype = "II")))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "poisson", advancedoptions = list(anovatype = "II")))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "exponential", advancedoptions = list(anovatype = "II")))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "binomial", advancedoptions = list(anovatest = "LR", anovatype = "II")))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "poisson", advancedoptions = list(anovatest = "LR", anovatype = "II")))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "exponential", advancedoptions = list(anovatest = "LR", anovatype = "II")))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "binomial", advancedoptions = list(anovatest = "F", anovatype = "II")))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "poisson", advancedoptions = list(anovatest = "F", anovatype = "II")))
  expect_silent(eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, anticoef = c(1.5, 0.7, 0.7),
                               glmfamily = "exponential", advancedoptions = list(anovatest = "F", anovatype = "II")))

  factorialpois = expand.grid(a = as.numeric(c(-1, 0, 1)), b = c(-1, 0, 1))
  designpois = gen_design(factorialpois, ~a + b, trials = 90, optimality = "D", repeats = 100)

})


test_that("eval_design_survival_mc example code runs without errors", {
  basicdesign = expand.grid(a = c(-1, 1))
  prev_options = options()
  options("skpr_progress" = FALSE)
  on.exit(options(prev_options), add = TRUE)

  expect_silent({
    design = gen_design(candidateset = basicdesign, model = ~a, trials = 100,
                            optimality = "D", repeats = 100)
  })
  rsurvival = function(X, b) {
    Y = rexp(n = nrow(X), rate = exp(-(X %*% b)))
    censored = Y > 1
    Y[censored] = 1
    return(survival::Surv(time = Y, event = !censored, type = "right"))
  }
  expect_warning({
    a = eval_design_survival_mc(design = design, model = ~a, alpha = 0.05, nsim = 100,
                          distribution = "exponential", rfunctionsurv = rsurvival, effectsize = 1)
  },
  "default or length 1 delta used with distribution == 'exponential'. This can lead to unrealistic effect sizes - make sure the generated anticipated coeffcients are appropriate.")

  rlognorm = function(X, b) {
    Y = rlnorm(n = nrow(X), meanlog = X %*% b, sdlog = 0.4)
    censored = Y > 1.2
    Y[censored] = 1.2
    return(survival::Surv(time = Y, event = !censored, type = "right"))
  }
  expect_silent(
    eval_design_survival_mc(design = design, model = ~a, alpha = 0.2, nsim = 100,
                          distribution = "lognormal", rfunctionsurv = rlognorm,
                          anticoef = c(0.184, 0.101), scale = 0.4)
  )
  #testing parallel
  options(cores = 2)
  eval_design_survival_mc(design = design, model = ~a, alpha = 0.05, effectsize = c(1, 2),
                          nsim = 100, distribution = "exponential",
                           censorpoint = 5, censortype = "right", parallel = TRUE)
  options(cores = NULL)
})

test_that("eval_design_custom_mc example code runs without errors", {
  prev_options = options()
  options("skpr_progress" = FALSE)
  on.exit(options(prev_options), add = TRUE)
  #'@examples #To demonstrate how a user can use their own libraries for Monte Carlo power generation,
  #'#We will recreate eval_design_survival_mc using the eval_design_custom_mc framework.
  #'
  #'#To begin, first let us generate the same design and random generation function shown in the
  #'#eval_design_survival_mc examples:
  #'
  basicdesign = expand.grid(a = c(-1, 1))
  expect_silent(design <- gen_design(candidateset = basicdesign, model = ~a, trials = 100,
                            optimality = "D", repeats = 100))
  #'
  #'#Random number generating function
  #'
  rsurvival = function(X, b) {
    Y = rexp(n = nrow(X), rate = exp(-(X %*% b)))
    censored = Y > 1
    Y[censored] = 1
    return(survival::Surv(time = Y, event = !censored, type = "right"))
  }
  #'
  #'#We now need to tell the package how we want to fit our data,
  #'#given the formula and the model matrix X (and, if needed, the list of contrasts).
  #'#If the contrasts aren't required, "contrastlist" should be set to NULL.
  #'#This should return some type of fit object.
  #'
  fitsurv = function(formula, X, contrastlist = NULL) {
    return(survival::survreg(formula, data = X, dist = "exponential"))
  }
  #'
  #'
  #'#We now need to tell the package how to extract the p-values from the fit object returned
  #'#from the fit function. This is how to extract the p-values from the survreg fit object:
  #'
  pvalsurv = function(fit) {
    return(summary(fit)$table[, 4])
  }
  #'
  #'#And now we evaluate the design, passing the fitting function and p-value extracting function
  #'#in along with the standard inputs for eval_design_mc.
  #'
  expect_silent(eval_design_custom_mc(design = design, model = ~a, alpha = 0.05, nsim = 100,
                        fitfunction = fitsurv, pvalfunction = pvalsurv, rfunction = rsurvival, effectsize = 1))

  #trying with ~. operator
  expect_silent(eval_design_custom_mc(design = design, model = ~., alpha = 0.05, nsim = 100,
                                      fitfunction = fitsurv, pvalfunction = pvalsurv, rfunction = rsurvival, effectsize = 1))

  #testing parallel

  options(cores = c("localhost", "localhost"))
  basicdesign = expand.grid(a = c(-1, 1))
  design = gen_design(candidateset = basicdesign, model = ~a, trials = 100,
                      optimality = "D", repeats = 100)

  rsurvival = function(X, b) {
    Y = rexp(n = nrow(X), rate = exp(-(X %*% b)))
    censored = Y > 1
    Y[censored] = 1
    return(survival::Surv(time = Y, event = !censored, type = "right"))
  }

  fitsurv = function(formula, X, contrastslist = NULL) {
    return(survival::survreg(formula, data = X, dist = "exponential"))
  }

  pvalsurv = function(fit) {
    return(summary(fit)$table[, 4])
  }

  expect_silent({d = eval_design_custom_mc(design = design, model = ~a, alpha = 0.05, nsim = 100,
                          fitfunction = fitsurv, pvalfunction = pvalsurv, rfunction = rsurvival, effectsize = 1, parallel = TRUE)})
})
