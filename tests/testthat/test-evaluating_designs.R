test_that("logical vectors are converted to factors and work", {
  cs = expand.grid(a=c(1,-1),b=c("A","B","C"), d=c(TRUE, FALSE),num = 1:10)
  testthat::expect_no_error(eval_design(cs,model=~.,alpha=0.2))
  testthat::expect_no_error(eval_design_mc(cs,model=~.,alpha=0.2, nsim=10))
  testthat::expect_no_error(eval_design_survival_mc(cs,model=~.,alpha=0.2, nsim=10))
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

  testthat::expect_no_error(
  eval_design_custom_mc(design = cs, model = ~.*.,
                        alpha = 0.05, nsim = 10,
                        fitfunction = fitsurv, pvalfunction = pvalsurv,
                        rfunction = rsurvival, effectsize = 1)
  )

})

test_that("parallel blocked Poisson summaries handle the RX fallback like serial summaries", {
  skip_if_not_installed("mbest")
  skip_if_not_installed("lmtest")
  design = data.frame(
    Location = factor(rep(c("West", "East", "West"), c(4, 6, 2))),
    Climate = factor(c("Dry", "Arid", "Dry", "Wet", "Arid", "Dry",
                       "Wet", "Arid", "Dry", "Wet", "Arid", "Wet"))
  )
  rownames(design) = paste(rep(1:6, each = 2), seq_len(12), sep = ".")
  attr(design, "candidate_set") = unique(design)
  # Every Dry count is zero. This response triggers the Hessian fallback in lme4 1.1-37.
  simulate_counts = function(X, b, blockvector) c(0, 1, 0, 2, 11, 0, 18, 51, 0, 10, 2, 0)
  evaluate = function(parallel) {
    eval_design_mc(design, ~., 0.2, nsim = 2, blocking = TRUE, glmfamily = "poisson",
                   effectsize = c(1, 10), parallel = parallel, rfunction = simulate_counts,
                   progress = FALSE)
  }
  serial = evaluate(FALSE)
  expect_silent(parallel_result <- evaluate(TRUE))
  expect_equal(parallel_result$power, serial$power)
  for (name in c("pvals", "effect_pvals", "stderrors")) {
    expect_equal(unname(attr(parallel_result, name)), unname(attr(serial, name)))
  }
})
