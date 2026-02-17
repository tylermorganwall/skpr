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
