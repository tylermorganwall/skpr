#'@title Evaluate Power for Survival Design
#'
#'@description Evaluates power for a right censored survival design with a Monte Carlo simulation,
#'using the survival package and survreg to fit the data.
#'
#'@param RunMatrix The run matrix of the design.
#'@param model The model used in the evaluation.
#'@param alpha The type-I error.
#'@param nsim The number of simulations.
#'@param distribution Distribution of survival function
#'@param rfunctionsurv Random number generator function. Should be a function of the form f(X,b), where X is the
#'model matrix and b are the anticipated coefficients. This function should return a Surv object from
#'the survival package.
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients
#'will be automatically generated based on the delta argument.
#'@param delta The signal-to-noise ratio. Default 2. This specifies the difference between the high
#'and low levels. If you do not specify anticoef, the anticipated coefficients will be half of delta
#'@param conservative Default FALSE. Specifies whether default method for generating
#'anticipated coefficents should be conservative or not. TRUE will give the most conservative
#'estimate of power by setting all but one level in a categorical factor's anticipated coefficients
#'to zero.
#'@param parallel Default FALSE. If TRUE, uses all cores available to speed up computation of power.
#'@param ... Any additional arguments to be input into the survreg function during fitting.
#'@return A data frame consisting of the parameters and their powers
#'@import AlgDesign foreach doParallel survival
#'@export
#'@examples #These examples focus on the survival analysis case and assume familiarity
#'#with the basic functionality of eval_design_mc.
#'
#'#We first generate simple 2-level design using expand.grid:
#'basicdesign = expand.grid(a=c(-1,1))
#'design = gen_design(factorial=basicdesign,model=~a,trials=100,
#'                          optimality="D",repeats=100)
#'
#'#We want to evaluate this design with a Monte Carlo approach, taking into account
#'#that some of the points will be censored. In this case, we need
#'#to create a function that generates random numbers based on our run matrix X and
#'#our anticipated coefficients (b), censors the results from those numbers based
#'#on the censoring criteria, and then returns a Surv object from the survival package.
#'#For an exponential distribution, the censored response is generated according to the
#'#following formula:
#'
#'rsurvival = function(X,b) {
#'  Y = rexp(n=nrow(X),rate=exp(-(X %*% b)))
#'  censored = Y > 1
#'  Y[censored] = 1
#'  return(Surv(time=Y,event=!censored,type="right"))
#'}
#'
#'#We can then evaluate the power of the design in the same way as eval_design_mc:
#'
#'eval_design_survival_mc(RunMatrix=design,model=~a,alpha=0.05,nsim=1000,
#'                        distribution="exponential",rfunctionsurv=rsurvival, delta=1)
#'
#'#We can also evaluate different censored distributions by specifying a different
#'#random generating function and changing the distribution argument. You can also
#'#specify any additional arguments at the end of the function call and they will be
#'#input into the survreg function when it evaluates.
#'
#'rlognorm = function(X,b) {
#'  Y = rlnorm(n=nrow(X), meanlog = X %*% b, sdlog = 0.4)
#'  censored = Y > 1.2
#'  Y[censored] = 1.2
#'  return(Surv(time=Y,event=!censored,type="right"))
#'}
#'
#'#The argument "scale" was not specified in eval_design_survival_mc, but it was passed into
#'#the survreg function call.
#'
#'eval_design_survival_mc(RunMatrix=design,model=~a,alpha=0.2,nsim=1000,
#'                        distribution="lognormal",rfunctionsurv=rlognorm,
#'                        anticoef=c(0.184,0.101),delta=2,scale=0.4)
eval_design_survival_mc = function(RunMatrix, model, alpha, nsim, distribution, rfunctionsurv,
                          anticoef, delta=2,
                          conservative=FALSE, parallel=FALSE, ...) {

  if(is.null(attr(RunMatrix,"modelmatrix"))) {
    contrastslist = list()
    for(x in names(RunMatrix[sapply(RunMatrix,class) == "factor"])) {
      contrastslist[x] = "contr.sum"
    }
    if(length(contrastslist) == 0) {
      attr(RunMatrix,"modelmatrix") = model.matrix(model,RunMatrix)
    } else {
      attr(RunMatrix,"modelmatrix") = model.matrix(model,RunMatrix,contrasts.arg=contrastslist)
    }
  }

  if(length(contrastslist) < 1) {
    contrastslist = NULL
  }

  #remove columns from variables not used in the model
  RunMatrixReduced = reduceRunMatrix(RunMatrix,model,contrasts)
  ModelMatrix = model.matrix(model,RunMatrixReduced,contrasts.arg=contrasts)

  # autogenerate anticipated coefficients
  if(missing(anticoef)) {
    anticoef = gen_anticoef(RunMatrixReduced,model,conservative=conservative)
  }
  if(length(anticoef) != dim(ModelMatrix)[2] && any(sapply(RunMatrixReduced,class)=="factor")) {
    stop("Wrong number of anticipated coefficients")
  }
  if(length(anticoef) != dim(ModelMatrix)[2] && !any(sapply(RunMatrixReduced,class)=="factor")) {
    anticoef = rep(1,dim(ModelMatrix)[2])
  }
  nparam = ncol(ModelMatrix)
  RunMatrixReduced$Y = 1
  contrastlist = attr(attr(RunMatrixReduced,"modelmatrix"),"contrasts")

  if(!parallel) {
    power_values = rep(0, ncol(ModelMatrix))
    for (j in 1:nsim) {

      #simulate the data.
      anticoef_adjusted = anticoef*delta/2

      RunMatrixReduced$Y = rfunctionsurv(ModelMatrix,anticoef_adjusted)

      model_formula = update.formula(model, Y ~ .)

      #fit a model to the simulated data.
      fit = survival::survreg(model_formula, data=RunMatrixReduced,dist=distribution,...)

      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = summary(fit)$table[,4]
      for(i in 1:length(pvals)) {
        if (pvals[i] < alpha) {
          power_values[i] = power_values[i] + 1
        }
      }
    }
    power_values = power_values/nsim

    #output the results
    return(data.frame(parameters=colnames(ModelMatrix),type=rep("parameter.power.mc",length(power_values)), power=power_values))
  } else {
    cl <- parallel::makeCluster(parallel::detectCores())
    doParallel::registerDoParallel(cl, cores = parallel::detectCores())

    power_values = foreach::foreach (i = 1:nsim, .combine = "+", .packages=c("survival")) %dopar% {
      power_values = rep(0, ncol(ModelMatrix))
      #simulate the data.

      anticoef_adjusted = anticoef*delta/2

      RunMatrixReduced$Y = rfunctionsurv(ModelMatrix,anticoef_adjusted)

      model_formula = update.formula(model, Y ~ .)

      #fit a model to the simulated data.
      fit = survival::survreg(model_formula, data=RunMatrixReduced,dist=distribution,...)

      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = summary(fit)$table[,4]

      for(j in 1:length(pvals)) {
        if (pvals[j] < alpha) {
          power_values[j] = power_values[j] + 1
        }
      }
      power_values
    }
    parallel::stopCluster(cl)
    power_values = power_values/nsim
    #output the results

    return(data.frame(parameters=colnames(ModelMatrix),type=rep("parameter.power.mc",length(power_values)), power=power_values))
  }
}
globalVariables('i')
