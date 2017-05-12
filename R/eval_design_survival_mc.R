#'@title Evaluate Power for Survival Design
#'
#'@description Evaluates power for a right censored survival design with a Monte Carlo simulation,
#'using the survival package and survreg to fit the data.
#'
#'@param RunMatrix The run matrix of the design.
#'@param model The model used in the evaluation.
#'@param alpha The type-I error.
#'@param nsim Default 1000. The number of simulations.
#'@param distribution Default "gaussian". Distribution of survival function.
#'@param censorpoint Default NA for no censoring. The point after/before (for right censored or left censored data, respectively)
#'which data should be labelled as censored.
#'@param censortype Default "right". The type of censoring (either "left" or "right")
#'@param rfunctionsurv Default NULL. Random number generator function. Should be a function of the form f(X,b), where X is the
#'model matrix and b are the anticipated coefficients. This function should return a Surv object from
#'the survival package. This is available if the user wants to add their own distribution not interally supported
#'or modify the existing random generation functions.
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients
#'will be automatically generated based on the delta argument.
#'@param delta The signal-to-noise ratio. Default 2. This specifies the difference between the high
#'and low levels. If you do not specify anticoef, the anticipated coefficients will be half of delta
#'@param contrasts Function used to generate the contrasts encoding for categorical variables. Default contr.sum.
#'@param parallel Default FALSE. If TRUE, uses all cores available to speed up computation of power.
#'@param ... Any additional arguments to be input into the survreg function during fitting.
#'@return A data frame consisting of the parameters and their powers. The parameter estimates from the simulations are
#'stored in the 'estimates' attribute.
#'@import foreach doParallel survival stats
#'@export
#'@examples #These examples focus on the survival analysis case and assume familiarity
#'#with the basic functionality of eval_design_mc.
#'
#'#We first generate simple 2-level design using expand.grid:
#'basicdesign = expand.grid(a=c(-1, 1))
#'design = gen_design(candidateset=basicdesign, model=~a, trials=15)
#'
#'#We can then evaluate the power of the design in the same way as eval_design_mc,
#'#now including the type of censoring (either right or left) and the point at which
#'#the data should be censored:
#'
#'eval_design_survival_mc(RunMatrix=design, model=~a, alpha=0.05,
#'                        nsim=100, distribution="exponential",
#'                        censorpoint=5,censortype="right")
#'
#'#Built-in Monte Carlo random generating functions are included for the gaussian, exponential,
#'#and lognormal distributions.
#'
#'#We can also evaluate different censored distributions by specifying a custom
#'#random generating function and changing the distribution argument. You can also
#'#specify any additional arguments at the end of the function call and they will be
#'#input into the survreg function when it evaluates.
#'
#'rlognorm = function(X, b) {
#'  Y = rlnorm(n=nrow(X), meanlog = X %*% b, sdlog = 0.4)
#'  censored = Y > 1.2
#'  Y[censored] = 1.2
#'  return(Surv(time=Y, event=!censored, type="right"))
#'}
#'
#'#Any additional arguments are passed into the survreg function call.  As an example, you
#'#might want to fix the "scale" argument to survreg, when fitting a lognormal:
#'
#'eval_design_survival_mc(RunMatrix=design, model=~a, alpha=0.2, nsim=100,
#'                        distribution="lognormal", rfunctionsurv=rlognorm,
#'                        anticoef=c(0.184,0.101), delta=2, scale=0.4)
eval_design_survival_mc = function(RunMatrix, model, alpha,
                                   nsim=1000, distribution="gaussian", censorpoint=NA, censortype="right",
                                   rfunctionsurv=NULL, anticoef=NULL, delta=2, contrasts = contr.sum,
                                   parallel=FALSE, ...) {

  #Generating random generation function for survival. If no censorpoint specified, return all uncensored.
  if(is.na(censorpoint)) {
    censorfunction = function(data, point) {rep(FALSE,length(data))}
  }
  if(censortype == "left" && !is.na(censorpoint)) {
    censorfunction = function(data, point) {data<point}
  }
  if(censortype == "right" && !is.na(censorpoint)) {
    censorfunction = function(data, point) {data>point}
  }

  if(is.null(rfunctionsurv)) {
    if(distribution == "exponential") {
      rfunctionsurv = function(X, b) {
        Y = rexp(n=nrow(X), rate=exp(-(X %*% b)))
        Y[censorfunction(Y,censorpoint)] = censorpoint
        return(Surv(time=Y, event=!censorfunction(Y,censorpoint), type=censortype))
      }
    }
    if(distribution == "lognormal") {
      rfunctionsurv = function(X, b) {
        Y = rlnorm(n=nrow(X), meanlog = X %*% b, sdlog = 1)
        Y[censorfunction(Y,censorpoint)] = censorpoint
        return(Surv(time=Y, event=!censorfunction(Y,censorpoint), type=censortype))
      }
    }
    if(distribution == "gaussian") {
      rfunctionsurv = function(X, b) {
        Y = rnorm(n=nrow(X), mean=X %*% b, sd=1)
        Y[censorfunction(Y,censorpoint)] = censorpoint
        return(Surv(time=Y, event=!censorfunction(Y,censorpoint), type=censortype))
      }
    }
  }

  #------Normalize/Center numeric columns ------#
  for(column in 1:ncol(RunMatrix)) {
    if(class(RunMatrix[,column]) == "numeric") {
      midvalue = mean(c(max(RunMatrix[,column]),min(RunMatrix[,column])))
      RunMatrix[,column] = (RunMatrix[,column]-midvalue)/(max(RunMatrix[,column])-midvalue)
    }
  }

  #---------- Generating model matrix ----------#
  #remove columns from variables not used in the model
  RunMatrixReduced = reduceRunMatrix(RunMatrix,model)

  contrastslist = list()
  for(x in names(RunMatrix[lapply(RunMatrixReduced, class) == "factor"])) {
    contrastslist[[x]] = contrasts
  }
  if(length(contrastslist) < 1) {
    contrastslist = NULL
  }

  #---------- Convert dot formula to terms -----#
  if((as.character(model)[2] == ".")) {
    model = as.formula(paste("~", paste(attr(RunMatrixReduced, "names"), collapse=" + "), sep=""))
  }

  ModelMatrix = model.matrix(model,RunMatrixReduced,contrasts.arg=contrastslist)
  #We'll need the parameter and effect names for output
  parameter_names = colnames(ModelMatrix)
  effect_names = c("(Intercept)", attr(terms(model), 'term.labels'))

  # autogenerate anticipated coefficients
  if(missing(anticoef)) {
    anticoef = gen_anticoef(RunMatrixReduced,model)
  }
  if(length(anticoef) != dim(ModelMatrix)[2] && any(sapply(RunMatrixReduced,class)=="factor")) {
    stop("Wrong number of anticipated coefficients")
  }
  if(length(anticoef) != dim(ModelMatrix)[2] && !any(sapply(RunMatrixReduced,class)=="factor")) {
    anticoef = rep(1,dim(ModelMatrix)[2])
  }
  nparam = ncol(ModelMatrix)
  RunMatrixReduced$Y = 1

  if(!parallel) {
    power_values = rep(0, ncol(ModelMatrix))
    estimates = matrix(0, nrow = nsim, ncol = nparam)
    for (j in 1:nsim) {

      #simulate the data.
      anticoef_adjusted = anticoef*delta/2

      RunMatrixReduced$Y = rfunctionsurv(ModelMatrix,anticoef_adjusted)

      model_formula = update.formula(model, Y ~ .)

      #fit a model to the simulated data.
      fit = survival::survreg(model_formula, data=RunMatrixReduced,dist=distribution, ...)

      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = extractPvalues(fit)[1:ncol(ModelMatrix)]
      power_values[pvals < alpha] = power_values[pvals < alpha] + 1
      estimates[j, ] = coef(fit)
    }
    power_values = power_values / nsim

  } else {
    cl <- parallel::makeCluster(parallel::detectCores())
    doParallel::registerDoParallel(cl, cores = parallel::detectCores())

    power_estimates = foreach::foreach (i = 1:nsim, .combine = "rbind", .packages=c("survival")) %dopar% {
      power_values = rep(0, ncol(ModelMatrix))
      #simulate the data.

      anticoef_adjusted = anticoef*delta/2

      RunMatrixReduced$Y = rfunctionsurv(ModelMatrix,anticoef_adjusted)

      model_formula = update.formula(model, Y ~ .)

      #fit a model to the simulated data.
      fit = survival::survreg(model_formula, data=RunMatrixReduced,dist=distribution, ...)

      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = extractPvalues(fit)[1:ncol(ModelMatrix)]
      power_values[pvals < alpha] = 1
      estimates = coef(fit)
      c(power_values, estimates)
    }
    parallel::stopCluster(cl)
    power_values = apply(power_estimates[, 1:nparam], 2, sum) / nsim
    estimates = power_estimates[, (nparam + 1):ncol(power_estimates)]
  }
  #output the results (tidy data format)
  retval = data.frame(parameters=parameter_names,
                      type="parameter.power.mc",
                      power=power_values)
  colnames(estimates) = parameter_names
  attr(retval, 'estimates') = estimates
  retval

}
globalVariables('i')
