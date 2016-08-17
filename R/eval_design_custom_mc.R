#'@title Evaluates power for model matrix with a Monte Carlo simulation for a user supplied library
#'
#'@description Evaluates design given a model matrix with a monte carlo simulation and returns
#'a data frame of parameter powers. Currently only works with linear, non-interacting models.
#'
#'@param RunMatrix The run matrix of the design.
#'@param model The model used in the evaluation.
#'@param alpha The type-I error.
#'@param nsim The number of simulations.
#'@param rfunction Random number generator function. Should be a function of the form f(X,b), where X is the
#'model matrix and b are the anticipated coefficients.
#'@param fitfunction Function from library used to evaluate fit. Should be of the form f(formula, X, contrasts)
#'where X is the model matrix. If contrasts do not need to be specified for the user supplied
#'library, that argument can be ignored.
#'@param pvalfunction Function that returns a vector of pvals from the object returned from the fitfunction.
#'@param coef_function Function that, when applied to a fitfunction return object, returns the estimated coefficients.
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients will be
#'automatically generated.
#'@param delta The signal-to-noise ratio. Default 2. This specifies the difference between the high and low levels.
#'Anticipated coefficients will be half of this number.
#'@param conservative Default FALSE. Specifies whether default method for generating
#'anticipated coefficents should be conservative or not. TRUE will give the most conservative
#'estimate of power by setting all but one level in a categorical factor's anticipated coefficients
#'to zero.
#'@param parallel Default FALSE. If TRUE, uses all cores available to speed up computation of power.
#'@param parallelpackages A vector of strings listing the external packages to be input into the parallel package.
#'@return A data frame consisting of the parameters and their powers. The parameter estimates from the simulations are
#'stored in the 'estimates' attribute.
#'@import AlgDesign foreach doParallel
#'@export
#'@examples #To demonstrate how a user can use their own libraries for Monte Carlo power generation,
#'#We will recreate eval_design_survival_mc using the eval_design_custom_mc framework.
#'
#'#To begin, first let us generate the same design and random generation function shown in the
#'#eval_design_survival_mc examples:
#'
#'basicdesign = expand.grid(a=c(-1,1))
#'design = gen_design(factorial=basicdesign,model=~a,trials=100,
#'                          optimality="D",repeats=100)
#'
#'#Random number generating function
#'
#'rsurvival = function(X,b) {
#'  Y = rexp(n=nrow(X),rate=exp(-(X %*% b)))
#'  censored = Y > 1
#'  Y[censored] = 1
#'  return(Surv(time=Y,event=!censored,type="right"))
#'}
#'
#'#We now need to tell the package how we want to fit our data,
#'#given the formula and the model matrix X (and, if needed, the list of contrasts).
#'#If the contrasts aren't required, "contrastlist" should be set to NULL.
#'#This should return some type of fit object.
#'
#'fitsurv = function(formula, X, contrastlist=NULL) {
#'  return(survreg(formula, data=X,dist="exponential"))
#'}
#'
#'
#'#We now need to tell the package how to extract the p-values from the fit object returned
#'#from the fit function. This is how to extract the p-values from the survreg fit object:
#'
#'pvalsurv = function(fit) {
#'  return(summary(fit)$table[,4])
#'}
#'
#'#And now we evaluate the design, passing the fitting function and p-value extracting function
#'#in along with the standard inputs for eval_design_mc.
#'
#'d=eval_design_custom_mc(RunMatrix=design,model=~a,alpha=0.05,nsim=100,
#'                      fitfunction=fitsurv, pvalfunction=pvalsurv, rfunction=rsurvival, delta=1)
#'
#'#This has the exact same behavior as eval_design_survival_mc.
#'
#'#-----Cookbook of extractor functions for various libraries-----#
#'
#'#glm:
#'
#'#bbmle (mle2):
#'
#'
#'#fit.f = mle2(Y~,data=RunMatrix)
#'
#'#pvals = coef(bbmle::summary(fit.f))[,4]
#'
#'#aov
#'
#'#fit = aov(y ~ A + B + A:B, data=mydataframe)
eval_design_custom_mc = function(RunMatrix, model, alpha, nsim, rfunction, fitfunction, pvalfunction,
                                 anticoef, delta=2, contrasts = contr.sum,
                                 conservative=FALSE, parallel=FALSE, parallelpackages=NULL) {

  #---------- Generating model matrix ----------#
  #remove columns from variables not used in the model
  RunMatrixReduced = reduceRunMatrix(RunMatrix,model)

  contrastslist = list()
  for(x in names(RunMatrixReduced[lapply(RunMatrixReduced,class) == "factor"])) {
    contrastslist[[x]] = contrasts
  }

  if(length(contrastslist) < 1) {
    contrastslist = NULL
  }

  #---------- Convert dot formula to terms -----#
  if((as.character(model)[2] == ".")) {
    model = as.formula(paste("~",paste(attr(RunMatrixReduced,"names"),collapse=" + "),sep=""))
  }

  ModelMatrix = model.matrix(model,RunMatrixReduced,contrasts.arg=contrastslist)
  #We'll need the parameter and effect names for output
  parameter_names = colnames(ModelMatrix)
  effect_names = c("(Intercept)", attr(terms(model), 'term.labels'))


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

  model_formula = update.formula(model, Y ~ .)
  nparam = ncol(ModelMatrix)
  RunMatrixReduced$Y = 1
  contrastlist = attr(attr(RunMatrixReduced,"modelmatrix"),"contrasts")

  if(!parallel) {
    power_values = rep(0, length(parameter_names))
    estimates = matrix(0, nrow = nsim, ncol = nparam)
    for (j in 1:nsim) {

      #simulate the data.
      RunMatrixReduced$Y = rfunction(ModelMatrix,anticoef*delta/2)

      #fit a model to the simulated data.
      fit = fitfunction(model_formula, RunMatrixReduced, contrastlist)

      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = pvalfunction(fit)
      power_values[pvals < alpha] = power_values[pvals < alpha] + 1
      estimates[j, ] = coef_function(fit)
    }
    power_values = power_values / nsim

  } else {
    cl <- parallel::makeCluster(parallel::detectCores())
    doParallel::registerDoParallel(cl, cores = parallel::detectCores())

    power_estimates = foreach::foreach (i = 1:nsim, .combine = "+",.packages = parallelpackages) %dopar% {
      power_values = rep(0, ncol(ModelMatrix))
      #simulate the data.
      RunMatrixReduced$Y = rfunction(ModelMatrix,anticoef*delta/2)

      #fit a model to the simulated data.
      fit = fitfunction(model_formula, RunMatrixReduced, contrastlist)

      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = pvalfunction(fit)
      power_values[pvals < alpha] = 1
      estimates = coef_function(fit)

      #We are going to output a tidy data.frame with the results, so just append the effect powers
      #to the parameter powers. We'll use another column of that dataframe to label wether it is parameter
      #or effect power.
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
