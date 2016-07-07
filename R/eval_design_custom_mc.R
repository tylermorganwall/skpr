#'@title Evaluates power for model matrix with a Monte Carlo simulation for a user supplied library
#'
#'@description Evaluates design given a model matrix with a monte carlo simulation and returns
#'a data frame of parameter powers. Currently only works with linear, non-interacting models.
#'
#'@param RunMatrix The run matrix of the design.
#'@param model The model used in the evaluation.
#'@param alpha The type-I error.
#'@param nsim The number of simulations.
#'@param fitfunction Function from library used to evaluate fit. Should be of the form f(formula, X, contrasts)
#'where X is the model matrix. If contrasts do not need to be specified for the user supplied
#'library, that argument can be ignored.
#'@param rfunction Random number generator function. Should be a function of the form f(X,b), where X is the
#'model matrix and b are the anticipated coefficients.
#'@param pvalfunction Function that returns a vector of pvals from the object returned from the fitfunction.
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients will be
#'automatically generated.
#'@param delta The signal-to-noise ratio. Default 2. This specifies the difference between the high and low levels.
#'Anticipated coefficients will be half of this number.
#'@param conservative Default FALSE. Specifies whether default method for generating
#'anticipated coefficents should be conservative or not. TRUE will give the most conservative
#'estimate of power by setting all but one level in a categorical factor's anticipated coefficients
#'to zero.
#'@param parallel Default FALSE. If TRUE, uses all cores available to speed up computation of power.
#'@return A data frame consisting of the parameters and their powers
#'@import AlgDesign foreach doParallel
#'@export
#'@examples #We first generate a full factorial design using expand.grid:
eval_design_custom_mc = function(RunMatrix, model, alpha, nsim, fitfunction, rfunction, pvalfunction, anticoef,
                          delta=2, conservative=FALSE, parallel=FALSE) {

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

  #remove columns from variables not used in the model
  RunMatrixReduced = reducemodelmatrix(RunMatrix,model)
  ModelMatrix = attr(RunMatrixReduced,"modelmatrix")

  # autogenerate anticipated coefficients
  if(missing(anticoef)) {
    anticoef = gen_anticoef(RunMatrixReduced,model,conservative=conservative)
  }
  if(length(anticoef) != dim(attr(RunMatrixReduced,"modelmatrix"))[2] && any(sapply(RunMatrixReduced,class)=="factor")) {
    stop("Wrong number of anticipated coefficients")
  }
  if(length(anticoef) != dim(attr(RunMatrixReduced,"modelmatrix"))[2] && !any(sapply(RunMatrixReduced,class)=="factor")) {
    anticoef = rep(1,dim(attr(RunMatrixReduced,"modelmatrix"))[2])
  }

  model_formula = update.formula(model, Y ~ .)
  nparam = ncol(ModelMatrix)
  RunMatrixReduced$Y = 1
  contrastlist = attr(attr(RunMatrixReduced,"modelmatrix"),"contrasts")

  if(!parallel) {
    power_values = rep(0, ncol(ModelMatrix))
    for (j in 1:nsim) {

      #simulate the data.
      RunMatrixReduced$Y = rfunction(ModelMatrix,anticoef*delta/2)

      #fit a model to the simulated data.
      fit = fitfunction(model_formula, RunMatrixReduced, contrastlist)

      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = pvalfunction(fit)
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

    power_values = foreach::foreach (i = 1:nsim, .combine = "+") %dopar% {
      power_values = rep(0, ncol(ModelMatrix))
      #simulate the data.
      RunMatrixReduced$Y = rfunction(ModelMatrix,anticoef*delta/2)

      #fit a model to the simulated data.
      fit = fitfunction(model_formula, RunMatrixReduced, contrastlist)

      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = pvalfunction(fit)
      for(i in 1:length(pvals)) {
        if (pvals[i] < alpha) {
          power_values[i] = power_values[i] + 1
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
