#'@title Evaluates power for model matrix with a parallel Monte Carlo simulation.
#'
#'@description Evaluates design given a model matrix with a parallel Monte Carlo simulation and returns
#'a data frame of parameter powers. Usage is identical to eval_design_mc.
#'See \code{\link{eval_design_mc}} for full documentation and examples on usage.
#'
#'@param ModelMatrix Parameter (column) of the model matrix being evaluated
#'@param model The model used in the evaluation.
#'@param alpha The type-I error
#'@param nsim The number of simulations for the Monte Carlo simulation.
#'@param glmfamily String indicating the family the distribution is from for the glm function (e.g. binomial)
#'@param rfunction Random number generator function
#'@param rfunctionargs List of arguments to be passed to the rfunction function. The n parameter
#'is handled automatically.
#'@param rlistfuncs List of functions corresponding (in order) to the arguments listed in the rfunctionargs argument.
#'These functions take the column of the model matrix as an input, and output a vector of values used in generating
#'the random values in rfunction.
#'@param skipintercept A boolean factor on whether the intercept term should be skipped in calculation. Default is FALSE.
#'@import doParallel parallel foreach
#'@return A data frame consisting of the parameters and their powers
#'@export
#'@examples #See documentation for eval_design_mc. This is just a parallel wrapper around that function.
#'#If large number of designs are being evaluated, it might be advisable to parallelize the outer loop
#'#and use eval_design_mc rather than eval_design_parmc.
eval_design_parmc = function(RunMatrix, model, alpha, nsim, glmfamily, rfunction,
                             rfunctionargs,rlistfuncs,skipintercept=FALSE) {

  if(is.null(attr(RunMatrix,"modelmatrix"))) {
    contrastslist = list()
    for(x in names(RunMatrix[sapply(RunMatrix,class) == "factor"])) {
      contrastslist[x] = "contr.sum"
    }
    if(length(contrastslist) == 0) {
      attr(RunMatrix,"modelmatrix") = model.matrix(model.matrix(model,RunMatrix))
    } else {
      attr(RunMatrix,"modelmatrix") = model.matrix(model.matrix(model,RunMatrix,contrasts.arg=contrastslist))
    }
  }
  #remove columns from variables not used in the model
  RunMatrixReduced = reducemodelmatrix(RunMatrix,model)
  ModelMatrix = attr(RunMatrixReduced,"modelmatrix")

  model_formula = update.formula(model, Y ~ .)
  nparam = ncol(ModelMatrix)
  RunMatrixReduced$Y = 1
  power_values = rep(0, nparam)
  rfunctionargslist = vector("list",length(rlistfuncs)+1)
  names(rfunctionargslist) = c("n",rfunctionargs)
  rfunctionargslist$n = nrow(RunMatrixReduced)
  contrastlist = attr(attr(RunMatrixReduced,"modelmatrix"),"contrasts")
  start = 1

  if (skipintercept) {
    start = 2
  }

  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl, cores = parallel::detectCores())

  power_values = foreach::foreach (i = start:nparam, .combine = c) %dopar% {
    nsignificant = 0  #will hold the number of times we call parameter[i] significant
    for (argument in 1:length(rlistfuncs)) {
      rfunctionargslist[[argument+1]] = rlistfuncs[[argument]](ModelMatrix[,i])
    }
    for (j in 1:nsim) {
      #simulate the data.
      RunMatrixReduced$Y = do.call(rfunction,rfunctionargslist)

      #fit a model to the simulated data.
      fit = glm(model_formula, family=glmfamily, data=RunMatrixReduced,contrasts = contrastlist)

      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = coef(summary.glm(fit))[,4]
      if (pvals[i] < alpha) {
        nsignificant = nsignificant + 1
      }
    }
    #return proportion of significant simulations for each parameter
    nsignificant / nsim
  }
  parallel::stopCluster(cl)

  #output the results
  if(skipintercept) {
    data.frame(parameters=colnames(ModelMatrix)[-1],type=rep("parameter.power.mc",length(power_values)), power=power_values)
  } else {
    data.frame(parameters=colnames(ModelMatrix),type=rep("parameter.power.mc",length(power_values)), power=power_values)
  }
}
globalVariables('i')
