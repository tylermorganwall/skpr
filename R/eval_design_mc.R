#'@title Evaluates power for model matrix with a Monte Carlo simulation
#'
#'@description Evaluates design given a model matrix with a monte carlo simulation and returns
#'a data frame of parameter powers. A parallelized version of this function is \code{\link{eval_design_parmc}}.
#'Currently only works with linear, non-interacting models.
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
#'These functions take each column of the model matrix as an input, and output a vector of values used in generating
#'the random values in rfunction. Examples of these custom functions are provided below.
#'@param skipintercept A boolean factor on whether the intercept term should be skipped in calculation. Default is FALSE.
#'@return A data frame consisting of the parameters and their powers
#'@export
#'@examples factorial = expand.grid(a=c(-1,0,1),b=c(1,2,3,4),c=c(-1,1),d=c(-1,1))
#'designlinear = gen_design(factorial,~a+b+c+d,10,"D",100)
#'
#'eval_design(designlinear,alpha=0.05)
#'
#'factorialmixed = expand.grid(a=as.factor(c(1,2,3,4,5,6)),b=c(1,0,-1),
#'                             c=as.factor(c(1,2,3,4)),d=c(-1,1), stringsAsFactors = TRUE)
#'mixeddesign = gen_design(factorialmixed,~a+b+c+d,18,"D",100)
#'
#'#Evaluate the design with a normal approximation
#'eval_design(mixeddesign,0.05)
#'
#'#Evaluate the design with a Monte Carlo approach. In this case, we are generating
#'#random numbers into a vector with a mean given by that entry in the column of the
#'#model matrix (mean = returncol) and a standard deviation of one (sd = returnone).
#'#We then fit these data with our model in a glm with a gaussian family 1,000 times,
#'#and count how many times it was significant (with a 0.05 cutoff for significance).
#'#The ratio of the number of times that parameter is significant to the total number
#'#of simulations is the power for that parameter.
#'returncol = function(column) return(column)
#'returnone = function(column) return(1)
#'eval_design_mc(mixeddesign,~(a+b+c+d),0.05,100,"gaussian",rnorm,c("mean","sd"),
#'               list(returncol,returnone))
#'#The functions can also be stated as anonymous functions defined in the list:
#'eval_design_mc(mixeddesign,~(a+b+c+d),0.05,100,"gaussian",rnorm,c("mean","sd"),
#'               list(function(column) return(column),function(column) return(1)))

#'
#'#Evaluate the design with a different model than the one that generated the design.
#'eval_design_mc(mixeddesign,~(a+d),0.05,100,"gaussian",rnorm,
#'               c("mean","sd"), list(returncol,returnone))
#'
#'#Evaluate the design with a different signal-to-noise ratio.
#'#Here we halve the signal-to-noise ratio by doubling the
#'#standard deviation of the pool from which random numbers are being drawn.
#'returntwo = function(col) return(2)
#'eval_design_mc(mixeddesign,~(a+b+c+d),0.05,100,"gaussian",rnorm,
#'               c("mean","sd"), list(returncol,returntwo))
#'#This is the same as evaluating the power with a normal
#'#approximation using halved anticipated coefficients
#'eval_design(mixeddesign,0.05,c(1,1,-1,1,-1,1,1,1,-1,1,1)*1/2)
#'
#'#We can also use this method to evaluate designs that cannot be easily
#'#evaluated using normal approximations. Here, we evaluate a design and see
#'#if we can detect the difference between a 70% and 80% probability of an event
#'#occuring with a binomial response.
#'factorialbinom = expand.grid(a=c(-1,1),b=c(-1,1),c=c(1,0,-1))
#'designbinom = gen_design(factorialbinom,~a+b+c,100,"D",100)
#'
#'#This function translates the low and high values of the column from
#'#the model matrix into probabilities (70% for low and 80% for high)
#'returnprob = function(col) return(0.7+0.1*(col - min(col))/(max(col)-min(col)))
#'
#'#Plugging everything in, we now evaluate our model and obtain the power calculations.
#'eval_design_mc(designbinom,~(a+b+c),0.05,100,"binomial",rbinom,c("size","prob"),
#'               list(returnone,returnprob),skipintercept=TRUE)
eval_design_mc = function(RunMatrix, model, alpha, nsim, glmfamily, rfunction,
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

  for (i in start:nparam) {
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
    power_values[i] = nsignificant / nsim
  }

  #output the results
  if(skipintercept) {
    data.frame(parameters=colnames(ModelMatrix)[-1],type=rep("parameter.power.mc",length(power_values[-1])), power=power_values[-1])
  } else {
    data.frame(parameters=colnames(ModelMatrix),type=rep("parameter.power.mc",length(power_values)), power=power_values)
  }
}
