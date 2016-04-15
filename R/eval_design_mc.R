#'@title Evaluates power for model matrix with a Monte Carlo simulation
#'
#'@description Evaluates design given a model matrix with a monte carlo simulation and returns
#'a data frame of parameter powers. A parallelized version of this function is \code{\link{eval_design_parmc}}.
#'
#'@param ModelMatrix Parameter (column) of the model matrix being evaluated
#'@param model The model used in the evaluation.
#'@param alpha The type-I error
#'@param nsim The number of simulations for the Monte Carlo simulation.
#'@param family String indicating the family the distribution is from for the glm function (e.g. binomial)
#'@param rfunction Random number generator function
#'@param rfunctionargs List of arguments to be passed to the rfunction function. The n parameter
#'is handled automatically.
#'@param rlistfuncs List of functions corresponding (in order) to the arguments listed in the rfunctionargs argument.
#'These functions take the column of the model matrix as an input, and output a vector of values used in generating
#'the random values in rfunction. For convenience, three helper functions are provided. returnzero (which returns a
#'vector of zeros of the correct length), returnone (which returns of vector of ones of the correct length), and
#'returncol (which returns the column as is). Examples of the use of these functions and custom functions are provided below.
#'@return A data frame consisting of the parameters and their powers
#'@export
#'@examples factorial = expand.grid(a=c(-1,0,1),b=c(1,2,3,4),c=c(-1,1),d=c(-1,1))
#'designlinear = gen_design(dategc,~a+b+c+d,10,"D",100)
#'designquadratic = gen_design(dategc,~(a+b+c+d)^2,15,"D",100)
#'
#'eval_design(designlinear,alpha=0.05)
#'eval_design(designquadratic,alpha=0.05)
#'
#'factorialmixed = expand.grid(a=as.factor(c(1,2,3,4,5,6)),b=c(1,0,-1),c=as.factor(c(1,2,3,4)),d=c(-1,1), stringsAsFactors = TRUE)
#'mixeddesign = gen_design(factorialmixed,~a+b+c+d,18,"D",100)
#'
#'#Evaluate the design with a normal approximation
#'eval_design(mixeddesign,0.2)
#'
#'#Evaluate the design with a Monte Carlo approach. In this case, we are generating random numbers into a vector
#'#with a mean given by that entry in the column of the model matrix (mean = returncol) and a standard deviation
#'#of one (sd = returnone). We then fit these data with our model in a glm with a gaussian family 1,000 times, and count how many
#'#times it was significant (with a 0.2 cutoff for significance). The ratio of the number of times that parameter
#'#is significant to the total number of simulations is the power for that parameter.
#'eval_design_mc(mixeddesign,~(a+b+c+d),0.2,1000,"gaussian",rnorm,c("mean","sd"), list(returncol,returnone))
#'
#'#We can explicitly write these function as anonymous functions defined in the list:
#'eval_design_mc(mixeddesign,~(a+b+c+d),0.2,1000,"gaussian",rnorm,c("mean","sd"), list(function(col) return(col),function(col) return(1)))
#'
#'#Evaluate the design with a different model than the one that generated the design.
#'eval_design_mc(mixeddesign,~(a+d),0.2,1000,"gaussian",rnorm,c("mean","sd"), list(returncol,returnone))
#'
#'#Evaluate the design with a different signal-to-noise ratio. Here we halve the signal-to-noise ratio by doubling the
#'#standard deviation of the pool from which random numbers are being drawn.
#'returntwo = function(col) return(2)
#'eval_design_mc(mixeddesign,~(a+b+c+d),0.2,1000,"gaussian",rnorm,c("mean","sd"), list(returncol,returntwo))
#'#This is the same as evaluating the power with a normal approximation using halved anticipated coefficients
#'eval_design(mixeddesign,0.2,c(1,1,-1,1,-1,1,1,1,-1,1,1)*1/2)
#'
#'#We can also use this method to evaluate designs that cannot be easily evaluated using normal approximations.
#'#We evaluate here a model that is trying to detect the difference between an event that happens with
#'#70% probability and 80% probability. Taking the null hypothesis as occuring 70% and and the alte
#'factorialbinom = expand.grid(a=c(-1,1),b=c(-1,1),c=c(1,0,-1))
#'designbinom = gen_design(factorialbinom,~a+b+c,100,"D",100)
#'
#'returnprob = function(col) return(0.7+0.1*(col - min(col))/(max(col)-min(col)))
#'eval_design_mc(designbinom,~(a+b+c),0.2,1000,"binomial",rbinom,c("size","prob"), list(returnone,returnprob),skipintercept=TRUE)
eval_design_mc = function(ModelMatrix, model, alpha, nsim, glmfamily, rfunction,
                          rfunctionargs,rlistfuncs,skipintercept=FALSE) {

  #remove columns from variables not used in the model
  ModelMatrix = reducemodelmatrix(ModelMatrix,model)

  # if(length(attr(ModelMatrix,"Design"))!=0) {
  #   RunMatrix = attr(ModelMatrix,"Design")
  # } else {
  #   RunMatrix = data.frame(ModelMatrix[,-1])
  #   colnames(RunMatrix) = names(attr(ModelMatrix,"type"))
  #   attr(ModelMatrix,"Design") = data.frame(ModelMatrix[,-1])
  # }

  RunMatrix = attr(ModelMatrix,"Design")
  RunMatrix$Y = 1
  model_formula = update.formula(model, Y ~ .)
  nparam = ncol(ModelMatrix)
  power_values = rep(0, nparam)
  rfunctionargslist = vector("list",length(rlistfuncs)+1)
  names(rfunctionargslist) = c("n",rfunctionargs)
  rfunctionargslist$n = nrow(RunMatrix)
  contrastlist = attr(ModelMatrix,"contrasts")
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
      RunMatrix$Y = do.call(rfunction,rfunctionargslist)

      #fit a model to the simulated data.
      fit = glm(model_formula, family=glmfamily, data=RunMatrix,contrasts = contrastlist)

      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = coef(summary.glm(fit))[,4]/2
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
