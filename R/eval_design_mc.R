#'@title Evaluates power for model matrix with a Monte Carlo simulation
#'
#'@description Evaluates design given a model matrix with a monte carlo simulation and returns
#'a data frame of parameter powers. Currently only works with linear, non-interacting models.
#'
#'@param RunMatrix The run matrix of the design.
#'@param model The model used in the evaluation.
#'@param alpha The type-I error.
#'@param nsim The number of simulations.
#'@param glmfamily String indicating the family the distribution is from for the glm function
#'(e.g. gaussian, binomial, poisson)
#'@param rfunction Random number generator function. Should be a function of the form f(X,b), where X is the
#'model matrix and b are the anticipated coefficients.
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients
#'will be automatically generated based on the delta argument.
#'@param randomeffects A formula specifying the model for the blocking effects.
#'@param delta The signal-to-noise ratio. Default 2. This specifies the difference between the high
#'and low levels. If you do not specify anticoef, the anticipated coefficients will be half of delta
#'@param varianceratio Default 1. The ratio of the whole plot variance to the run-to-run variance.
#'@param conservative Default FALSE. Specifies whether default method for generating
#'anticipated coefficents should be conservative or not. TRUE will give the most conservative
#'estimate of power by setting all but one level in a categorical factor's anticipated coefficients
#'to zero.
#'@param parallel Default FALSE. If TRUE, uses all cores available to speed up computation of power.
#'@return A data frame consisting of the parameters and their powers
#'@import AlgDesign foreach doParallel
#'@export
#'@examples #We first generate a full factorial design using expand.grid:
#'factorialcoffee = expand.grid(cost=c(-1,1),
#'                              type=as.factor(c("Kona","Colombian","Ethiopian","Sumatra")),
#'                              size=as.factor(c("Short","Grande","Venti")))
#'
#'#And then generate the 21-run D-optimal design using gen_design.
#'
#'designcoffee = gen_design(factorialcoffee,model=~cost + type + size,trials=21,optimality="D")
#'
#'#To evaluate this design using a normal approximation, we just use eval_design
#'#(here using the default settings for contrasts, delta, and the anticipated coefficients):
#'
#'eval_design(RunMatrix=designcoffee,model=~cost + type + size, 0.05)
#'
#'#We want to evaluate this design with a Monte Carlo approach. In this case, we need
#'#to create a function that generates random numbers based on our run matrix X and
#'#our anticipated coefficients (b).
#'
#'rgen = function(X,b) {
#'  return(rnorm(n=nrow(X),mean = X %*% b, sd = 1))
#'}
#'
#'#Here we generate our nrow(X) random numbers from a population with a mean that varies depending
#'#on the design (and is set by multiplying the run matrix X with the anticipated coefficients
#'#vector b), and has a standard deviation of one. To evaluate this, we enter the same information
#'#used in eval_design, with the addition of the number of simulations "nsim", the distribution
#'#family used in fitting for the glm "glmfamily", the custom random generation function "rfunction",
#'#and whether or not we want the computation to be done with all the cores available "parallel".
#'
#'eval_design_mc(RunMatrix=designcoffee,model=~cost + type + size, alpha=0.05,
#'               nsim=100,glmfamily="gaussian",rfunction=rgen)
#'
#'#We see here we generate approximately the same parameter powers as we do
#'#using the normal approximation in eval_design. Like eval_design, we can also change
#'#delta to produce a different signal-to-noise ratio:
#'
#'eval_design_mc(RunMatrix=designcoffee,model=~cost + type + size, alpha=0.05,
#'               nsim=100,glmfamily="gaussian",rfunction=rgen,delta=1)
#'
#'#However, we could also specify this using a different random generator function by
#'#doubling the standard deviation of the population we are drawing from:
#'
#'rgensnr = function(X,b) {
#'  return(rnorm(n=nrow(X),mean = X %*% b, sd = 2))
#'}
#'
#'eval_design_mc(RunMatrix=designcoffee,model=~cost + type + size, alpha=0.05,
#'               nsim=100,glmfamily="gaussian",rfunction=rgensnr)
#'
#'#Both methods provide the same end result.
#'
#'#Like eval_design, we can also evaluate the design with a different model than
#'#the one that generated the design.
#'eval_design_mc(RunMatrix=designcoffee,model=~cost + type, 0.05,
#'               nsim=100,glmfamily="gaussian",rfunction=rgen)
#'
#'#Here we evaluate the design using conservative anticipated coefficients:
#'eval_design_mc(RunMatrix=designcoffee,model=~cost + type + size, 0.05,
#'               nsim=100,glmfamily="gaussian",rfunction=rgen,conservative=TRUE)
#'
#'#And here it is evaluated with higher order effects included:
#'eval_design_mc(RunMatrix=designcoffee,model=~cost + type + size+cost*type, 0.05,
#'               nsim=100,glmfamily="gaussian",rfunction=rgen)
#'
#'#We can also set "parallel=TRUE" to turn use all the cores available to speed up
#'#computation.
#'\dontrun{eval_design_mc(RunMatrix=designcoffee,model=~cost + type + size, 0.05,
#'               nsim=100,glmfamily="gaussian",rfunction=rgen,parallel=TRUE)}
#'
#'#We can also evaluate split-plot designs by specifying the randomeffects argument.
#'
#'blocking = data.frame(Temp = c(1,-1,1,-1,1,-1))
#'
#'#5 runs per block
#'designblocked = gen_design(factorial=designcoffee,model=~cost+ type + size,trials=30,
#'                           wholeblock=blocking, blocksize=5)
#'
#'eval_design_mc(RunMatrix=designblocked, model=~cost+type+size, randomeffects= ~1|Temp,
#'               alpha=0.05, nsim=100, glmfamily="gaussian",rfunction=rgen)
#'
#'#We can also evaluate the design with a custom ratio between the whole plot error to
#'#the run-to-run error.
#'eval_design_mc(RunMatrix=designblocked, model=~cost+type+size, randomeffects= ~1|Temp,
#'               alpha=0.05, nsim=100, glmfamily="gaussian",rfunction=rgen,varianceratio=2)
#'
#'#We can also use this method to evaluate designs that cannot be easily
#'#evaluated using normal approximations. Here, we evaluate a design and see
#'#if we can detect the difference between each factor changing whether an event
#'#70% of the time or 90% of the time.
#'
#'factorialbinom = expand.grid(a=c(-1,1),b=c(-1,1))
#'designbinom = gen_design(factorialbinom,model=~a+b,trials=90,optimality="D",repeats=100)
#'
#'#Here our random binomial generator simulates a response based on the resulting
#'#probability from of all the columns in one row influencing the result.
#'
#'rgenbinom = function(X,b) {
#'  rbinom(n=nrow(X),size=1,prob = exp(X %*% b)/(1+exp(X %*% b)))
#'}
#'
#'#Plugging everything in, we now evaluate our model and obtain the binomial power.
#'#(the anticipated coefficients were determined empircally to set the
#'#high and low probabilities correctly for each factor)
#'
#'eval_design_mc(designbinom,~a+b,alpha=0.2,nsim=100,anticoef=c(1.5,0.7,0.7),
#'               glmfamily="binomial",rfunction=rgenbinom)
#'
#'#We can also use this method to determine power for poisson response variables.
#'#We design our test to detect if each factor changes the base rate of 0.2 by
#'#a factor of 2. We generate the design:
#'
#'factorialpois = expand.grid(a=as.numeric(c(-1,0,1)),b=c(-1,0,1))
#'designpois = gen_design(factorialpois,~a+b,trials=90,optimality="D",repeats=1000)
#'
#'
#'#Here we return a random poisson number of events that vary depending
#'#on the rate in the design.
#'rrate = function(X,b) {
#'  return(rpois(n=nrow(X),lambda=exp(X%*%b)))
#'}
#'eval_design_mc(designpois,~a+b,0.2,nsim=100,glmfamily="poisson",rfunction=rrate,
#'               anticoef=c(log(0.2),log(2),log(2)))
#'#where the anticipated coefficients are chosen to set the base rate at 0.2
#'#(from the intercept) as well as how each factor changes the rate (a factor of 2, so log(2)).
#'#We see here we need about 90 test events to get accurately distinguish the three different
#'#rates in each factor to 90% power.
eval_design_mc = function(RunMatrix, model, alpha, nsim, glmfamily, rfunction, anticoef,
                          randomeffects=NULL, delta=2, varianceratio = 1,
                          conservative=FALSE, parallel=FALSE) {

  contrastslist = list()
  for(x in names(RunMatrix[sapply(RunMatrix,class) == "factor"])) {
    contrastslist[x] = "contr.sum"
  }

  if(length(contrastslist) < 1) {
    contrastslist = NULL
  }

  if(!is.null(randomeffects)) {
    randomvars = all.vars(randomeffects)
    mmrandomcols = suppressWarnings(ncol(model.matrix(reformulate(termlabels = randomvars),RunMatrix,contrasts.arg=contrastslist))-1)
    allvariablemodel = reformulate(termlabels = c(randomvars,attr(terms(model),"term.labels")))
  } else {
    allvariablemodel = model
  }

  if(!is.null(randomeffects)) {
    BlockDesign = data.frame()
    block = floor(as.numeric(rownames(RunMatrix)))
    blocknumbers = unique(block)
    indices = c()
    for(blockstarts in blocknumbers) {
      indices = c(indices, match(blockstarts,block))
    }
    for(ind in indices) {
      BlockDesign = rbind(BlockDesign,RunMatrix[ind,])
    }
    names(BlockDesign) = names(RunMatrix)
    attr(BlockDesign,"modelmatrix") = model.matrix(model,BlockDesign,contrasts.arg=contrastslist)
  }

  if(!is.null(randomeffects)) {
    BlockedRunMatrix = reducemodelmatrix(BlockDesign, reformulate(termlabels = randomvars))
    if(any(lapply(BlockedRunMatrix,class) == "factor")) {
      blockedcontrastslist = list()
      for(x in names(BlockedRunMatrix[sapply(BlockedRunMatrix,class) == "factor"])) {
        blockedcontrastslist[x] = "contr.sum"
      }
      attr(BlockedRunMatrix,"modelmatrix") = model.matrix(reformulate(termlabels = randomvars),BlockedRunMatrix,contrasts.arg=blockedcontrastslist)
    } else {
      blockedcontrastslist = NULL
      attr(BlockedRunMatrix,"modelmatrix") = model.matrix(reformulate(termlabels = randomvars),BlockedRunMatrix)
    }
    blockedanticoef = gen_anticoef(BlockedRunMatrix,reformulate(termlabels = randomvars),conservative=conservative)
    model_formula_blocked = update.formula(reformulate(termlabels = randomvars), Y ~.)
    BlockedRunMatrix$Y = 1
  }

  attr(RunMatrix,"modelmatrix") = model.matrix(allvariablemodel,RunMatrix,contrasts.arg=contrastslist)

  #remove columns from variables not used in the model
  RunMatrixReduced = reducemodelmatrix(RunMatrix,allvariablemodel)
  ModelMatrix = attr(RunMatrixReduced,"modelmatrix")

    # autogenerate anticipated coefficients
  if(missing(anticoef)) {
    anticoef = gen_anticoef(RunMatrixReduced, allvariablemodel, conservative=conservative)
  }
  if(length(anticoef) != dim(attr(RunMatrixReduced,"modelmatrix"))[2] && any(sapply(RunMatrixReduced,class)=="factor")) {
    stop("Wrong number of anticipated coefficients")
  }
  if(length(anticoef) != dim(attr(RunMatrixReduced,"modelmatrix"))[2] && !any(sapply(RunMatrixReduced,class)=="factor")) {
    anticoef = rep(1,dim(attr(RunMatrixReduced,"modelmatrix"))[2])
  }

  model_formula = update.formula(model, Y ~ .)
  RunMatrixReduced$Y = 1
  contrastlist = attr(attr(RunMatrixReduced,"modelmatrix"),"contrasts")

    if(!parallel) {
    power_values = rep(0, ncol(ModelMatrix))

    for (j in 1:nsim) {

      #simulate the data.
      RunMatrixReduced$Y = rfunction(ModelMatrix,anticoef*delta/2)
      if(!is.null(randomeffects)) {
        BlockedRunMatrix$Y = rfunction(attr(BlockedRunMatrix,"modelmatrix"),
                                       blockedanticoef*delta/sqrt(varianceratio+nrow(BlockedRunMatrix)/nrow(RunMatrixReduced))*1/2)
      }

      if(!is.null(randomeffects)) {
        if(glmfamily == "gaussian") {
          fit = nlme::lme(fixed=model_formula, random=randomeffects, data=RunMatrixReduced, contrasts = contrastlist)
          fitblock = lm(model_formula_blocked, data=BlockedRunMatrix, contrasts = blockedcontrastslist)
        } else {
          fit = lme4::glmer(model_formula, family=glmfamily, data=RunMatrixReduced,contrasts = contrastlist)
          fitblock = lme4::glmer(model_formula_blocked, family=glmfamily, data=BlockedRunMatrix,contrasts = blockedcontrastslist)
        }
      } else {
        fit = glm(model_formula, family=glmfamily, data=RunMatrixReduced,contrasts = contrastlist)
      }

      if(!is.null(randomeffects)) {
        coefs <- data.frame(coef(summary(fit)))
        if(glmfamily == "gaussian") {
          blockedcoefs = data.frame(coef(summary(fitblock)))
        } else {
          blockedcoefs = coef(summary.glm(fitblock))[,4]
        }
        #if any NaN in blocked, set all to 1-alpha
        if(any(is.nan(blockedcoefs[,4]))) {
          blockedcoefs[,4] = rep(1,length(blockedcoefs[,4]))
        }

        pvals = c(coefs[-1,5],blockedcoefs[,4])

        #determine whether beta[i] is significant. If so, increment nsignificant
        for(i in 1:length(pvals)) {
          if (pvals[i] < alpha) {
            power_values[i] = power_values[i] + 1
          }
        }

        if(j == 1) {
          names = c(rownames(coefs)[-1],rownames(blockedcoefs))
        }
      } else {
        #determine whether beta[i] is significant. If so, increment nsignificant
        pvals = coef(summary.glm(fit))[,4]
        for(i in 1:length(pvals)) {
          if (pvals[i] < alpha) {
            power_values[i] = power_values[i] + 1
          }
        }
      }
    }
    power_values = power_values/nsim

    #output the results
    if(is.null(randomeffects)) {
      return(data.frame(parameters=colnames(ModelMatrix),type=rep("parameter.power.mc",length(power_values)), power=power_values))
    } else {
      return(data.frame(parameters=names,type=rep("parameter.power.mc",length(power_values)), power=power_values))
    }
  } else {
    cl <- parallel::makeCluster(parallel::detectCores())
    doParallel::registerDoParallel(cl, cores = parallel::detectCores())

    power_values = foreach::foreach (j = 1:nsim, .combine = "+") %dopar% {
      power_values = rep(0, ncol(ModelMatrix))
      #simulate the data.
      RunMatrixReduced$Y = rfunction(ModelMatrix,anticoef*delta/2)
      if(!is.null(randomeffects)) {
        BlockedRunMatrix$Y = rfunction(attr(BlockedRunMatrix,"modelmatrix"),
                                       blockedanticoef*delta/sqrt(varianceratio+nrow(BlockedRunMatrix)/nrow(RunMatrixReduced))*1/2)
      }

      if(!is.null(randomeffects)) {
        if(glmfamily == "gaussian") {
          fit = nlme::lme(fixed=model_formula, random=randomeffects, data=RunMatrixReduced, contrasts = contrastlist)
          fitblock = lm(model_formula_blocked, data=BlockedRunMatrix, contrasts = blockedcontrastslist)
        } else {
          fit = lme4::glmer(model_formula, family=glmfamily, data=RunMatrixReduced,contrasts = contrastlist)
          fitblock = lme4::glmer(model_formula_blocked, family=glmfamily, data=BlockedRunMatrix,contrasts = blockedcontrastslist)
        }
      } else {
        fit = glm(model_formula, family=glmfamily, data=RunMatrixReduced,contrasts = contrastlist)
      }

      if(!is.null(randomeffects)) {
        coefs <- data.frame(coef(summary(fit)))
        if(glmfamily == "gaussian") {
          blockedcoefs = data.frame(coef(summary(fitblock)))
        } else {
          blockedcoefs = coef(summary.glm(fitblock))[,4]
        }
        #if any NaN in blocked, set all to 1-alpha
        if(any(is.nan(blockedcoefs[,4]))) {
          blockedcoefs[,4] = rep(1,length(blockedcoefs[,4]))
        }

        pvals = c(coefs[-1,5],blockedcoefs[,4])

        #determine whether beta[i] is significant. If so, increment nsignificant
        for(i in 1:length(pvals)) {
          if (pvals[i] < alpha) {
            power_values[i] = power_values[i] + 1
          }
        }

        if(j == 1) {
          names = c(rownames(coefs)[-1],rownames(blockedcoefs))
        }
      } else {
        #determine whether beta[i] is significant. If so, increment nsignificant
        pvals = coef(summary.glm(fit))[,4]
        for(i in 1:length(pvals)) {
          if (pvals[i] < alpha) {
            power_values[i] = power_values[i] + 1
          }
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
