#'@title Evaluates power for model matrix with a Monte Carlo simulation
#'
#'@description Evaluates design, given a run matrix, with a monte carlo simulation and returns
#'a data frame of parameter and effect powers.
#'
#'
#'@param RunMatrix The run matrix of the design.
#'@param model The model used in the evaluation.
#'@param alpha The type-I error.
#'@param nsim The number of simulations.
#'@param glmfamily String indicating the family of distribution for the glm function
#'(e.g. "gaussian", "binomial", "poisson")
#'@param rfunction Random number generator function for the response variable. Should be a function of the form f(X,b), where X is the
#'model matrix and b are the anticipated coefficients.
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients
#'will be automatically generated based on the delta argument.
#'
#'@param blockfuntion Random number generator for the noise due to blocks. See examples for details.
#'
#'@param blocknoise Vector of noise levels for each block, one element per blocking level. See examples for details.
#'@param delta The signal-to-noise ratio. Default 2. This specifies the difference between the high
#'and low levels. If you do not specify anticoef, the anticipated coefficients will be half of delta.
#'@param conservative Default FALSE. Specifies whether default method for generating
#'anticipated coefficents should be conservative or not. TRUE will give the most conservative
#'estimate of power by setting all but one level in a categorical factor's anticipated coefficients
#'to zero.
#'@param contrasts The contrasts to use for categorical factors. Defaults to contr.sum.
#'@param parallel Default FALSE. If TRUE, uses all cores available to speed up computation.
#'@return A data frame consisting of the parameters and their powers. The parameter estimates from the simulations are
#'stored in the 'estimates' attribute.
#'@import foreach doParallel
#'@export
#'@examples #We first generate a full factorial design using expand.grid:
#'factorialcoffee = expand.grid(cost=c(-1, 1),
#'                              type=as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
#'                              size=as.factor(c("Short", "Grande", "Venti")))
#'
#'#And then generate the 21-run D-optimal design using gen_design.
#'
#'designcoffee = gen_design(factorialcoffee, model=~cost + type + size, trials=21, optimality="D")
#'
#'#To evaluate this design using a normal approximation, we just use eval_design
#'#(here using the default settings for contrasts, delta, and the anticipated coefficients):
#'
#'eval_design(RunMatrix=designcoffee, model=~cost + type + size, 0.05)
#'
#'#We want to evaluate this design with a Monte Carlo approach. In this case, we need
#'#to create a function that generates random numbers based on our run matrix X and
#'#our anticipated coefficients (b).
#'
#'rgen = function(X,b) {
#'  return(rnorm(n=nrow(X), mean = X %*% b, sd = 1))
#'}
#'
#'#Here we generate our nrow(X) random numbers from a population with a mean that varies depending
#'#on the design (and is set by multiplying the run matrix X with the anticipated coefficients
#'#vector b), and has a standard deviation of one. To evaluate this, we enter the same information
#'#used in eval_design, with the addition of the number of simulations "nsim", the distribution
#'#family used in fitting for the glm "glmfamily", the custom random generation function "rfunction",
#'#and whether or not we want the computation to be done with all the cores available "parallel".
#'
#'eval_design_mc(RunMatrix=designcoffee, model=~cost + type + size, alpha=0.05,
#'               nsim=100, glmfamily="gaussian", rfunction=rgen)
#'
#'#We see here we generate approximately the same parameter powers as we do
#'#using the normal approximation in eval_design. Like eval_design, we can also change
#'#delta to produce a different signal-to-noise ratio:
#'
#'eval_design_mc(RunMatrix=designcoffee, model=~cost + type + size, alpha=0.05,
#'               nsim=100, glmfamily="gaussian", rfunction=rgen, delta=1)
#'
#'#However, we could also specify this using a different random generator function by
#'#doubling the standard deviation of the population we are drawing from:
#'
#'rgensnr = function(X,b) {
#'  return(rnorm(n=nrow(X), mean = X %*% b, sd = 2))
#'}
#'
#'eval_design_mc(RunMatrix=designcoffee, model=~cost + type + size, alpha=0.05,
#'               nsim=100, glmfamily="gaussian", rfunction=rgensnr)
#'
#'#Both methods provide the same end result.
#'
#'#Like eval_design, we can also evaluate the design with a different model than
#'#the one that generated the design.
#'eval_design_mc(RunMatrix=designcoffee, model=~cost + type, alpha=0.05,
#'               nsim=100, glmfamily="gaussian", rfunction=rgen)
#'
#'#Here we evaluate the design using conservative anticipated coefficients:
#'eval_design_mc(RunMatrix=designcoffee, model=~cost + type + size, 0.05,
#'               nsim=100, glmfamily="gaussian", rfunction=rgen, conservative=TRUE)
#'
#'#And here it is evaluated with higher order effects included:
#'eval_design_mc(RunMatrix=designcoffee,model=~cost + type + size + cost*type, 0.05,
#'               nsim=100,glmfamily="gaussian",rfunction=rgen)
#'
#'#We can also set "parallel=TRUE" to turn use all the cores available to speed up
#'#computation.
#'\dontrun{eval_design_mc(RunMatrix=designcoffee,model=~cost + type + size, 0.05,
#'               nsim=100,glmfamily="gaussian",rfunction=rgen,parallel=TRUE)}
#'
#'#We can also evaluate split-plot designs. First, let us generate the split-plot design:
#'
#'vhtc = expand.grid(Store=as.factor(c("A","B")))
#'htc = expand.grid(Temp = c(1,-1))
#'
#'vhtcdesign = gen_design(factorial=vhtc, model=~Store, trials=6)
#'htcdesign = gen_design(factorial=htc, model=~Temp, trials=18, splitplotdesign=vhtcdesign, splitplotsizes=rep(3,6))
#'splitplotdesign = gen_design(factorial=factorialcoffee, model=~cost+type+size+size*Store, trials=54,
#'                             splitplotdesign=htcdesign, splitplotsizes=rep(3,18))
#'
#'#Each block has an additional noise term associated with it in addition to the normal error term.
#'#This is specified by an additional random generating function and a vector specifying the input for
#'#each split-plot level. This function is stating that there is an addition gaussian noise term with each
#'#block, and the vector is stating it has a standard deviation of one for each level. This is equivalent to
#'#a variance ratio of one between the whole plots and the sub-plots.
#'#See the accompanying paper _____ for further technical details.
#'
#'rgenblocking = function(v) {
#'  return(rnorm(n=1, mean = 0, sd = v))
#'}
#'
#'blockvector = c(1,1)
#'
#'#Evaluate the design. Note the decreased power for the blocking factors. If
#'eval_design_mc(RunMatrix=splitplotdesign, model=~Store+Temp+cost+type+size, alpha=0.05,
#'               nsim=100, glmfamily="gaussian", rfunction=rgen,blockfunction = rgenblocking,
#'               blocknoise = blockvector)
#'
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
eval_design_mc = function(RunMatrix, model, alpha, nsim, glmfamily, rfunction,
                          blockfunction=NULL, blocknoise = NULL, anticoef, delta=2,
                          conservative=FALSE, contrasts=contr.simplex, parallel=FALSE) {

  #---------- Generating model matrix ----------#
  #Remove columns from variables not used in the model
  RunMatrixReduced = reduceRunMatrix(RunMatrix,model)

  contrastslist = list()
  for(x in names(RunMatrixReduced[lapply(RunMatrixReduced, class) == "factor"])) {
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

  #-----Autogenerate Anticipated Coefficients---#
  if(missing(anticoef)) {
    anticoef = gen_anticoef(RunMatrixReduced, model, conservative=conservative)
  }
  if(length(anticoef) != dim(ModelMatrix)[2] && any(lapply(RunMatrixReduced,class)=="factor")) {
    stop("Wrong number of anticipated coefficients")
  }
  if(length(anticoef) != dim(ModelMatrix)[2] && !any(lapply(RunMatrixReduced,class)=="factor")) {
    anticoef = rep(1,dim(ModelMatrix)[2])
  }

  #------------ Generate Responses -------------#

  responses = replicate(nsim, rfunction(ModelMatrix,anticoef*delta/2))

  #-------------- Blocking errors --------------#
  blocking = FALSE
  blocknames = rownames(RunMatrix)
  blocklist = strsplit(blocknames,".",fixed=TRUE)

  if(any(lapply(blocklist,length) > 1)) {
    if(is.null(blocknoise)) {
      stop("Error: blocknoise argument missing. If blocking present, need blocknoise vector to generate extra block variance")
    }
    blocking = TRUE
    blockstructure = do.call(rbind,blocklist)
    blockgroups = apply(blockstructure,2,blockingstructure)
    listblocknoise = list()
  }

  if(!is.null(blocknoise)) {
    for(i in 1:(length(blockgroups)-1)) {
      blocktemp = list()
      for(j in 1:length(blockgroups[[i]])) {
        row = replicate(nsim,blockfunction(blocknoise[i]))
        blocktemp[[j]] = do.call(rbind, replicate(blockgroups[[i]][j],row,simplify = FALSE))
      }
      listblocknoise[[i]] = do.call(rbind,blocktemp)
    }
  }

  if(!is.null(blocknoise)) {
    totalblocknoise = Reduce("+",listblocknoise)
  }

  #-------Update formula with random blocks------#

  genBlockIndicators = function(blockgroup) {return(rep(1:length(blockgroup),blockgroup))}

  if(!is.null(blocknoise)) {
    blockindicators = lapply(blockgroups,genBlockIndicators)
    responses = responses + totalblocknoise
    blocknumber = length(blockgroups)-1
    randomeffects = c()
    for(i in 1:(length(blockgroups)-1)) {
      RunMatrixReduced[paste("Block",i,sep="")] = blockindicators[[i]]
      randomeffects = c(randomeffects, paste("( 1 | Block",i, " )", sep=""))
    }
    randomeffects = paste(randomeffects, collapse=" + ")
    blockform = paste("~. + ", randomeffects, sep="")
    #Adding random block variables to formula
    model = update.formula(model, blockform)
  }

  model_formula = update.formula(model, Y ~ .)
  RunMatrixReduced$Y = 1

  #---------------- Run Simulations ---------------#
  if(!parallel) {
    power_values = rep(0, ncol(ModelMatrix))
    for (j in 1:nsim) {

      #simulate the data.
      RunMatrixReduced$Y = responses[,j]
      if (blocking) {
        if(glmfamily == "gaussian") {
          fit = lme4::lmer(model_formula, data=RunMatrixReduced, contrasts = contrastslist)
        } else {
          fit = lme4::glmer(model_formula, data=RunMatrixReduced, family=glmfamily, contrasts = contrastslist)
        }
      } else {
        if (glmfamily == "gaussian") {
          fit = lm(model_formula, data=RunMatrixReduced, contrasts = contrastslist)
        } else {
          fit = glm(model_formula, family=glmfamily, data=RunMatrixReduced, contrasts = contrastslist)
        }
      }
      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = extractPvalues(fit)
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

    power_values = foreach::foreach (j = 1:nsim, .combine = "+", .packages = c("lme4")) %dopar% {
      power_values = rep(0, ncol(ModelMatrix))
      #simulate the data.
      RunMatrixReduced$Y = responses[,j]
      if (blocking) {
        if(glmfamily == "gaussian") {
          fit = lme4::lmer(model_formula, data=RunMatrixReduced, contrasts = contrastslist)
        } else {
          fit = lme4::glmer(model_formula, data=RunMatrixReduced, family=glmfamily, contrasts = contrastslist)
        }
      } else {
        if (glmfamily == "gaussian") {
          fit = lm(model_formula, data=RunMatrixReduced, contrasts = contrastslist)
        } else {
          fit = glm(model_formula, family=glmfamily, data=RunMatrixReduced,contrasts = contrastslist)
        }
      }
      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = extractPvalues(fit)

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
