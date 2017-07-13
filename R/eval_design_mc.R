#'@title Evaluates power for model matrix with a Monte Carlo simulation
#'
#'@description Evaluates design, given a run matrix, with a monte carlo simulation and returns
#'a data frame of parameter and effect powers.
#'
#'@param RunMatrix The run matrix of the design.
#'@param model The model used in the evaluation.
#'@param alpha The type-I error.
#'@param blocking Default FALSE. Set to TRUE if design has blocking structure.
#'@param nsim The number of simulations.
#'@param glmfamily String indicating the family of distribution for the glm function
#'(e.g. "gaussian", "binomial", "poisson")
#'@param rfunction Random number generator function for the response variable. Should be a function of the form f(X, b, delta), where X is the
#'model matrix, b are the anticipated coefficients, and delta is a vector of blocking errors. typically something like rnorm(nrow(X), X * b + delta, 0)
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients
#'will be automatically generated based on the delta argument.
#'@param varianceratios Default 1. The ratio of the whole plot variance to the run-to-run variance. For designs with more than one subplot
#'this ratio can be a vector specifying the variance ratio for each subplot. Otherwise, it will use a single value for all strata.
#'@param delta The signal-to-noise ratio. Default 2. This specifies the difference between the high
#'and low levels. If you do not specify anticoef, the anticipated coefficients will be half of delta.
#'@param contrasts The contrasts to use for categorical factors. Defaults to contr.sum.
#'@param binomialprobs Default NULL. If the glm family is binomial, user should specify a length-two vector consisting of the base probability and the maximum expected probability given all the level settings in the experiment. As an example, if the user wants to detect at an increase in successes from 0.5 to 0.8, the user would pass the vector c(0.5,0.8) to the argument
#'@param parallel Default FALSE. If TRUE, uses all cores available to speed up computation. WARNING: This can slow down computation if nonparallel time to complete the computation is less than a few seconds.
#'@param detailedoutput Default FALSE. Returns additional information about evaluation in results.
#'@return A data frame consisting of the parameters and their powers. The parameter estimates from the simulations are stored in the 'estimates' attribute.
#'@import foreach doParallel nlme stats
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
#'#To evaluate this design with a Monte Carlo method, we enter the same information
#'#used in eval_design, with the addition of the number of simulations "nsim" and the distribution
#'#family used in fitting for the glm "glmfamily". For gaussian, binomial, expontial, and poisson
#'#families, a default random generating function (rfunction) will be supplied. If another glm
#'#family is used or the default random generating function is not adequate, a custom generating
#'#function can be supplied by the user.
#'
#'eval_design_mc(RunMatrix=designcoffee, model=~cost + type + size, alpha=0.05, nsim=100,
#'               glmfamily="gaussian")
#'
#'#We see here we generate approximately the same parameter powers as we do
#'#using the normal approximation in eval_design. Like eval_design, we can also change
#'#delta to produce a different signal-to-noise ratio:
#'
#'eval_design_mc(RunMatrix=designcoffee, model=~cost + type + size, alpha=0.05,
#'               nsim=100, glmfamily="gaussian", delta=1)
#'
#'#Like eval_design, we can also evaluate the design with a different model than
#'#the one that generated the design.
#'eval_design_mc(RunMatrix=designcoffee, model=~cost + type, alpha=0.05,
#'               nsim=100, glmfamily="gaussian")
#'
#'
#'#And here it is evaluated with interactions included:
#'eval_design_mc(RunMatrix=designcoffee,model=~cost + type + size + cost*type, 0.05,
#'               nsim=100, glmfamily="gaussian")
#'
#'#We can also set "parallel=TRUE" to turn use all the cores available to speed up
#'#computation.
#'\dontrun{eval_design_mc(RunMatrix=designcoffee, model=~cost + type + size, 0.05,
#'               nsim=10000, glmfamily="gaussian", parallel=TRUE)}
#'
#'#We can also evaluate split-plot designs. First, let us generate the split-plot design:
#'
#'vhtc = expand.grid(Store=as.factor(c("A","B")))
#'htc = expand.grid(Temp = c(1,-1))
#'
#'vhtcdesign = gen_design(candidateset=vhtc, model=~Store, trials=6, varianceratio=1)
#'htcdesign = gen_design(candidateset=htc, model=~Store+Temp, trials=18,
#'                       splitplotdesign=vhtcdesign, splitplotsizes=rep(3,6),varianceratio=1)
#'splitplotdesign = gen_design(candidateset=factorialcoffee,
#'                             model=~Store+Temp+cost+type+size, trials=54,
#'                             splitplotdesign=htcdesign, splitplotsizes=rep(3,18),varianceratio=1)
#'
#'#Each block has an additional noise term associated with it in addition to the normal error
#'#term in the model. This is specified by a vector specifying the additional variance for
#'#each split-plot level. This is equivalent to specifying a variance ratio of one between
#'#the whole plots and the sub-plots for gaussian models.
#'
#'#Evaluate the design. Note the decreased power for the blocking factors. If
#'eval_design_mc(RunMatrix=splitplotdesign, model=~Store+Temp+cost+type+size, alpha=0.05,
#'               nsim=100, glmfamily="gaussian", varianceratios = c(1,1))
#'
#'#We can also use this method to evaluate designs that cannot be easily
#'#evaluated using normal approximations. Here, we evaluate a design with a binomial response and see
#'#if we can detect the difference between each factor changing whether an event
#'#70% of the time or 90% of the time.
#'
#'factorialbinom = expand.grid(a=c(-1,1),b=c(-1,1))
#'designbinom = gen_design(factorialbinom,model=~a+b,trials=90,optimality="D",repeats=100)
#'
#'eval_design_mc(designbinom,~a+b,alpha=0.2,nsim=100,anticoef=c(1.5,0.7,0.7),
#'               glmfamily="binomial")
#'
#'#We can also use this method to determine power for poisson response variables.
#'#We design our test to detect if each factor changes the base rate of 0.2 by
#'#a factor of 2. We generate the design:
#'
#'factorialpois = expand.grid(a=as.numeric(c(-1,0,1)),b=c(-1,0,1))
#'designpois = gen_design(factorialpois, ~a+b, trials=90, optimality="D", repeats=100)
#'
#'eval_design_mc(designpois,~a+b,0.2,nsim=100,glmfamily="poisson", anticoef=c(log(0.2),log(2),log(2)))
#'
#'#where the anticipated coefficients are chosen to set the base rate at 0.2
#'#(from the intercept) as well as how each factor changes the rate (a factor of 2, so log(2)).
#'#We see here we need about 90 test events to get accurately distinguish the three different
#'#rates in each factor to 90% power.
eval_design_mc = function(RunMatrix, model, alpha,
                          blocking=FALSE, nsim=1000, glmfamily="gaussian",
                          varianceratios = NULL, rfunction=NULL, anticoef=NULL, delta=2,
                          contrasts=contr.sum, binomialprobs = NULL,
                          parallel=FALSE, detailedoutput=FALSE) {
  glmfamilyname = glmfamily
  #------Auto-set random generating function----#
  if(is.null(rfunction)) {
    if(glmfamily == "gaussian") {
      rfunction = function(X,b,blockvector) {return(rnorm(n=nrow(X), mean = X %*% b + blockvector, sd = 1))}
    }
    if(glmfamily == "binomial") {
      rfunction = function(X,b,blockvector) {return(rbinom(n=nrow(X), size = 1 ,prob = 1/(1+exp(-(X %*% b + blockvector)))))}
    }
    if(glmfamily == "poisson") {
      rfunction = function(X,b,blockvector) {return(rpois(n=nrow(X), lambda = exp((X %*% b + blockvector))))}
    }
    if(glmfamily == "exponential") {
      glmfamily = Gamma(link="log")
      rfunction = function(X,b,blockvector) {return(rexp(n=nrow(X), rate = exp(X %*% b + blockvector)))}
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
  #Remove columns from variables not used in the model
  RunMatrixReduced = reduceRunMatrix(RunMatrix,model)

  contrastslist_correlationmatrix = list()
  contrastslist = list()
  for(x in names(RunMatrixReduced[lapply(RunMatrixReduced, class) == "factor"])) {
    contrastslist[[x]] = contrasts
    contrastslist_correlationmatrix[[x]] = contr.simplex

  }
  if(length(contrastslist) < 1) {
    contrastslist = NULL
    contrastslist_correlationmatrix = NULL
  }

  #---------- Convert dot formula to terms -----#
  if((as.character(model)[2] == ".")) {
    model = as.formula(paste("~", paste(attr(RunMatrixReduced, "names"), collapse=" + "), sep=""))
  }

  if(as.character(model)[2] == "quad(.)") {
    if(any(lapply(RunMatrixReduced,class) %in% c("factor","character"))) {
      stop("quad() function cannot be used in models with categorical factors. Manually specify your model")
    }
    modelvars = colnames(model.matrix(~.,data=RunMatrixReduced,contrasts.arg = contrastslist))[-1]
    modellinear = paste(modelvars,collapse=" + ")
    modellinear = paste("~",modellinear,sep="")
    model = quad(as.formula(modellinear))
  }

  ModelMatrix = model.matrix(model,RunMatrixReduced,contrasts.arg=contrastslist)
  #We'll need the parameter and effect names for output
  parameter_names = colnames(ModelMatrix)
  effect_names = c("(Intercept)", attr(terms(model), 'term.labels'))

  #-----Autogenerate Anticipated Coefficients---#
  if(missing(anticoef)) {
    anticoef = gen_anticoef(RunMatrixReduced, model) * delta / 2
  }
  if(length(anticoef) != dim(ModelMatrix)[2] && any(lapply(RunMatrixReduced,class)=="factor")) {
    stop("Wrong number of anticipated coefficients")
  }
  if(length(anticoef) != dim(ModelMatrix)[2] && !any(lapply(RunMatrixReduced,class)=="factor")) {
    anticoef = rep(1,dim(ModelMatrix)[2]) * delta / 2
  }
  if(glmfamilyname == "binomial" && is.null(binomialprobs)) {
    warning("Warning: Binomial model using default (or user supplied) anticipated coefficients. Default anticipated coefficients can result in
            large shifts in probability throughout the design space. It is recommended to specify probability bounds in the argument
            binomialprobs for more realistic effect sizes.")
  }
  if(glmfamilyname == "binomial" && !is.null(binomialprobs)) {
    anticoef = gen_binomial_anticoef(anticoef,binomialprobs[1],binomialprobs[2]) #ignore delta argument
  }

  #-------------- Blocking errors --------------#
  blocknames = rownames(RunMatrix)
  blocklist = strsplit(blocknames,".",fixed=TRUE)

  if(any(lapply(blocklist,length) > 1)) {
    # if(blocking && (is.null(varianceratios) || max(unlist(lapply(blocklist,length)))-1 != length(varianceratios))) {
    if(blocking) {

      blockstructure = do.call(rbind,blocklist)
      blockgroups = apply(blockstructure,2,blockingstructure)

      if(!is.null(varianceratios) && max(unlist(lapply(blocklist,length)))-1 != length(varianceratios) && length(varianceratios) != 1) {
        warning("varianceratios length does not match number of split plots. Defaulting to variance ratio of 1 for all strata. ")
        varianceratios = rep(1,max(unlist(lapply(blocklist,length)))-1)
      }
      if(length(varianceratios) == 1 && max(unlist(lapply(blocklist,length)))-1 != length(varianceratios)) {
        varianceratios = rep(varianceratios,max(unlist(lapply(blocklist,length)))-1)
      }
      if(is.null(varianceratios)) {
        varianceratios = rep(1,max(unlist(lapply(blocklist,length)))-1)
      }
    }
  } else {
    if(blocking) {
      warning("Blocking set to TRUE, but no blocks detected in rownames. Blocking ignored.")
      blocking=FALSE
    }
  }

  rblocknoise = function(noise,groups) {
    listblocknoise = list()
    for(i in 1:(length(groups)-1)) {
      blocktemp = list()
      for(j in 1:length(groups[[i]])) {
        row = rnorm(n=1,mean=0,sd=noise[i])
        blocktemp[[j]] = do.call(rbind, replicate(groups[[i]][j],row,simplify = FALSE))
      }
      listblocknoise[[i]] = do.call(rbind,blocktemp)
    }
    totalblocknoise = Reduce("+",listblocknoise)
    return(totalblocknoise)
  }

  #------------ Generate Responses -------------#

  responses = matrix(ncol=nsim,nrow=nrow(ModelMatrix))
  if(blocking) {
    for(i in 1:nsim) {
      responses[,i] = rfunction(ModelMatrix,anticoef,rblocknoise(noise=varianceratios,groups=blockgroups))
    }
  } else {
    responses = replicate(nsim, rfunction(ModelMatrix,anticoef,rep(0,nrow(ModelMatrix))))
  }
  #-------Update formula with random blocks------#

  if(blocking) {
    genBlockIndicators = function(blockgroup) {return(rep(1:length(blockgroup),blockgroup))}
    blockindicators = lapply(blockgroups,genBlockIndicators)
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
    pvallist = list()
    power_values = rep(0, ncol(ModelMatrix))
    estimates = matrix(0, nrow = nsim, ncol = ncol(ModelMatrix))
    for (j in 1:nsim) {

      #simulate the data.
      RunMatrixReduced$Y = responses[,j]
      if (blocking) {
        if(glmfamilyname == "gaussian") {
          fit = lme4::lmer(model_formula, data=RunMatrixReduced, contrasts = contrastslist)
        } else {
          fit = lme4::glmer(model_formula, data=RunMatrixReduced, family=glmfamily, contrasts = contrastslist)
        }
        estimates[j, ] = coef(summary(fit))[, 1]
      } else {
        if (glmfamilyname == "gaussian") {
          fit = lm(model_formula, data=RunMatrixReduced, contrasts = contrastslist)
        } else {
          fit = glm(model_formula, family=glmfamily, data=RunMatrixReduced, contrasts = contrastslist)
        }
        estimates[j, ] = coef(fit)
      }
      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = extractPvalues(fit)
      pvallist[[j]] = pvals
      power_values[pvals < alpha] = power_values[pvals < alpha] + 1
    }
    #We are going to output a tidy data.frame with the results, so just append the effect powers
    #to the parameter powers. We'll use another column of that dataframe to label whether it is parameter
    #or effect power.\
    attr(power_values, "pvals") = do.call(rbind,pvallist)
    power_values = power_values / nsim

  } else {
    cl <- parallel::makeCluster(parallel::detectCores())
    doParallel::registerDoParallel(cl, cores = parallel::detectCores())

    power_estimates = foreach::foreach (j = 1:nsim, .combine = "rbind", .packages = c("lme4")) %dopar% {
      power_values = rep(0, ncol(ModelMatrix))
      #simulate the data.
      RunMatrixReduced$Y = responses[,j]
      if (blocking) {
        if(glmfamilyname == "gaussian") {
          fit = lme4::lmer(model_formula, data=RunMatrixReduced, contrasts = contrastslist)
        } else {
          fit = lme4::glmer(model_formula, data=RunMatrixReduced, family=glmfamily, contrasts = contrastslist)
        }
        estimates = coef(summary(fit))[, 1]
      } else {
        if (glmfamilyname == "gaussian") {
          fit = lm(model_formula, data=RunMatrixReduced, contrasts = contrastslist)
        } else {
          fit = glm(model_formula, family=glmfamily, data=RunMatrixReduced,contrasts = contrastslist)
        }
        estimates = coef(fit)
      }
      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = extractPvalues(fit)
      power_values[pvals < alpha] = 1

      c(power_values, estimates, pvals)
    }
    dfsplit = ncol(ModelMatrix)
    parallel::stopCluster(cl)
    power_values = apply(power_estimates[, 1:dfsplit], 2, sum) / nsim
    estimates = power_estimates[, (dfsplit + 1):(2*dfsplit)]
    attr(power_values,"pvals") = power_estimates[, (2*dfsplit + 1):ncol(power_estimates)]
  }
  #output the results (tidy data format)
  retval = data.frame(parameters=parameter_names,
                    type="parameter.power.mc",
                    power=power_values)
  attr(retval, "modelmatrix") = ModelMatrix
  attr(retval, "anticoef") = anticoef

  modelmatrix_cor = model.matrix(model,RunMatrixReduced,contrasts.arg=contrastslist_correlationmatrix)
  if(ncol(modelmatrix_cor) > 2) {
    tryCatch({
      correlation.matrix = abs(cov2cor(solve(t(modelmatrix_cor) %*% modelmatrix_cor))[-1,-1])
      colnames(correlation.matrix) = colnames(modelmatrix_cor)[-1]
      rownames(correlation.matrix) = colnames(modelmatrix_cor)[-1]
      attr(retval,"correlation.matrix") = round(correlation.matrix,8)
    }, error = function(e) {})
  }

  if(detailedoutput) {
    retval$anticoef = anticoef
    retval$alpha = alpha
    retval$glmfamily = glmfamily
    retval$trials = nrow(RunMatrix)
    retval$nsim = nsim
    retval$blocking = blocking
  }

  colnames(estimates) = parameter_names
  attr(retval, 'estimates') = estimates
  attr(retval, 'pvals') = attr(power_values, "pvals")
  retval
}
globalVariables('i')
