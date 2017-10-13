#'@title Monte Carlo Power Evaluation for Experimental Designs
#'
#'@description Evaluates the power of an experimental design, given its run matrix and the
#'statistical model to be fit to the data, using monte carlo simulation. Simulated data is fit using a
#'generalized linear model
#'and power is estimated by the fraction of times a parameter is significant.
#'Returns a data frame of parameter powers.
#'
#'@param RunMatrix The run matrix of the design. Internally, \code{eval_design_mc} rescales each numeric column
#'to the range [-1, 1].
#'@param model The model used in evaluating the design. It can be a subset of the model used to
#'generate the design, or include higher order effects not in the original design generation. It cannot include
#'factors that are not present in the run matrix.
#'@param alpha The type-I error. p-values less than this will be counted as significant.
#'@param blocking If TRUE, \code{eval_design_mc} will look at the rownames to determine blocking structure. Default FALSE.
#'@param nsim The number of simulations to perform.
#'@param glmfamily String indicating the family of distribution for the glm function
#'("gaussian", "binomial", "poisson", or "exponential").
#'@param varianceratios Default 1. The ratio of the whole plot variance to the run-to-run variance. For designs with more than one subplot
#'this ratio can be a vector specifying the variance ratio for each subplot. Otherwise, it will use a single value for all strata.
#'@param rfunction Random number generator function for the response variable. Should be a function of the form f(X, b, delta), where X is the
#'model matrix, b are the anticipated coefficients, and delta is a vector of blocking errors. Typically something like rnorm(nrow(X), X * b + delta, 1).
#'You only need to specify this if you do not like the default behavior described below.
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients
#'will be automatically generated based on the \code{effectsize} or \code{binomialprobs} arguments.
#'@param effectsize Helper argument to generate anticipated coefficients. See details for more info.
#'If you specify \code{anticoef}, \code{effectsize} will be ignored.
#'@param contrasts Default \code{contr.sum}. The contrasts to use for categorical factors. If the user has specified their own contrasts
#'for a categorical factor using the contrasts function, those will be used. Otherwise, skpr will use contr.sum.
#'@param binomialprobs Equivalent to \code{effectsize}, maintained for backwards compatibility. See details for more info.
#'If you specify \code{anticoef}, this argument will be ignored.
#'@param parallel Default FALSE. If TRUE, uses all cores available to speed up computation. WARNING: This can slow down computation if nonparallel time to complete the computation is less than a few seconds.
#'@param detailedoutput If TRUE, return additional information about evaluation in results.
#'@param progressBarUpdater Default NULL. Function called in non-parallel simulations that can be used to update external progress bar.
#'@param delta Deprecated. Use \code{effectsize} instead.
#'@return A data frame consisting of the parameters and their powers, with supplementary information
#'stored in the data frame's attributes. The parameter estimates from the simulations are stored in the "estimates"
#' attribute. The "modelmatrix" attribute contains the model matrix that was used for power evaluation, and
#' also provides the encoding used for categorical factors. If you want to specify the anticipated
#' coefficients manually, do so in the order the parameters appear in the model matrix.
#'@details Evaluates the power of a design with Monte Carlo simulation. Data is simulated and then fit
#' with a generalized linear model, and the fraction of simulations in which a parameter
#' is significant
#'  (its p-value, according to the fit function used, is less than the specified \code{alpha})
#' is the estimate of power for that parameter.
#'
#'First, if \code{blocking = TURE}, the random noise from blocking is generated with \code{rnorm}.
#'Each block gets a single sample of Gaussian random noise, with a variance as specified in
#'\code{varianceratios},
#'and that sample is copied to each run in the block. Then, \code{rfunction} is called to generate a simulated
#'response for each run of the design, and the data is fit using the appropriate fitting function.
#'The functions used to simulate the data and fit it are determined by the \code{glmfamily}
#'and \code{blocking} arguments
#'as follows. Below, X is the model matrix, b is the anticipated coefficients, and d
#'is a vector of blocking noise (if \code{blocking = FALSE} then d = 0):
#'
#'\tabular{llrr}{
#'\bold{glmfamily}      \tab \bold{blocking} \tab \bold{rfunction} \tab \bold{fit} \cr
#'"gaussian"     \tab F        \tab \code{rnorm(mean = X \%*\% b + d, sd = 1)}        \tab \code{lm}         \cr
#'"gaussian"     \tab T        \tab \code{rnorm(mean = X \%*\% b + d, sd = 1)}        \tab \code{lme4::lmer} \cr
#'"binomial"     \tab F        \tab \code{rbinom(prob = 1/(1+exp(-(X \%*\% b + d))))} \tab \code{glm(family = "binomial")}  \cr
#'"binomial"     \tab T        \tab \code{rbinom(prob = 1/(1+exp(-(X \%*\% b + d))))} \tab \code{lme4::glmer(family = "binomial")} \cr
#'"poisson"      \tab F        \tab \code{rpois(lambda = exp((X \%*\% b + d)))}       \tab \code{glm(family = "poisson")}         \cr
#'"poisson"      \tab T        \tab \code{rpois(lambda = exp((X \%*\% b + d)))}       \tab \code{lme4::glmer(family = "poisson")} \cr
#'"exponential"  \tab F        \tab \code{rexp(rate = exp(-(X \%*\% b + d)))}            \tab \code{glm(family = Gamma(link="log"))} \cr
#'"exponential"  \tab T        \tab \code{rexp(rate = exp(-(X \%*\% b + d)))}            \tab \code{lme4:glmer(family = Gamma(link="log"))} \cr
#'}
#'Note that the exponential random generator uses the "rate" parameter, but \code{skpr} and \code{glm} use
#'the mean value parameterization (= 1 / rate), hence the minus sign above. Also note that
#'the gaussian model assumes a root-mean-square error of 1.
#'
#'Power is dependent on the anticipated coefficients. You can specify those directly with the \code{anticoef}
#'argument, or you can use the \code{effectsize} argument to specify an effect size and \code{skpr} will auto-generate them.
#'You can provide either a length-1 or length-2 vector. If you provide a length-1 vector, the anticipated
#'coefficients will be half of \code{effectsize}; this is equivalent to saying that the \emph{linear predictor}
#'(for a gaussian model, the mean response; for a binomial model, the log odds ratio; for an exponential model,
#'the log of the mean value; for a poisson model, the log of the expected response)
#'changes by \code{effectsize} when a continuous factor goes from its lowest level to its highest level. If you provide a
#'length-2 vector, the anticipated coefficients will be set such that the \emph{mean response} (for
#'a gaussian model, the mean response; for a binomial model, the probability; for an exponential model, the mean
#'response; for a poisson model, the expected response) changes from
#'\code{effectsize[1]} to \code{effectsize[2]} when a factor goes from its lowest level to its highest level, assuming
#'that the other factors are inactive (their x-values are zero).
#'
#'The effect of a length-2 \code{effectsize} depends on the \code{glmfamily} argument as follows:
#'
#'For \code{glmfamily = 'gaussian'}, the coefficients are set to \code{(effectsize[2] - effectsize[1]) / 2}.
#'
#'For \code{glmfamily = 'binomial'}, the intercept will be
#'\code{1/2 * log(effectsize[1] * effectsize[2] / (1 - effectsize[1]) / (1 - effectsize[2]))},
#'and the other coefficients will be
#'\code{1/2 * log(effectsize[2] * (1 - effectsize[1]) / (1 - effectsize[2]) / effectsize[1])}.
#'
#'For \code{glmfamily = 'exponential'} or \code{'poisson'},
#'the intercept will be
#'\code{1 / 2 * (log(effectsize[2]) + log(effectsize[1]))},
#'and the other coefficients will be
#'\code{1 / 2 * (log(effectsize[2]) - log(effectsize[1]))}.
#'
#'
#'@export
#'@import foreach doParallel nlme stats
#'@examples #We first generate a full factorial design using expand.grid:
#'factorialcoffee = expand.grid(cost=c(-1, 1),
#'                               type=as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
#'                               size=as.factor(c("Short", "Grande", "Venti")))
#'
#'#And then generate the 21-run D-optimal design using gen_design.
#'
#'designcoffee = gen_design(factorialcoffee, model=~cost + type + size, trials=21, optimality="D")
#'
#'#To evaluate this design using a normal approximation, we just use eval_design
#'#(here using the default settings for contrasts, effectsize, and the anticipated coefficients):
#'
#'eval_design(RunMatrix=designcoffee, model=~cost + type + size, 0.05)
#'
#'#To evaluate this design with a Monte Carlo method, we enter the same information
#'#used in eval_design, with the addition of the number of simulations "nsim" and the distribution
#'#family used in fitting for the glm "glmfamily". For gaussian, binomial, exponential, and poisson
#'#families, a default random generating function (rfunction) will be supplied. If another glm
#'#family is used or the default random generating function is not adequate, a custom generating
#'#function can be supplied by the user.
#'
#'\dontrun{eval_design_mc(RunMatrix=designcoffee, model=~cost + type + size, alpha=0.05, nsim=100,
#'               glmfamily="gaussian")}
#'
#'#We see here we generate approximately the same parameter powers as we do
#'#using the normal approximation in eval_design. Like eval_design, we can also change
#'#effectsize to produce a different signal-to-noise ratio:
#'
#'\dontrun{eval_design_mc(RunMatrix=designcoffee, model=~cost + type + size, alpha=0.05,
#'               nsim=100, glmfamily="gaussian", effectsize=1)}
#'
#'#Like eval_design, we can also evaluate the design with a different model than
#'#the one that generated the design.
#'\dontrun{eval_design_mc(RunMatrix=designcoffee, model=~cost + type, alpha=0.05,
#'               nsim=100, glmfamily="gaussian")}
#'
#'
#'#And here it is evaluated with interactions included:
#'\dontrun{eval_design_mc(RunMatrix=designcoffee,model=~cost + type + size + cost*type, 0.05,
#'               nsim=100, glmfamily="gaussian")}
#'
#'#We can also set "parallel=TRUE" to use all the cores available to speed up
#'#computation.
#'\dontrun{eval_design_mc(RunMatrix=designcoffee, model=~cost + type + size, 0.05,
#'               nsim=10000, glmfamily="gaussian", parallel=TRUE)}
#'
#'#We can also evaluate split-plot designs. First, let us generate the split-plot design:
#'
#'factorialcoffee2 = expand.grid(Temp = c(1,-1),
#'                                Store=as.factor(c("A","B")),
#'                                cost=c(-1, 1),
#'                                type=as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
#'                                size=as.factor(c("Short", "Grande", "Venti")))
#'
#'vhtcdesign = gen_design(candidateset=factorialcoffee2, model=~Store, trials=6, varianceratio=1)
#'htcdesign = gen_design(candidateset=factorialcoffee2, model=~Store+Temp, trials=18,
#'                        splitplotdesign=vhtcdesign, splitplotsizes=rep(3,6),varianceratio=1)
#'splitplotdesign = gen_design(candidateset=factorialcoffee2,
#'                              model=~Store+Temp+cost+type+size, trials=54,
#'                              splitplotdesign=htcdesign, splitplotsizes=rep(3,18),varianceratio=1)
#'
#'#Each block has an additional noise term associated with it in addition to the normal error
#'#term in the model. This is specified by a vector specifying the additional variance for
#'#each split-plot level. This is equivalent to specifying a variance ratio of one between
#'#the whole plots and the sub-plots for gaussian models.
#'
#'#Evaluate the design. Note the decreased power for the blocking factors.
#'\dontrun{eval_design_mc(RunMatrix=splitplotdesign, model=~Store+Temp+cost+type+size, alpha=0.05,
#'               blocking=TRUE, nsim=100, glmfamily="gaussian", varianceratios = c(1,1))}
#'
#'#We can also use this method to evaluate designs that cannot be easily
#'#evaluated using normal approximations. Here, we evaluate a design with a binomial response and see
#'#whether we can detect the difference between each factor changing whether an event occurs
#'#70% of the time or 90% of the time.
#'
#'factorialbinom = expand.grid(a=c(-1,1),b=c(-1,1))
#'designbinom = gen_design(factorialbinom,model=~a+b,trials=90,optimality="D")
#'
#'\dontrun{eval_design_mc(designbinom,~a+b,alpha=0.2,nsim=100, effectsize = c(0.7, 0.9),
#'               glmfamily="binomial")}
#'
#'#We can also use this method to determine power for poisson response variables.
#'#Generate the design:
#'
#'factorialpois = expand.grid(a=as.numeric(c(-1,0,1)),b=c(-1,0,1))
#'designpois = gen_design(factorialpois, ~a+b, trials=70, optimality="D")
#'
#'#Evaluate the power:
#'
#'\dontrun{eval_design_mc(designpois, ~a+b, 0.05, nsim=100, glmfamily="poisson",
#'                anticoef=log(c(0.2, 2, 2)))}
#'
#'
#'#The coefficients above set the nominal value -- that is, the expected count
#'#when all inputs = 0 -- to 0.2 (from the intercept), and say that each factor
#'#changes this count by a factor of 4 (multiplied by 2 when x= +1, and divided by 2 when x = -1).
#'#Note the use of log() in the anticipated coefficients.
eval_design_mc = function(RunMatrix, model, alpha,
                          blocking=FALSE, nsim=1000, glmfamily="gaussian",
                          varianceratios = NULL, rfunction=NULL, anticoef=NULL, effectsize=2,
                          contrasts=contr.sum, binomialprobs = NULL,
                          parallel=FALSE, detailedoutput=FALSE, progressBarUpdater=NULL,delta=NULL) {

  if(!missing(delta)) {
    warning("argument delta deprecated. Use effectsize instead. Setting effectsize = delta.")
    effectsize=delta
  }

  if(class(RunMatrix) %in% c("tbl","tbl_df") && blocking) {
    warning("Tibbles strip out rownames, which encode blocking information. Use data frames if the design has a split plot structure. Converting input to data frame")
  }

  #detect pre-set contrasts
  presetcontrasts = list()
  for(x in names(RunMatrix[lapply(RunMatrix,class) %in% c("character", "factor")])) {
    if(!is.null(attr(RunMatrix[[x]],"contrasts"))) {
      presetcontrasts[[x]] = attr(RunMatrix[[x]],"contrasts")
    }
  }

  #covert tibbles
  RunMatrix = as.data.frame(RunMatrix)

  #Detect externally generated blocking columns and convert to rownames
  if(is.null(attr(RunMatrix,"splitanalyzable")) &&
     any(grepl("(Block|block)(\\s?)+[0-9]+$",colnames(RunMatrix),perl=TRUE)) ||
     any(grepl("(Whole Plots|Subplots)",colnames(RunMatrix),perl=TRUE))) {
    blockcols = grepl("(Block|block)(\\s?)+[0-9]+$",colnames(RunMatrix),perl=TRUE) | grepl("(Whole Plots|Subplots)",colnames(RunMatrix),perl=TRUE)
    if(blocking) {
      warning("Detected externally generated blocking columns: attempting to interpret blocking structure.")
      blockmatrix = RunMatrix[,blockcols]
      blockvals = lapply(blockmatrix,unique)
      rownamematrix = matrix(nrow=nrow(RunMatrix),ncol=ncol(blockmatrix) + 1)
      for(col in 1:ncol(blockmatrix)) {
        uniquevals = blockvals[[col]]
        blockcounter = 1
        for(block in uniquevals) {
          if(col == 1) {
            rownamematrix[blockmatrix[,col] == block,col] = blockcounter
            blockcounter = blockcounter + 1
          }
          if(col != 1) {
            superblock = rownamematrix[blockmatrix[,col] == block,col-1][1]
            modop = length(unique(blockmatrix[blockmatrix[,col-1] == superblock,col]))
            if(blockcounter %% modop == 0) {
              rownamematrix[blockmatrix[,col] == block,col] = modop
            } else {
              rownamematrix[blockmatrix[,col] == block,col] = blockcounter %% modop
            }
            blockcounter = blockcounter + 1
          }
          if(col == ncol(blockmatrix)) {
            rownamematrix[blockmatrix[,col] == block,col+1] = 1:sum(blockmatrix[,col] == block)
          }
        }
        blockcounter = blockcounter + 1
      }
      allattr = attributes(RunMatrix)
      allattr$names = allattr$names[!blockcols]
      RunMatrix = RunMatrix[,!blockcols]
      attributes(RunMatrix) = allattr
      rownames(RunMatrix) = apply(rownamematrix,1,paste,collapse=".")
    } else {
      warning("Detected externally generated blocking columns but blocking not turned on: ignoring blocking structure and removing blocking columns.")
      allattr = attributes(RunMatrix)
      allattr$names = allattr$names[!blockcols]
      RunMatrix = RunMatrix[,!blockcols]
      attributes(RunMatrix) = allattr
    }
  }

  #Remove skpr-generated REML blocking indicators if present
  if(!is.null(attr(RunMatrix,"splitanalyzable"))) {
    if(attr(RunMatrix,"splitanalyzable")) {
      allattr = attributes(RunMatrix)
      RunMatrix = RunMatrix[,-1:-length(allattr$splitcolumns)]
      allattr$names = allattr$names[-1:-length(allattr$splitcolumns)]
      attributes(RunMatrix) = allattr
    }
  }

  glmfamilyname = glmfamily

  #------Auto-set random generating function----#
  if(is.null(rfunction)) {
    if(glmfamily == "gaussian") {
      rfunction = function(X,b,blockvector) {return(rnorm(n=nrow(X), mean = X %*% b + blockvector, sd = 1))}
    }
    if(glmfamily == "binomial") {
      rfunction = function(X,b,blockvector) {return(rbinom(n=nrow(X), size = 1, prob = 1/(1+exp(-(X %*% b + blockvector)))))}
    }
    if(glmfamily == "poisson") {
      rfunction = function(X,b,blockvector) {return(rpois(n=nrow(X), lambda = exp((X %*% b + blockvector))))}
    }
    if(glmfamily == "exponential") {
      glmfamily = Gamma(link="log")
      rfunction = function(X,b,blockvector) {return(rexp(n=nrow(X), rate = exp(-(X %*% b + blockvector))))}
    }
  }

  #------Normalize/Center numeric columns ------#
  for(column in 1:ncol(RunMatrix)) {
    if(is.numeric(RunMatrix[,column])) {
      midvalue = mean(c(max(RunMatrix[,column]),min(RunMatrix[,column])))
      RunMatrix[,column] = (RunMatrix[,column]-midvalue)/(max(RunMatrix[,column])-midvalue)
    }
  }

  #---------- Generating model matrix ----------#
  #Remove columns from variables not used in the model
  #Variables used later: contrastslist, contrastslist_correlationmatrix
  RunMatrixReduced = reduceRunMatrix(RunMatrix,model)

  contrastslist_correlationmatrix = list()
  contrastslist = list()
  for(x in names(RunMatrixReduced[lapply(RunMatrixReduced, class) %in% c("character", "factor")])) {
    if(!(x %in% names(presetcontrasts))) {
      contrastslist[[x]] = contrasts
    } else {
      contrastslist[[x]] = presetcontrasts[[x]]
    }
    contrastslist_correlationmatrix[[x]] = contr.simplex

  }
  if(length(contrastslist) < 1) {
    contrastslist = NULL
    contrastslist_correlationmatrix = NULL
  }

  #---------- Convert dot formula to terms -----#

  if(model == as.formula("~.*.")) {
    model = as.formula(paste0("~(",paste(colnames(RunMatrixReduced),collapse = " + "),")^2"))
  }

  #Variables used later: model, ModelMatrix
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

  #Parameter names for final output
  parameter_names = colnames(ModelMatrix)

  #-----Autogenerate Anticipated Coefficients---#
  #Variables used later: anticoef
  if (!is.null(binomialprobs) && glmfamily == "binomial") {
    #effectsize now has the same functionality as binomialprobs
    effectsize = binomialprobs
    message("binomialprobs deprecated: binomialprobs is now equivalent to effectsize. ")
  }
  if (!missing(anticoef) && !missing(effectsize)) {
    warning("User defined anticipated coefficients (anticoef) detected; ignoring effectsize argument.")
  }
  if(missing(anticoef)) {
    default_coef = gen_anticoef(RunMatrixReduced, model)
    anticoef = anticoef_from_delta(default_coef, effectsize, glmfamilyname)
  }
  if(length(anticoef) != dim(ModelMatrix)[2]) {
    stop("Wrong number of anticipated coefficients")
  }

  #-------------- Blocking errors --------------#
  #Variables used later: blockgroups, varianceratios
  blocknames = rownames(RunMatrix)
  blocklist = strsplit(blocknames,".",fixed=TRUE)

  if(any(lapply(blocklist,length) > 1)) {
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
        row = rnorm(n=1,mean=0,sd=sqrt(noise[i]))
        blocktemp[[j]] = do.call(rbind, replicate(groups[[i]][j],row,simplify = FALSE))
      }
      listblocknoise[[i]] = do.call(rbind,blocktemp)
    }
    totalblocknoise = Reduce("+",listblocknoise)
    return(totalblocknoise)
  }

  #------------ Generate Responses -------------#
  #Variables used later: responses
  responses = matrix(ncol=nsim,nrow=nrow(ModelMatrix))
  if(blocking) {
    for(i in 1:nsim) {
      responses[,i] = rfunction(ModelMatrix,anticoef,rblocknoise(noise=varianceratios,groups=blockgroups))
    }
  } else {
    responses = replicate(nsim, rfunction(ModelMatrix,anticoef,rep(0,nrow(ModelMatrix))))
  }

  #-------Update formula with random blocks------#
  #Variables used later: model, model_formula
  if(blocking) {
    genBlockIndicators = function(blockgroup) {return(rep(1:length(blockgroup),blockgroup))}
    blockindicators = lapply(blockgroups,genBlockIndicators)
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

  progressbarupdates = floor(seq(1,nsim,length.out=50))
  progresscurrent = 1

  if(!parallel) {
    pvallist = list()
    stderrlist = list()
    iterlist = list()
    power_values = rep(0, ncol(ModelMatrix))
    estimates = matrix(0, nrow = nsim, ncol = ncol(ModelMatrix))
    for (j in 1:nsim) {
      if(!is.null(progressBarUpdater)) {
        if(nsim > 50) {
          if(progressbarupdates[progresscurrent] == j) {
            progressBarUpdater(1/50)
            progresscurrent = progresscurrent + 1
          }
        } else {
          progressBarUpdater(1/nsim)
        }
      }

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
      stderrlist[[j]] = coef(summary(fit))[,2]
      if (!blocking) {
        iterlist[[j]] = fit$iter
      } else {
        iterlist[[j]] = NA
      }
      power_values[pvals < alpha] = power_values[pvals < alpha] + 1
    }
    #We are going to output a tidy data.frame with the results.
    attr(power_values, "pvals") = do.call(rbind,pvallist)
    attr(power_values, "stderrors") = do.call(rbind,stderrlist)
    attr(power_values, "fisheriterations") = do.call(rbind,iterlist)
    power_values = power_values / nsim

  } else {
    cl <- parallel::makeCluster(parallel::detectCores())
    numbercores = parallel::detectCores()
    doParallel::registerDoParallel(cl, cores = numbercores)

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
      stderrval = coef(summary(fit))[,2]
      power_values[pvals < alpha] = 1
      if (!blocking && !is.null(fit$iter)) {
        iterval = fit$iter
      } else {
        iterval = NA
      }
      c(power_values, estimates, pvals, stderrval, iterval)
    }
    dfsplit = ncol(ModelMatrix)
    parallel::stopCluster(cl)
    power_values = apply(power_estimates[, 1:dfsplit], 2, sum) / nsim
    estimates = power_estimates[, (dfsplit + 1):(2*dfsplit)]
    attr(power_values,"pvals") = power_estimates[, (2*dfsplit + 1):(3*dfsplit)]
    attr(power_values,"stderrors") = power_estimates[, (3*dfsplit + 1):(4*dfsplit)]
    attr(power_values,"fisheriterations") = power_estimates[, (4*dfsplit + 1)]
  }

  #output the results (tidy data format)
  retval = data.frame(parameter=parameter_names,
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
    if (is.character(glmfamilyname)) {
      retval$glmfamily = glmfamilyname
    }
    else { #user supplied a glm family object
      retval$glmfamily = paste(glmfamilyname, collapse = ' ')
    }
    retval$trials = nrow(RunMatrix)
    retval$nsim = nsim
    retval$blocking = blocking
  }

  colnames(estimates) = parameter_names
  attr(retval, 'estimates') = estimates
  attr(retval, 'pvals') = attr(power_values, "pvals")
  attr(retval, 'stderrors') = attr(power_values, "stderrors")
  attr(retval, 'fisheriterations') = attr(power_values,"fisheriterations")
  return(retval)
}
globalVariables('i')
