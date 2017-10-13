#'@title Evaluate Power for Survival Design
#'
#'@description Evaluates power for an experimental design in which the response variable may be
#'right- or left-censored. Power is evaluated with a Monte Carlo simulation,
#'using the \code{survival} package and \code{survreg} to fit the data. Split-plot designs are not supported.
#'
#'@param RunMatrix The run matrix of the design. Internally, all numeric columns will be rescaled to [-1, +1].
#'@param model The statistical model used to fit the data.
#'@param alpha The type-I error.
#'@param nsim The number of simulations. Default 1000.
#'@param distribution Distribution of survival function to use when fitting the data. Valid choices are described
#'in the documentation for \code{survreg}. \emph{Supported} options are
#'"exponential", "lognormal", or "gaussian". Default "gaussian".
#'@param censorpoint The point after/before (for right-censored or left-censored data, respectively)
#'which data should be labelled as censored. Default NA for no censoring. This argument is
#'used only by the internal random number generators; if you supply your own function to
#'the \code{rfunctionsurv} parameter, then this parameter will be ignored.
#'@param censortype The type of censoring (either "left" or "right"). Default "right".
#'@param rfunctionsurv Random number generator function. Should be a function of the form f(X,b), where X is the
#'model matrix and b are the anticipated coefficients. This function should return a \code{Surv} object from
#'the \code{survival} package. You do not need to provide this argument if \code{distribution} is one of
#' the supported choices and you are satisfied with the default behavior described below.
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients
#'will be automatically generated based on the \code{effectsize} argument.
#'@param effectsize Helper argument to generate anticipated coefficients. See details for more info.
#'If you specify \code{anticoef}, \code{effectsize} will be ignored.
#'@param contrasts Default \code{contr.sum}. Function used to encode categorical variables in the model matrix. If the user has specified their own contrasts
#'for a categorical factor using the contrasts function, those will be used. Otherwise, skpr will use contr.sum.
#'@param parallel If TRUE, uses all cores available to speed up computation of power. Default FALSE.
#'@param detailedoutput If TRUE, return additional information about evaluation in results. Default FALSE.
#'@param progressBarUpdater Default NULL. Function called in non-parallel simulations that can be used to update external progress bar.
#'@param delta Deprecated. Use \code{effectsize} instead.
#'@param ... Any additional arguments to be passed into the \code{survreg} function during fitting.
#'@return A data frame consisting of the parameters and their powers. The parameter estimates from the simulations are
#'stored in the 'estimates' attribute. The 'modelmatrix' attribute contains the model matrix and the encoding used for
#'categorical factors. If you manually specify anticipated coefficients, do so in the order of the model matrix.
#'@import foreach doParallel survival stats iterators
#'@details Evaluates the power of a design with Monte Carlo simulation. Data is simulated and then fit
#'with a survival model (\code{survival::survreg}), and the fraction of simulations in which a parameter
#'is significant
#'(its p-value is less than the specified \code{alpha})
#'is the estimate of power for that parameter.
#'
#'If not supplied by the user, \code{rfunctionsurv} will be generated based on the \code{distribution}
#'argument as follows:
#'\tabular{lr}{
#'\bold{distribution}  \tab \bold{generating function} \cr
#'"gaussian"                  \tab \code{rnorm(mean = X \%*\% b, sd=1)}           \cr
#'"exponential"               \tab \code{rexp(rate = exp(-X \%*\% b))}           \cr
#'"lognormal"                 \tab \code{rlnorm(meanlog = X \%*\% b, sdlog=1)}           \cr
#'}
#'
#'In each case, if a simulated data point is past the censorpoint (greater than for right-censored, less than for
#'left-censored) it is marked as censored. See the examples below for how to construct your own function.
#'
#'
#'Power is dependent on the anticipated coefficients. You can specify those directly with the \code{anticoef}
#'argument, or you can use the \code{effectsize} argument to specify an effect size and \code{skpr} will auto-generate them.
#'You can provide either a length-1 or length-2 vector. If you provide a length-1 vector, the anticipated
#'coefficients will be half of \code{effectsize}; this is equivalent to saying that the \emph{linear predictor}
#'(for a gaussian model, the mean response; for an exponential model or lognormal model,
#'the log of the mean value)
#'changes by \code{effectsize} when a continuous factor goes from its lowest level to its highest level. If you provide a
#'length-2 vector, the anticipated coefficients will be set such that the \emph{mean response} changes from
#'\code{effectsize[1]} to \code{effectsize[2]} when a factor goes from its lowest level to its highest level, assuming
#'that the other factors are inactive (their x-values are zero).
#'
#'The effect of a length-2 \code{effectsize} depends on the \code{distribution} argument as follows:
#'
#'For \code{distribution = 'gaussian'}, the coefficients are set to \code{(effectsize[2] - effectsize[1]) / 2}.
#'
#'For \code{distribution = 'exponential'} or \code{'lognormal'},
#'the intercept will be
#'\code{1 / 2 * (log(effectsize[2]) + log(effectsize[1]))},
#'and the other coefficients will be
#'\code{1 / 2 * (log(effectsize[2]) - log(effectsize[1]))}.
#'
#'@export
#'@examples #These examples focus on the survival analysis case and assume familiarity
#'#with the basic functionality of eval_design_mc.
#'
#'#We first generate a simple 2-level design using expand.grid:
#'basicdesign = expand.grid(a=c(-1, 1))
#'design = gen_design(candidateset=basicdesign, model=~a, trials=15)
#'
#'#We can then evaluate the power of the design in the same way as eval_design_mc,
#'#now including the type of censoring (either right or left) and the point at which
#'#the data should be censored:
#'
#'eval_design_survival_mc(RunMatrix=design, model=~a, alpha=0.05,
#'                         nsim=100, distribution="exponential",
#'                         censorpoint=5,censortype="right")
#'
#'#Built-in Monte Carlo random generating functions are included for the gaussian, exponential,
#'#and lognormal distributions.
#'
#'#We can also evaluate different censored distributions by specifying a custom
#'#random generating function and changing the distribution argument.
#'
#'rlognorm = function(X, b) {
#'   Y = rlnorm(n=nrow(X), meanlog = X %*% b, sdlog = 0.4)
#'   censored = Y > 1.2
#'   Y[censored] = 1.2
#'   return(survival::Surv(time=Y, event=!censored, type="right"))
#'}
#'
#'#Any additional arguments are passed into the survreg function call.  As an example, you
#'#might want to fix the "scale" argument to survreg, when fitting a lognormal:
#'
#'eval_design_survival_mc(RunMatrix=design, model=~a, alpha=0.2, nsim=100,
#'                         distribution="lognormal", rfunctionsurv=rlognorm,
#'                         anticoef=c(0.184,0.101), scale=0.4)
eval_design_survival_mc = function(RunMatrix, model, alpha,
                                   nsim=1000, distribution="gaussian", censorpoint=NA, censortype="right",
                                   rfunctionsurv=NULL, anticoef=NULL, effectsize=2, contrasts = contr.sum,
                                   parallel=FALSE, detailedoutput=FALSE, progressBarUpdater=NULL, delta=NULL, ...) {

  if(!missing(delta)) {
    warning("argument delta deprecated. Use effectsize instead. Setting effectsize = delta.")
    effectsize=delta
  }

  #detect pre-set contrasts
  presetcontrasts = list()
  for(x in names(RunMatrix[lapply(RunMatrix,class) %in% c("character", "factor")])) {
    if(!is.null(attr(RunMatrix[[x]],"contrasts"))) {
      presetcontrasts[[x]] = attr(RunMatrix[[x]],"contrasts")
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

  #covert tibbles
  RunMatrix = as.data.frame(RunMatrix)

  #Generating random generation function for survival. If no censorpoint specified, return all uncensored.
  if(is.na(censorpoint)) {
    censorfunction = function(data, point) {return(rep(FALSE,length(data)))}
  }
  if(censortype == "left" && !is.na(censorpoint)) {
    censorfunction = function(data, point) {return(data<point)}
  }
  if(censortype == "right" && !is.na(censorpoint)) {
    censorfunction = function(data, point) {return(data>point)}
  }

  if(is.null(rfunctionsurv)) {
    if(distribution == "exponential") {
      rfunctionsurv = function(X, b) {
        Y = rexp(n=nrow(X), rate=exp(-(X %*% b)))
        condition = censorfunction(Y,censorpoint)
        Y[condition] = censorpoint
        return(survival::Surv(time=Y, event=!condition, type=censortype))
      }
    }
    if(distribution == "lognormal") {
      rfunctionsurv = function(X, b) {
        Y = rlnorm(n=nrow(X), meanlog = X %*% b, sdlog = 1)
        condition = censorfunction(Y,censorpoint)
        Y[condition] = censorpoint
        return(survival::Surv(time=Y, event=!condition, type=censortype))
      }
    }
    if(distribution == "gaussian") {
      rfunctionsurv = function(X, b) {
        Y = rnorm(n=nrow(X), mean=X %*% b, sd=1)
        condition = censorfunction(Y,censorpoint)
        Y[condition] = censorpoint
        return(survival::Surv(time=Y, event=!condition, type=censortype))
      }
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
  #remove columns from variables not used in the model
  RunMatrixReduced = reduceRunMatrix(RunMatrix,model)

  contrastslist = list()
  for(x in names(RunMatrix[lapply(RunMatrixReduced, class) %in% c("character", "factor")])) {
    if(!(x %in% names(presetcontrasts))) {
      contrastslist[[x]] = contrasts
    } else {
      contrastslist[[x]] = presetcontrasts[[x]]
    }
  }
  if(length(contrastslist) < 1) {
    contrastslist = NULL
  }

  #---------- Convert dot formula to terms -----#

  if(model == as.formula("~.*.")) {
    model = as.formula(paste0("~(",paste(colnames(RunMatrixReduced),collapse = " + "),")^2"))
  }

  if((as.character(model)[2] == ".")) {
    model = as.formula(paste("~", paste(attr(RunMatrixReduced, "names"), collapse=" + "), sep=""))
  }

  ModelMatrix = model.matrix(model,RunMatrixReduced,contrasts.arg=contrastslist)
  #We'll need the parameter and effect names for output
  parameter_names = colnames(ModelMatrix)
  effect_names = c("(Intercept)", attr(terms(model), 'term.labels'))

  # autogenerate anticipated coefficients
  if (!missing(anticoef) && !missing(effectsize)) {
    warning("User defined anticipated coefficients (anticoef) detected; ignoring effectsize argument.")
  }
  if(missing(anticoef)) {
    default_coef = gen_anticoef(RunMatrixReduced, model)
    anticoef = anticoef_from_delta_surv(default_coef, effectsize, distribution)
  }
  if(length(anticoef) != dim(ModelMatrix)[2]) {
    stop("Wrong number of anticipated coefficients")
  }


  nparam = ncol(ModelMatrix)
  RunMatrixReduced$Y = 1

  #---------------- Run Simulations ---------------#

  progressbarupdates = floor(seq(1,nsim,length.out=50))
  progresscurrent = 1

  if(!parallel) {
    power_values = rep(0, ncol(ModelMatrix))
    estimates = matrix(0, nrow = nsim, ncol = nparam)
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
      anticoef_adjusted = anticoef

      RunMatrixReduced$Y = rfunctionsurv(ModelMatrix,anticoef_adjusted)

      model_formula = update.formula(model, Y ~ .)

      #fit a model to the simulated data.
      fit = survival::survreg(model_formula, data=RunMatrixReduced, dist=distribution, ...)

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

      anticoef_adjusted = anticoef

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
  retval = data.frame(parameter=parameter_names,
                      type="parameter.power.mc",
                      power=power_values)
  colnames(estimates) = parameter_names
  attr(retval, 'estimates') = estimates
  attr(retval, "modelmatrix") = ModelMatrix
  attr(retval, "anticoef") = anticoef

  if(detailedoutput) {
    retval$anticoef = anticoef
    retval$alpha = alpha
    retval$distribution = distribution
    retval$trials = nrow(RunMatrix)
    retval$nsim = nsim
  }
  retval

}
globalVariables('i')
