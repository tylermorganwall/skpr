#'@title Monte Carlo Power Evaluation for Experimental Designs
#'
#'@description Evaluates the power of an experimental design, given the run matrix and the
#'statistical model to be fit to the data, using monte carlo simulation. Simulated data is fit using a
#'generalized linear model
#'and power is estimated by the fraction of times a parameter is significant.
#'Returns a data frame of parameter powers.
#'
#'@param design The experimental design. Internally, \code{eval_design_mc} rescales each numeric column
#'to the range [-1, 1].
#'@param model The model used in evaluating the design. It can be a subset of the model used to
#'generate the design, or include higher order effects not in the original design generation. It cannot include
#'factors that are not present in the experimental design.
#'@param alpha The type-I error. p-values less than this will be counted as significant.
#'@param blocking If TRUE, \code{eval_design_mc} will look at the rownames to determine blocking structure. Default FALSE.
#'@param nsim The number of simulations to perform.
#'@param glmfamily String indicating the family of distribution for the glm function
#'("gaussian", "binomial", "poisson", or "exponential").
#'@param calceffect Default `TRUE`. Calculates effect power for a Type-III Anova (using the car package) using a Wald test.
#'this ratio can be a vector specifying the variance ratio for each subplot. Otherwise, it will use a single value for all strata.
#'@param varianceratios Default `1`. The ratio of the whole plot variance to the run-to-run variance. For designs with more than one subplot
#'this ratio can be a vector specifying the variance ratio for each subplot. Otherwise, it will use a single value for all strata.
#'@param rfunction Random number generator function for the response variable. Should be a function of the form f(X, b, delta), where X is the
#'model matrix, b are the anticipated coefficients, and delta is a vector of blocking errors. Typically something like rnorm(nrow(X), X * b + delta, 1).
#'You only need to specify this if you do not like the default behavior described below.
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients
#'will be automatically generated based on the \code{effectsize} argument.
#'@param effectsize Helper argument to generate anticipated coefficients. See details for more info.
#'If you specify \code{anticoef}, \code{effectsize} will be ignored.
#'@param contrasts Default \code{contr.sum}. The contrasts to use for categorical factors. If the user has specified their own contrasts
#'for a categorical factor using the contrasts function, those will be used. Otherwise, skpr will use contr.sum.
#'@param parallel Default FALSE. If TRUE, uses all cores available to speed up computation. WARNING: This can slow down computation if nonparallel time to complete the computation is less than a few seconds.
#'@param detailedoutput If TRUE, return additional information about evaluation in results.
#'@param advancedoptions Default NULL. Named list of advanced options. `advancedoptions$anovatype` specifies the Anova type in the car package (default type `III`),
#'user can change to type `II`). `advancedoptions$anovatest` specifies the test statistic if the user does not want a `Wald` test--other options are likelyhood-ratio `LR` and F-test `F`.
#'`advancedoptions$progressBarUpdater` is a function called in non-parallel simulations that can be used to update external progress bar.`advancedoptions$GUI` turns off some warning messages when in the GUI.
#'@param ... Additional arguments.
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
#'"exponential"  \tab F        \tab \code{rexp(rate = exp(-(X \%*\% b + d)))}            \tab \code{glm(family = Gamma(link = "log"))} \cr
#'"exponential"  \tab T        \tab \code{rexp(rate = exp(-(X \%*\% b + d)))}            \tab \code{lme4:glmer(family = Gamma(link = "log"))} \cr
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
#'factorialcoffee = expand.grid(cost = c(-1, 1),
#'                               type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
#'                               size = as.factor(c("Short", "Grande", "Venti")))
#'
#'#And then generate the 21-run D-optimal design using gen_design.
#'
#'designcoffee = gen_design(factorialcoffee,
#'                          model = ~cost + type + size, trials = 21, optimality = "D")
#'
#'#To evaluate this design using a normal approximation, we just use eval_design
#'#(here using the default settings for contrasts, effectsize, and the anticipated coefficients):
#'
#'eval_design(design = designcoffee, model = ~cost + type + size, 0.05)
#'
#'#To evaluate this design with a Monte Carlo method, we enter the same information
#'#used in eval_design, with the addition of the number of simulations "nsim" and the distribution
#'#family used in fitting for the glm "glmfamily". For gaussian, binomial, exponential, and poisson
#'#families, a default random generating function (rfunction) will be supplied. If another glm
#'#family is used or the default random generating function is not adequate, a custom generating
#'#function can be supplied by the user.
#'
#'\dontrun{eval_design_mc(designcoffee, model = ~cost + type + size, alpha = 0.05, nsim = 100,
#'                        glmfamily = "gaussian")}
#'
#'#We see here we generate approximately the same parameter powers as we do
#'#using the normal approximation in eval_design. Like eval_design, we can also change
#'#effectsize to produce a different signal-to-noise ratio:
#'
#'\dontrun{eval_design_mc(design = designcoffee, model = ~cost + type + size, alpha = 0.05,
#'               nsim = 100, glmfamily = "gaussian", effectsize = 1)}
#'
#'#Like eval_design, we can also evaluate the design with a different model than
#'#the one that generated the design.
#'\dontrun{eval_design_mc(design = designcoffee, model = ~cost + type, alpha = 0.05,
#'               nsim = 100, glmfamily = "gaussian")}
#'
#'
#'#And here it is evaluated with interactions included:
#'\dontrun{eval_design_mc(design = designcoffee, model = ~cost + type + size + cost * type, 0.05,
#'               nsim = 100, glmfamily = "gaussian")}
#'
#'#We can also set "parallel = TRUE" to use all the cores available to speed up
#'#computation.
#'\dontrun{eval_design_mc(design = designcoffee, model = ~cost + type + size, 0.05,
#'               nsim = 10000, glmfamily = "gaussian", parallel = TRUE)}
#'
#'#We can also evaluate split-plot designs. First, let us generate the split-plot design:
#'
#'factorialcoffee2 = expand.grid(Temp = c(1, -1),
#'                                Store = as.factor(c("A", "B")),
#'                                cost = c(-1, 1),
#'                                type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
#'                                size = as.factor(c("Short", "Grande", "Venti")))
#'
#'vhtcdesign = gen_design(factorialcoffee2,
#'                        model = ~Store, trials = 6, varianceratio = 1)
#'htcdesign = gen_design(factorialcoffee2, model = ~Store + Temp, trials = 18,
#'                        splitplotdesign = vhtcdesign, splitplotsizes = rep(3, 6), varianceratio = 1)
#'splitplotdesign = gen_design(factorialcoffee2,
#'                             model = ~Store + Temp + cost + type + size, trials = 54,
#'                             splitplotdesign = htcdesign, splitplotsizes = rep(3, 18),
#'                             varianceratio = 1)
#'
#'#Each block has an additional noise term associated with it in addition to the normal error
#'#term in the model. This is specified by a vector specifying the additional variance for
#'#each split-plot level. This is equivalent to specifying a variance ratio of one between
#'#the whole plots and the sub-plots for gaussian models.
#'
#'#Evaluate the design. Note the decreased power for the blocking factors.
#'\dontrun{eval_design_mc(splitplotdesign, model = ~Store + Temp + cost + type + size, alpha = 0.05,
#'               blocking = TRUE, nsim = 100, glmfamily = "gaussian", varianceratios = c(1, 1))}
#'
#'#We can also use this method to evaluate designs that cannot be easily
#'#evaluated using normal approximations. Here, we evaluate a design with a binomial response and see
#'#whether we can detect the difference between each factor changing whether an event occurs
#'#70% of the time or 90% of the time.
#'
#'factorialbinom = expand.grid(a = c(-1, 1), b = c(-1, 1))
#'designbinom = gen_design(factorialbinom, model = ~a + b, trials = 90, optimality = "D")
#'
#'\dontrun{eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, effectsize = c(0.7, 0.9),
#'               glmfamily = "binomial")}
#'
#'#We can also use this method to determine power for poisson response variables.
#'#Generate the design:
#'
#'factorialpois = expand.grid(a = as.numeric(c(-1, 0, 1)), b = c(-1, 0, 1))
#'designpois = gen_design(factorialpois, ~a + b, trials = 70, optimality = "D")
#'
#'#Evaluate the power:
#'
#'\dontrun{eval_design_mc(designpois, ~a + b, 0.05, nsim = 100, glmfamily = "poisson",
#'                anticoef = log(c(0.2, 2, 2)))}
#'
#'
#'#The coefficients above set the nominal value -- that is, the expected count
#'#when all inputs = 0 -- to 0.2 (from the intercept), and say that each factor
#'#changes this count by a factor of 4 (multiplied by 2 when x= +1, and divided by 2 when x = -1).
#'#Note the use of log() in the anticipated coefficients.
eval_design_mc = function(design, model, alpha,
                          blocking = FALSE, nsim = 1000, glmfamily = "gaussian", calceffect = TRUE,
                          varianceratios = NULL, rfunction = NULL, anticoef = NULL,
                          effectsize = 2, contrasts = contr.sum, parallel = FALSE,
                          detailedoutput = FALSE, advancedoptions = NULL, ...) {
  args = list(...)
  if("RunMatrix" %in% names(args)) {
    stop("RunMatrix argument deprecated. Use `design` instead.")
  }

  if (!is.null(advancedoptions)) {
    if (is.null(advancedoptions$GUI)) {
      advancedoptions$GUI = FALSE
    }
    if (!is.null(advancedoptions$progressBarUpdater)) {
      progressBarUpdater = advancedoptions$progressBarUpdater
    } else {
      progressBarUpdater = NULL
    }
  } else {
    advancedoptions = list()
    advancedoptions$GUI = FALSE
    progressBarUpdater = NULL
  }
  if(attr(terms.formula(model,data=design),"intercept") == 1) {
    nointercept = FALSE
  } else {
    nointercept = TRUE
  }

  #detect pre-set contrasts
  presetcontrasts = list()
  for (x in names(design[lapply(design, class) %in% c("character", "factor")])) {
    if (!is.null(attr(design[[x]], "contrasts"))) {
      presetcontrasts[[x]] = attr(design[[x]], "contrasts")
    }
  }

  #covert tibbles
  run_matrix_processed = as.data.frame(design)
  #Detect externally generated blocking columns and convert to rownames
  if(is.null(varianceratios)) {
    if(!is.null(attr(design, "varianceratios"))) {
      varianceratios = attr(design, "varianceratios")[-1]
    } else {
      varianceratios = 1
    }
  }
  run_matrix_processed = convert_blockcolumn_rownames(run_matrix_processed, blocking, varianceratios)

  #Remove skpr-generated REML blocking indicators if present
  run_matrix_processed = remove_skpr_blockcols(run_matrix_processed)

  #----- Convert dots in formula to terms -----#
  model = convert_model_dots(run_matrix_processed,model)

  #----- Rearrange formula terms by order -----#
  model = rearrange_formula_by_order(model)
  if(nointercept) {
    model = update.formula(model,~-1 + .)
  }

  glmfamilyname = glmfamily

  #------Auto-set random generating function----#
  if (is.null(rfunction)) {
    if (glmfamily == "gaussian") {
      rfunction = function(X, b, blockvector) rnorm(n = nrow(X), mean = X %*% b + blockvector, sd = 1)
    }
    if (glmfamily == "binomial") {
      rfunction = function(X, b, blockvector) rbinom(n = nrow(X), size = 1, prob = 1 / (1 + exp(-(X %*% b + blockvector))))
    }
    if (glmfamily == "poisson") {
      rfunction = function(X, b, blockvector) rpois(n = nrow(X), lambda = exp(X %*% b + blockvector))
    }
    if (glmfamily == "exponential") {
      glmfamily = Gamma(link = "log")
      rfunction = function(X, b, blockvector) rexp(n = nrow(X), rate = exp(-(X %*% b + blockvector)))
    }
  }

  #------Normalize/Center numeric columns ------#
  run_matrix_processed = normalize_numeric_runmatrix(run_matrix_processed)

  #---------- Generating model matrix ----------#
  #Remove columns from variables not used in the model
  #Variables used later: contrastslist, contrastslist_cormat
  RunMatrixReduced = reduceRunMatrix(run_matrix_processed, model)

  contrastslist_cormat = list()
  contrastslist = list()
  for (x in names(RunMatrixReduced[lapply(RunMatrixReduced, class) %in% c("character", "factor")])) {
    if (!(x %in% names(presetcontrasts))) {
      contrastslist[[x]] = contrasts
    } else {
      contrastslist[[x]] = presetcontrasts[[x]]
    }
    contrastslist_cormat[[x]] = contr.simplex

  }
  if (length(contrastslist) < 1) {
    contrastslist = NULL
    contrastslist_cormat = NULL
  }
  ModelMatrix = model.matrix(model, RunMatrixReduced, contrasts.arg = contrastslist)

  #saving model for return attribute
  generatingmodel = model

  #Parameter names for final output
  parameter_names = colnames(ModelMatrix)

  #-----Autogenerate Anticipated Coefficients---#
  #Variables used later: anticoef
  if (!missing(effectsize)) {
    if(!is.null(anticoef)) {
      warning("User defined anticipated coefficients (anticoef) detected; ignoring effectsize argument.")
    }
  }
  if (missing(anticoef) || is.null(anticoef)) {
    default_coef = gen_anticoef(RunMatrixReduced, model, nointercept)
    anticoef = anticoef_from_delta(default_coef, effectsize, glmfamilyname)
    if (!("(Intercept)" %in% colnames(ModelMatrix))) {
      anticoef = anticoef[-1]
    }
  }
  if (length(anticoef) != dim(ModelMatrix)[2]) {
    stop("Wrong number of anticipated coefficients")
  }

  #-------------- Blocking errors --------------#
  #Variables used later: blockgroups, varianceratios, V
  blocknames = rownames(run_matrix_processed)
  blocklist = strsplit(blocknames, ".", fixed = TRUE)
  if (any(lapply(blocklist, length) > 1)) {
    if (blocking) {

      blockstructure = do.call(rbind, blocklist)
      blockgroups = apply(blockstructure, 2, blockingstructure)


      blockMatrixSize = nrow(run_matrix_processed)
      V = diag(blockMatrixSize)

      if (!is.null(varianceratios) && max(unlist(lapply(blocklist, length))) - 1 != length(varianceratios) && length(varianceratios) != 1) {
        warning("varianceratios length does not match number of split plots. Defaulting to variance ratio of 1 for all strata. ")
        varianceratios = rep(1, max(unlist(lapply(blocklist, length))) - 1)
      }
      if (length(varianceratios) == 1 && max(unlist(lapply(blocklist, length))) - 1 != length(varianceratios)) {
        varianceratios = rep(varianceratios, max(unlist(lapply(blocklist, length))) - 1)
      }
      if (is.null(varianceratios)) {
        varianceratios = rep(1, max(unlist(lapply(blocklist, length))) - 1)
      }

      blockcounter = 1

      blockgroups2 = blockgroups[-length(blockgroups)]
      for (block in blockgroups2) {
        V[1:block[1], 1:block[1]] =  V[1:block[1], 1:block[1]] + varianceratios[blockcounter]
        placeholder = block[1]
        for (i in 2:length(block)) {
          V[(placeholder + 1):(placeholder + block[i]), (placeholder + 1):(placeholder + block[i])] = V[(placeholder + 1):(placeholder + block[i]), (placeholder + 1):(placeholder + block[i])] + varianceratios[blockcounter]
          placeholder = placeholder + block[i]
        }
        blockcounter = blockcounter + 1
      }
    }
  } else {
    if (blocking) {
      warning("Blocking set to TRUE, but no blocks detected in rownames. Blocking ignored.")
      blocking = FALSE
    }
  }

  #------------ Generate Responses -------------#
  #Variables used later: responses
  responses = matrix(ncol = nsim, nrow = nrow(ModelMatrix))
  if (blocking) {
    for (i in 1:nsim) {
      responses[, i] = rfunction(ModelMatrix, anticoef, generate_noise_block(noise = varianceratios, groups = blockgroups))
    }
  } else {
    responses = replicate(nsim, rfunction(ModelMatrix, anticoef, rep(0, nrow(ModelMatrix))))
  }

  #-------Update formula with random blocks------#
  #Variables used later: model, model_formula
  if (blocking) {
    genBlockIndicators = function(blockgroup) rep(1:length(blockgroup), blockgroup)
    blockindicators = lapply(blockgroups, genBlockIndicators)
    randomeffects = c()
    for (i in 1:(length(blockgroups) - 1)) {
      RunMatrixReduced[paste("skprBlock", i, sep = "")] = blockindicators[[i]]
      randomeffects = c(randomeffects, paste("( 1 | skprBlock", i, " )", sep = ""))
    }
    randomeffects = paste(randomeffects, collapse = " + ")
    blockform = paste("~. + ", randomeffects, sep = "")
    #Adding random block variables to formula
    model = update.formula(model, blockform)
    if(nointercept) {
      model = update.formula(model, ~-1 + .)
    }
  } else {
    V = diag(nrow(run_matrix_processed))
  }

  model_formula = update.formula(model, Y ~ .)
  RunMatrixReduced$Y = 1
  #------------- Effect Power Settings ------------#

  if (!is.null(advancedoptions$anovatest)) {
    anovatest = advancedoptions$anovatest
    if (anovatest == "F") {
      pvalstring = "Pr(>F)"
    } else {
      pvalstring = "Pr(>Chisq)"
    }
  } else {
    anovatest = "Wald"
    pvalstring = "Pr(>Chisq)"
  }
  if (!is.null(advancedoptions$anovatype)) {
    anovatype = advancedoptions$anovatype
  } else {
    anovatype = "III"
  }

  #---------------- Run Simulations ---------------#

  progressbarupdates = floor(seq(1, nsim, length.out = 50))
  progresscurrent = 1
  estimates = matrix(0, nrow = nsim, ncol = ncol(ModelMatrix))
  if (!parallel) {
    pvallist = list()
    effectpvallist = list()
    stderrlist = list()
    iterlist = list()
    power_values = rep(0, ncol(ModelMatrix))
    effect_power_values = c()
    for (j in 1:nsim) {
      if (!is.null(progressBarUpdater)) {
        if (nsim > 50) {
          if (progressbarupdates[progresscurrent] == j) {
            progressBarUpdater(1 / 50)
            progresscurrent = progresscurrent + 1
          }
        } else {
          progressBarUpdater(1 / nsim)
        }
      }

      #simulate the data.
      RunMatrixReduced$Y = responses[, j]
      if (blocking) {
        if (glmfamilyname == "gaussian") {
          fit = suppressWarnings(lmerTest::lmer(model_formula, data = RunMatrixReduced, contrasts = contrastslist))
          if (calceffect) {
            effect_pvals = effectpowermc(fit, type = anovatype, test = "Pr(>Chisq)")
          }
        } else {
          fit = lme4::glmer(model_formula, data = RunMatrixReduced, family = glmfamily, contrasts = contrastslist)
          if (calceffect) {
            effect_pvals = effectpowermc(fit, type = anovatype, test = pvalstring, test.statistic = anovatest)
          }
        }
        estimates[j, ] = coef(summary(fit))[, 1]
      } else {
        if (glmfamilyname == "gaussian") {
          fit = lm(model_formula, data = RunMatrixReduced, contrasts = contrastslist)
          if (calceffect) {
            effect_pvals = effectpowermc(fit, type = anovatype, test = "Pr(>F)")
          }

        } else {
          fit = glm(model_formula, family = glmfamily, data = RunMatrixReduced, contrasts = contrastslist)
          if (calceffect) {
            effect_pvals = effectpowermc(fit, type = anovatype, test = pvalstring, test.statistic = anovatest)
          }
        }
        estimates[j, ] = coef(fit)
      }
      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = extractPvalues(fit)
      pvallist[[j]] = pvals
      if (j == 1 && calceffect) {
        effect_power_values = c(effect_power_values, rep(0, length(effect_pvals)))
      }
      stderrlist[[j]] = coef(summary(fit))[, 2]
      if (!blocking) {
        iterlist[[j]] = fit$iter
      } else {
        iterlist[[j]] = NA
      }
      if (calceffect) {
        effectpvallist[[j]] = effect_pvals
        effect_power_values[effect_pvals < alpha] = effect_power_values[effect_pvals < alpha] + 1
      }
      power_values[pvals < alpha] = power_values[pvals < alpha] + 1
    }
    #We are going to output a tidy data.frame with the results.
    attr(power_values, "pvals") = do.call(rbind, pvallist)
    if (calceffect) {
      attr(power_values, "effect_pvals") = do.call(rbind, effectpvallist)
    }
    attr(power_values, "stderrors") = do.call(rbind, stderrlist)
    attr(power_values, "fisheriterations") = do.call(rbind, iterlist)
    power_values = power_values / nsim
    if (calceffect) {
      effect_power_values = effect_power_values / nsim
      names(effect_power_values) = names(effectpvallist[[1]])
    }
  } else {
    if (is.null(options("cores")[[1]])) {
      numbercores = parallel::detectCores()
    } else {
      numbercores = options("cores")[[1]]
    }
    cl = parallel::makeCluster(numbercores)
    doParallel::registerDoParallel(cl, cores = numbercores)
    power_estimates = foreach::foreach (j = 1:nsim, .combine = "rbind", .export = c("extractPvalues", "effectpowermc"), .packages = c("lme4","lmerTest")) %dopar% {
      #simulate the data.
      RunMatrixReduced$Y = responses[, j]
      if (blocking) {
        if (glmfamilyname == "gaussian") {
          fit = suppressWarnings(lmerTest::lmer(model_formula, data = RunMatrixReduced, contrasts = contrastslist))
          if (calceffect) {
            effect_pvals = effectpowermc(fit, type = "III", test = "Pr(>Chisq)")
          }
        } else {
          fit = lme4::glmer(model_formula, data = RunMatrixReduced, family = glmfamily, contrasts = contrastslist)
          if (calceffect) {
            effect_pvals = effectpowermc(fit, type = "III", test = "Pr(>Chisq)")
          }
        }
        estimates = coef(summary(fit))[, 1]
      } else {
        if (glmfamilyname == "gaussian") {
          fit = lm(model_formula, data = RunMatrixReduced, contrasts = contrastslist)
          if (calceffect) {
            effect_pvals = effectpowermc(fit, type = "III", test = "Pr(>F)")
          }
        } else {
          fit = glm(model_formula, family = glmfamily, data = RunMatrixReduced, contrasts = contrastslist)
          if (calceffect) {
            effect_pvals = effectpowermc(fit, type = "III", test = "Pr(>Chisq)", test.statistic = "Wald")
          }
        }
        estimates = coef(fit)
      }
      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = extractPvalues(fit)
      power_values = rep(0, length(pvals))
      if (calceffect) {
        effect_power_values = rep(0, length(effect_pvals))
        names(effect_power_values) = names(effect_pvals)
        effect_power_values[effect_pvals < alpha] = 1
      }
      stderrval = coef(summary(fit))[, 2]
      power_values[pvals < alpha] = 1
      if (!blocking && !is.null(fit$iter)) {
        iterval = fit$iter
      } else {
        iterval = NA
      }
      if (calceffect) {
        list("parameterpower" = power_values, "effectpower" = effect_power_values, "estimates" = estimates, "pvals" = c(pvals), "strerrval" = stderrval, "iterval" = iterval)
      } else {
        list("parameterpower" = power_values, "estimates" = estimates, "pvals" = pvals, "strerrval" = stderrval, "iterval" = iterval)
      }
    }
    parallel::stopCluster(cl)
    power_values = apply(do.call(rbind, power_estimates[, "parameterpower"]), 2, sum) / nsim
    if (calceffect) {
      effect_power_values = apply(do.call(rbind, power_estimates[, "effectpower"]), 2, sum) / nsim
    }
    estimates = do.call(rbind, power_estimates[, "estimates"])
    attr(power_values, "pvals") = do.call(rbind, power_estimates[, "pvals"])
    attr(power_values, "stderrors") = do.call(rbind, power_estimates[, "strerrval"])
    attr(power_values, "fisheriterations") = do.call(rbind, power_estimates[, "iterval"])
  }
  #output the results (tidy data format)
  if (calceffect) {
    retval = data.frame(parameter = c(names(effect_power_values), parameter_names),
                        type = c(rep("effect.power.mc", length(effect_power_values)), rep("parameter.power.mc", length(parameter_names))),
                        power = c(effect_power_values, power_values))
  } else {
    retval = data.frame(parameter = parameter_names,
                        type = rep("parameter.power.mc", length(parameter_names)),
                        power = power_values)
  }
  attr(retval, "modelmatrix") = ModelMatrix
  attr(retval, "anticoef") = anticoef

  levelvector = sapply(lapply(RunMatrixReduced, unique), length)
  classvector = sapply(lapply(RunMatrixReduced, unique), class) == "factor"
  mm = gen_momentsmatrix(colnames(ModelMatrix), levelvector, classvector)

  if (glmfamilyname == "binomial") {
    pvalmat = attr(power_values, "pvals")
    likelyseparation = FALSE
    for (i in 2:ncol(pvalmat)) {
      pvalcount = hist(pvalmat[, i], breaks = seq(0, 1, 0.05), plot = FALSE)
      likelyseparation = likelyseparation || (all(pvalcount$count[20] > pvalcount$count[17:19]) && pvalcount$count[20] > nsim / 15)
    }
    if (likelyseparation && !advancedoptions$GUI) {
      warning("Partial or complete separation likely detected in the binomial Monte Carlo simulation. Increase the number of runs in the design or decrease the number of model parameters to improve power.")
    }
  }

  modelmatrix_cor = model.matrix(generatingmodel, RunMatrixReduced, contrasts.arg = contrastslist_cormat)
  if (ncol(modelmatrix_cor) > 2) {
    tryCatch({
      if ("(Intercept)" %in% colnames(modelmatrix_cor)) {
        correlation.matrix = abs(cov2cor(solve(t(modelmatrix_cor) %*% solve(V) %*% modelmatrix_cor))[-1, -1])
        colnames(correlation.matrix) = colnames(modelmatrix_cor)[-1]
        rownames(correlation.matrix) = colnames(modelmatrix_cor)[-1]
      } else {
        correlation.matrix = abs(cov2cor(solve(t(modelmatrix_cor) %*% solve(V) %*% modelmatrix_cor)))
        colnames(correlation.matrix) = colnames(modelmatrix_cor)
        rownames(correlation.matrix) = colnames(modelmatrix_cor)
      }
      attr(retval, "correlation.matrix") = round(correlation.matrix, 8)
    }, error = function(e) {})
  }

  if (detailedoutput) {
    if (nrow(retval) != length(anticoef)){
      retval$anticoef = c(rep(NA, nrow(retval) - length(anticoef)), anticoef)
    } else {
      retval$anticoef = anticoef
    }
    retval$alpha = alpha
    if (is.character(glmfamilyname)) {
      retval$glmfamily = glmfamilyname
    } else { #user supplied a glm family object
      retval$glmfamily = paste(glmfamilyname, collapse = " ")
    }
    retval$trials = nrow(run_matrix_processed)
    retval$nsim = nsim
    retval$blocking = blocking
  }

  colnames(estimates) = parameter_names
  if (!blocking) {
    attr(retval, "variance.matrix") = diag(nrow(modelmatrix_cor))
    attr(retval, "I") = IOptimality(modelmatrix_cor, momentsMatrix = mm, blockedVar = diag(nrow(modelmatrix_cor)))
    attr(retval, "D") = 100 * DOptimality(modelmatrix_cor) ^ (1 / ncol(modelmatrix_cor)) / nrow(modelmatrix_cor)
  } else {
    attr(retval, "variance.matrix") = V
    attr(retval, "I") = IOptimality(modelmatrix_cor, momentsMatrix = mm, blockedVar = V)
    attr(retval, "D") = 100 * DOptimalityBlocked(modelmatrix_cor, blockedVar = V) ^ (1 / ncol(modelmatrix_cor)) / nrow(modelmatrix_cor)
  }
  attr(retval, "generating.model") = generatingmodel
  attr(retval, "runmatrix") = RunMatrixReduced
  attr(retval, "variance.matrix") = V
  attr(retval, "estimates") = estimates
  attr(retval, "pvals") = attr(power_values, "pvals")
  attr(retval, "stderrors") = attr(power_values, "stderrors")
  attr(retval, "fisheriterations") = attr(power_values, "fisheriterations")

  return(retval)
}
globalVariables("i")
