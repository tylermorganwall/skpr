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
#'@param model The model used in evaluating the design. If this is missing and the design
#'was generated with skpr, the generating model will be used. It can be a subset of the model used to
#'generate the design, or include higher order effects not in the original design generation. It cannot include
#'factors that are not present in the experimental design.
#'@param alpha Default `0.05`. The type-I error. p-values less than this will be counted as significant.
#'@param blocking Default `NULL`. If `TRUE`, \code{eval_design_mc} will look at the rownames (or blocking columns) to determine blocking structure. Default FALSE.
#'@param nsim Default `1000`. The number of Monte Carlo simulations to perform.
#'@param glmfamily Default `gaussian`. String indicating the family of distribution for the `glm` function
#'("gaussian", "binomial", "poisson", or "exponential").
#'@param calceffect Default `TRUE`. Whether to calculate effect power. This calculation is more expensive than parameter power,
#'so turned off (if not needed) can greatly speed up calculation time.
#'@param effect_anova Default `TRUE`, whether to a Type-III Anova or a likelihood ratio test to calculate effect power.
#'If `TRUE`, effect power will be calculated using a Type-III Anova (using the car package) and a Wald test. If `FALSE`,
#'a likelihood ratio test (using a reduced model for each effect) will performed using the `lmtest` package. If `firth = TRUE`,
#'this will be set to `FALSE` automatically.
#'@param varianceratios Default `NULL`. The ratio of the whole plot variance to the run-to-run variance.
#'If not specified during design generation, this will default to 1. For designs with more than one subplot
#'this ratio can be a vector specifying the variance ratio for each subplot (comparing to the run-to-run variance).
#'Otherwise, it will use a single value for all strata.
#'@param rfunction Default `NULL`.Random number generator function for the response variable. Should be a function of the form f(X, b, delta), where X is the
#'model matrix, b are the anticipated coefficients, and delta is a vector of blocking errors. Typically something like rnorm(nrow(X), X * b + delta, 1).
#'You only need to specify this if you do not like the default behavior described below.
#'@param anticoef Default `NULL`.The anticipated coefficients for calculating the power. If missing, coefficients
#'will be automatically generated based on the \code{effectsize} argument.
#'@param firth Default `FALSE`. Whether to apply the firth correction (via the `mbest` package) to a logistic regression. This
#'setting also automatically sets `effect_lr = TRUE`.
#'@param effectsize Helper argument to generate anticipated coefficients. See details for more info.
#'If you specify \code{anticoef}, \code{effectsize} will be ignored.
#'@param contrasts Default \code{contr.sum}. The contrasts to use for categorical factors. If the user has specified their own contrasts
#'for a categorical factor using the contrasts function, those will be used. Otherwise, skpr will use contr.sum.
#' If the user wants to set the number of cores manually, they can do this by setting `options("cores")` to the desired number (e.g. `options("cores" = parallel::detectCores())`).
#' NOTE: If you have installed BLAS libraries that include multicore support (e.g. Intel MKL that comes with Microsoft R Open), turning on parallel could result in reduced performance.
#'@param adjust_alpha_inflation Default `FALSE`. If `TRUE`, this will run the simulation twice:
#'first to calculate the empirical distribution of p-values under the null hypothesis and find
#'the true Type-I error cutoff that corresponds to the desired Type-I error rate,
#'and then again given effect size to calculate power values.
#'@param detailedoutput Default `FALSE`. If `TRUE`, return additional information about evaluation in results.
#'@param progress Default `TRUE`. Whether to include a progress bar.
#'@param advancedoptions Default `NULL`. Named list of advanced options. `advancedoptions$anovatype` specifies the Anova type in the car package (default type `III`),
#'user can change to type `II`). `advancedoptions$anovatest` specifies the test statistic if the user does not want a `Wald` test--other options are likelyhood-ratio `LR` and F-test `F`.
#'`advancedoptions$progressBarUpdater` is a function called in non-parallel simulations that can be used to update external progress bar.`advancedoptions$GUI` turns off some warning messages when in the GUI.
#'If `advancedoptions$save_simulated_responses = TRUE`, the dataframe will have an attribute `simulated_responses` that contains the simulated responses from the power evaluation.
#'@param parallel Default `FALSE`. If `TRUE`, the Monte Carlo power calculation will use all but one of the available cores. If the user wants to set the number of cores manually, they can do this by setting `options("cores")` to the desired number (e.g. `options("cores" = parallel::detectCores())`).
#' NOTE: If you have installed BLAS libraries that include multicore support (e.g. Intel MKL that comes with Microsoft R Open), turning on parallel could result in reduced performance.
#'@param ... Additional arguments.
#'@return A data frame consisting of the parameters and their powers, with supplementary information
#'stored in the data frame's attributes. The parameter estimates from the simulations are stored in the "estimates"
#' attribute. The "modelmatrix" attribute contains the model matrix that was used for power evaluation, and
#' also provides the encoding used for categorical factors. If you want to specify the anticipated
#' coefficients manually, do so in the order the parameters appear in the model matrix.
#'@details Evaluates the power of a design with Monte Carlo simulation. Data is simulated and then fit
#' with a generalized linear model, and the fraction of simulations in which a parameter
#' is significant (its p-value, according to the fit function used, is less than the specified \code{alpha})
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
#'@import foreach doParallel stats doRNG
#'@examples #We first generate a full factorial design using expand.grid:
#'factorialcoffee = expand.grid(cost = c(-1, 1),
#'                               type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
#'                               size = as.factor(c("Short", "Grande", "Venti")))
#'if(skpr:::run_documentation()) {
#'#And then generate the 21-run D-optimal design using gen_design.
#'designcoffee = gen_design(factorialcoffee,
#'                          model = ~cost + type + size, trials = 21, optimality = "D")
#'}
#'if(skpr:::run_documentation()) {
#'#To evaluate this design using a normal approximation, we just use eval_design
#'#(here using the default settings for contrasts, effectsize, and the anticipated coefficients):
#'
#'eval_design(design = designcoffee, model = ~cost + type + size, 0.05)
#'}
#'if(skpr:::run_documentation()) {
#'#To evaluate this design with a Monte Carlo method, we enter the same information
#'#used in eval_design, with the addition of the number of simulations "nsim" and the distribution
#'#family used in fitting for the glm "glmfamily". For gaussian, binomial, exponential, and poisson
#'#families, a default random generating function (rfunction) will be supplied. If another glm
#'#family is used or the default random generating function is not adequate, a custom generating
#'#function can be supplied by the user. Like in `eval_design()`, if the model isn't entered, the
#'#model used in generating the design will be used.
#'
#'eval_design_mc(designcoffee, nsim = 100, glmfamily = "gaussian")
#'}
#'if(skpr:::run_documentation()) {
#'#We see here we generate approximately the same parameter powers as we do
#'#using the normal approximation in eval_design. Like eval_design, we can also change
#'#effectsize to produce a different signal-to-noise ratio:
#'
#'eval_design_mc(design = designcoffee, nsim = 100,
#'                        glmfamily = "gaussian", effectsize = 1)
#'}
#'if(skpr:::run_documentation()) {
#'#Like eval_design, we can also evaluate the design with a different model than
#'#the one that generated the design.
#'eval_design_mc(design = designcoffee, model = ~cost + type, alpha = 0.05,
#'               nsim = 100, glmfamily = "gaussian")
#'}
#'if(skpr:::run_documentation()) {
#'#And here it is evaluated with additional interactions included:
#'eval_design_mc(design = designcoffee, model = ~cost + type + size + cost * type, 0.05,
#'               nsim = 100, glmfamily = "gaussian")
#'}
#'if(skpr:::run_documentation()) {
#'#We can also set "parallel = TRUE" to use all the cores available to speed up
#'#computation.
#'eval_design_mc(design = designcoffee, nsim = 10000,
#'                        glmfamily = "gaussian", parallel = TRUE)
#'}
#'if(skpr:::run_documentation()) {
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
#'                        splitplotdesign = vhtcdesign, blocksizes = rep(3, 6), varianceratio = 1)
#'splitplotdesign = gen_design(factorialcoffee2,
#'                             model = ~Store + Temp + cost + type + size, trials = 54,
#'                             splitplotdesign = htcdesign, blocksizes = rep(3, 18),
#'                             varianceratio = 1)
#'
#'#Each block has an additional noise term associated with it in addition to the normal error
#'#term in the model. This is specified by a vector specifying the additional variance for
#'#each split-plot level. This is equivalent to specifying a variance ratio of one between
#'#the whole plots and the run-to-run variance for gaussian models.
#'
#'#Evaluate the design. Note the decreased power for the blocking factors.
#'eval_design_mc(splitplotdesign, blocking = TRUE, nsim = 100,
#'                        glmfamily = "gaussian", varianceratios = c(1, 1, 1))
#'}
#'if(skpr:::run_documentation()) {
#'#We can also use this method to evaluate designs that cannot be easily
#'#evaluated using normal approximations. Here, we evaluate a design with a binomial response and see
#'#whether we can detect the difference between each factor changing whether an event occurs
#'#70% of the time or 90% of the time.
#'
#'factorialbinom = expand.grid(a = c(-1, 1), b = c(-1, 1))
#'designbinom = gen_design(factorialbinom, model = ~a + b, trials = 90, optimality = "D")
#'
#'eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, effectsize = c(0.7, 0.9),
#'               glmfamily = "binomial")
#'}
#'if(skpr:::run_documentation()) {
#'#We can also use this method to determine power for poisson response variables.
#'#Generate the design:
#'
#'factorialpois = expand.grid(a = as.numeric(c(-1, 0, 1)), b = c(-1, 0, 1))
#'designpois = gen_design(factorialpois, ~a + b, trials = 70, optimality = "D")
#'
#'#Evaluate the power:
#'
#'eval_design_mc(designpois, ~a + b, 0.05, nsim = 100, glmfamily = "poisson",
#'                anticoef = log(c(0.2, 2, 2)))
#'}
#'
#'#The coefficients above set the nominal value -- that is, the expected count
#'#when all inputs = 0 -- to 0.2 (from the intercept), and say that each factor
#'#changes this count by a factor of 4 (multiplied by 2 when x= +1, and divided by 2 when x = -1).
#'#Note the use of log() in the anticipated coefficients.
eval_design_mc = function(design, model = NULL, alpha = 0.05,
                          blocking = NULL, nsim = 1000, glmfamily = "gaussian",
                          calceffect = TRUE, effect_anova = TRUE,
                          varianceratios = NULL, rfunction = NULL, anticoef = NULL, firth = FALSE,
                          effectsize = 2, contrasts = contr.sum, parallel = FALSE,
                          adjust_alpha_inflation = FALSE,
                          detailedoutput = FALSE, progress = TRUE, advancedoptions = NULL, ...) {
  if(!firth || glmfamily != "binomial") {
    method = "glm.fit"
  } else {
    if(!(length(find.package("mbest", quiet = TRUE)) > 0)) {
      stop("skpr: Firth correction requires installation of the `mbest` package.")
    }
    method = mbest::firthglm.fit
  }
  if(missing(design)) {
    stop("skpr: No design detected in arguments.")
  }
  if(missing(model) || (is.numeric(model) && missing(alpha))) {
    if(is.numeric(model) && missing(alpha)) {
      alpha = model
    }
    if(is.null(attr(design,"generating.model"))) {
      stop("skpr: No model detected in arguments or in design attributes.")
    } else {
      model = attr(design,"generating.model")
    }
  }
  user_specified_varianceratio = TRUE
  if(is.null(varianceratios)) {
    user_specified_varianceratio = FALSE
    if(!is.null(attr(design, "varianceratios"))) {
      varianceratios = attr(design, "varianceratios")
    } else {
      varianceratios = 1
    }
  }
  if(!is.null(attr(design,"splitcolumns"))) {
    if(varianceratios[length(varianceratios)] != 1 && !user_specified_varianceratio) {
      warning("Lowest level of varianceratios cannot be set to anything other than 1 (value of ",
              varianceratios[length(varianceratios)],
              " was set during design generation). Setting run-to-run variance to 1.")
      varianceratios[length(varianceratios)] = 1
    }
  }
  args = list(...)
  if ("RunMatrix" %in% names(args)) {
    stop("skpr: RunMatrix argument deprecated. Use `design` instead.")
  }

  if(is.null(blocking)) {
    blocking = FALSE
    if(!is.null(attr(design,"blocking"))) {
      blocking = attr(design,"blocking")
    }
    if(!is.null(attr(design,"splitplot"))) {
      blocking = blocking || attr(design,"splitplot")
    }
  }

  if (!is.null(advancedoptions)) {
    if(is.null(advancedoptions$save_simulated_responses)) {
      advancedoptions$save_simulated_responses = FALSE
    }
    if(!is.null(advancedoptions$progress_msg)) {
      progress_message = advancedoptions$progress_msg
    } else {
      progress_message = "Power"
    }
    if (is.null(advancedoptions$GUI)) {
      advancedoptions$GUI = FALSE
    }
    if (!is.null(advancedoptions$progressBarUpdater)) {
      progressBarUpdater = advancedoptions$progressBarUpdater
    } else {
      progressBarUpdater = NULL
    }
  } else {
    progress_message =  "Power"
    advancedoptions = list()
    advancedoptions$GUI = FALSE
    progressBarUpdater = NULL
    advancedoptions$save_simulated_responses = FALSE
    advancedoptions$aliaspower = 2
  }
  if(!advancedoptions$GUI) {
    progress = getOption("skpr_progress", progress)
  }
  if(is.null(advancedoptions$aliaspower)) {
    aliaspower = 2
  } else {
    if(!is.numeric(advancedoptions$aliaspower)) {
      stop("skpr: advancedoptions$aliaspower must be a positive integer")
    }
    aliaspower = advancedoptions$aliaspower
  }
  alpha_adjust = FALSE
  if (adjust_alpha_inflation) {
    alpha_adjust = TRUE
    adjust_alpha_inflation = FALSE
    if (is.null(advancedoptions$alphanull)) {
      effectsizetemp = c(effectsize[1], effectsize[1])
    } else {
      effectsizetemp = advancedoptions$alphanull
    }
    advancedoptions$progress_msg = "Type-I Error"
    nullresults = eval_design_mc(design = design, model = model, alpha = alpha,
                   blocking = blocking, nsim = nsim, glmfamily = glmfamily,
                   calceffect = calceffect, effect_anova = effect_anova, adjust_alpha_inflation = FALSE,
                   varianceratios = varianceratios, rfunction = rfunction, anticoef = anticoef, firth = firth,
                   effectsize = effectsizetemp, contrasts = contrasts, parallel = parallel,
                   detailedoutput = detailedoutput, advancedoptions = advancedoptions, ...)
    if (attr(terms.formula(model, data = design), "intercept") == 1) {
      alpha_parameter = c(alpha, apply(attr(nullresults, "pvals"), 2, quantile, probs = alpha)[-1])
      alpha_parameter[alpha_parameter > alpha] = alpha
      if (calceffect) {
        if(nullresults$parameter[1] == "(Intercept)") {
          alpha_effect = c(alpha, apply(attr(nullresults, "effect_pvals"), 2, quantile, probs = alpha)[-1])
        } else {
          alpha_effect = apply(attr(nullresults, "effect_pvals"), 2, quantile, probs = alpha)
        }
        alpha_effect[alpha_effect > alpha] = alpha
      }
    } else {
      alpha_parameter = apply(attr(nullresults, "pvals"), 2, quantile, probs = alpha)
      alpha_parameter[alpha_parameter > alpha] = alpha
      if (calceffect) {
        alpha_effect = apply(attr(nullresults, "effect_pvals"), 2, quantile, probs = alpha)
        alpha_effect[alpha_effect > alpha] = alpha
      }
    }
  } else {
    alpha_effect = alpha
    alpha_parameter = alpha
  }
  if (attr(terms.formula(model, data = design), "intercept") == 1) {
    nointercept = FALSE
  } else {
    nointercept = TRUE
  }

  #detect pre-set contrasts
  presetcontrasts = list()
  for (x in names(design)[lapply(design, class) %in% c("character", "factor")]) {
    if (!is.null(attr(design[[x]], "contrasts"))) {
      presetcontrasts[[x]] = attr(design[[x]], "contrasts")
    }
  }

  #covert tibbles
  run_matrix_processed = as.data.frame(design)
  #Detect externally generated blocking columns and convert to rownames
  run_matrix_processed = convert_blockcolumn_rownames(run_matrix_processed, blocking, varianceratios)
  zlist = attr(run_matrix_processed, "z.matrix.list")

  #Remove skpr-generated REML blocking indicators if present
  run_matrix_processed = remove_skpr_blockcols(run_matrix_processed)

  #----- Convert dots in formula to terms -----#
  model = convert_model_dots(run_matrix_processed, model)

  #----- Rearrange formula terms by order -----#
  model = rearrange_formula_by_order(model, data = run_matrix_processed)
  if(nointercept) {
    model = update.formula(model, ~-1 + .)
  }

  glmfamilyname = tolower(glmfamily)

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
  run_matrix_processed = normalize_design(run_matrix_processed)

  #---------- Generating model matrix ----------#
  #Remove columns from variables not used in the model
  #Variables used later: contrastslist, contrastslist_cormat
  RunMatrixReduced = reduceRunMatrix(run_matrix_processed, model)

  contrastslist_cormat = list()
  contrastslist = list()
  for (x in names(RunMatrixReduced)[lapply(RunMatrixReduced, class) %in% c("character", "factor")]) {
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
    stop("skpr: Wrong number of anticipated coefficients")
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
      if (length(blockgroups) == 1 | is.matrix(blockgroups)) {
        stop("skpr: No blocking detected. Specify block structure in row names or set blocking = FALSE")
      }
      if (length(blockgroups) != length(varianceratios) && length(varianceratios) == 1) {
        # warning("Single varianceratio entered for multiple layers. Setting all but the run-to-run varianceratio to that level.")
        varianceratios = c(rep(varianceratios,length(blockgroups)-1),1)
      }
      if (length(blockgroups) - 1 == length(varianceratios)) {
        varianceratios = c(varianceratios,1)
      }
      if (length(blockgroups) != length(varianceratios)) {
        stop("skpr: Wrong number of variance ratios specified. ", length(varianceratios),
             " variance ratios given c(",paste(varianceratios,collapse=", "), "), ", length(blockgroups), " expected. Either specify value for all blocking levels or one ratio for all blocks other than then run-to-run variance.")
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
    if(blocking) {
      anovatest = "Chisq"
    } else {
      anovatest = "Wald"
    }
    pvalstring = "Pr(>Chisq)"
  }
  if (!is.null(advancedoptions$anovatype)) {
    anovatype = advancedoptions$anovatype
  } else {
    anovatype = "III"
  }
  #-------------- -------------#
  if(effect_anova && firth && glmfamilyname == "binomial" && !alpha_adjust) {
    warning(r"(skpr uses a likelihood ratio test (instead of a type-III ANOVA) for",
      "effect power when `firth = TRUE` and `glmfamily = "binomial"`: setting `effect_lr = TRUE`.)")
  }
  fterms = terms.formula(model_formula)
  term_order = attr(fterms,"order")
  if(min(term_order) != 1) {
    stop("skpr: No main effect terms found--this model will not produce well-formed power estimates.")
  }

  #---------------- Run Simulations ---------------#
  aliasing_checked = FALSE
  num_updates = min(c(nsim,200))
  progressbarupdates = floor(seq(1, nsim, length.out = num_updates))
  progresscurrent = 1
  estimates = matrix(0, nrow = nsim, ncol = ncol(ModelMatrix))
  effect_terms = c(1,rownames(attr(terms(model_formula), "factors"))[-1])
  if (!parallel) {
    pvallist = list()
    effectpvallist = list()
    stderrlist = list()
    iterlist = list()
    if(interactive() && progress) {
      pb = progress::progress_bar$new(format = sprintf("  Calculating %s [:bar] (:current/:total, :tick_rate sim/s) ETA: :eta", progress_message),
                                      total = nsim, clear = TRUE, width= 100)
    }
    power_values = rep(0, ncol(ModelMatrix))
    effect_power_values = c()
    for (j in seq_len(nsim)) {
      if (advancedoptions$GUI) {
        #This code is to slow down the number of updates in the Shiny app--if there
        #are too many updates, the progress bar will lag behind the actual computation
        if (j %in% progressbarupdates) {
          progressBarUpdater(1 / num_updates)
        }
      }
      fiterror = FALSE
      #simulate the data.
      RunMatrixReduced$Y = responses[, j]
      if (blocking) {
        if (glmfamilyname == "gaussian") {
          fit = suppressWarnings(
            suppressMessages(
              lmerTest::lmer(model_formula, data = RunMatrixReduced, contrasts = contrastslist)
            )
          )
          if (calceffect) {
            effect_pvals = effectpowermc(fit, type = anovatype, test = "Pr(>Chisq)",
                                         model_formula = model_formula, firth = firth,
                                         glmfamily = glmfamilyname, effect_terms = effect_terms,
                                         RunMatrixReduced = RunMatrixReduced, method = method,
                                         contrastslist = contrastslist, effect_anova = effect_anova)
          }
        } else {
          tryCatch({
            fit = suppressWarnings(
              suppressMessages(
                lme4::glmer(model_formula, data = RunMatrixReduced, family = glmfamily, contrasts = contrastslist)
              )
            )
          }, error = function(e) {
            fiterror = TRUE
          })
          if (calceffect && !fiterror) {
            effect_pvals = effectpowermc(fit, type = anovatype,
                                         test = pvalstring,
                                         test.statistic = anovatest,
                                         model_formula = model_formula, firth = firth,
                                         glmfamily = glmfamilyname, effect_terms = effect_terms,
                                         RunMatrixReduced = RunMatrixReduced, method = method,
                                         contrastslist = contrastslist, effect_anova = effect_anova)
          }
        }
        if(!fiterror) {
          estimates[j, ] = suppressWarnings(
            suppressMessages(
              coef(summary(fit))[, 1]
            )
          )
        } else {
          estimates[j, ] = NA
        }
      } else {
        if (glmfamilyname == "gaussian") {
          fit = lm(model_formula, data = RunMatrixReduced, contrasts = contrastslist)
          if (calceffect) {
            effect_pvals = effectpowermc(fit, type = anovatype, test = "Pr(>F)", test.statistic = anovatest,
                                         model_formula = model_formula, firth = firth,
                                         glmfamily = glmfamilyname, effect_terms = effect_terms,
                                         RunMatrixReduced = RunMatrixReduced, method = method,
                                         contrastslist = contrastslist, effect_anova = effect_anova)
          }
        } else {
          tryCatch({
            fit = suppressWarnings(suppressMessages({
              glm(model_formula, family = glmfamily, data = RunMatrixReduced, contrasts = contrastslist, method = method)
            }))
          }, error = function(e) {
            fiterror = TRUE
          })
          if (calceffect && !fiterror) {
            effect_pvals = effectpowermc(fit, type = anovatype, test = pvalstring, test.statistic = anovatest,
                                         model_formula = model_formula, firth = firth,
                                         glmfamily = glmfamilyname, effect_terms = effect_terms,
                                         RunMatrixReduced = RunMatrixReduced, method = method,
                                         contrastslist = contrastslist, effect_anova = effect_anova,
                                         model_matrix = ModelMatrix)
          }
        }
        if(!fiterror) {
          estimates[j, ] = suppressWarnings(suppressMessages(coef(fit)))
        } else {
          estimates[j, ] = NA
        }
      }
      if(!fiterror) {
        #Check for perfect aliasing in design
        if(!aliasing_checked) {
          aliasing_checked = TRUE
          if(!inherits(fit, c("lmerMod","glmerMod","merMod")) && !is.null(alias(fit)$Complete)) {
            alias_mat_fit = alias(fit)$Complete
            if(nrow(alias_mat_fit) > 0) {
              perfectly_aliased_terms = paste0(rownames(alias_mat_fit), collapse = ", ")
              stop(sprintf("Perfectly aliased term(s) included in model (%s). Remove these terms to fit your model, or change your design.",
                           perfectly_aliased_terms))
            }
          }
        }
        #determine whether beta[i] is significant. If so, increment nsignificant
        pvals = suppressWarnings(extractPvalues(fit, glmfamily = glmfamilyname))
        #reorder since firth correction can change the ordering
        pvals = pvals[order(factor(names(pvals), levels = parameter_names))]
        stopifnot(all(names(pvals) == parameter_names))
        pvallist[[j]] = pvals
        if (length(effect_power_values) == 0 && calceffect && !fiterror) {
          effect_power_values = c(effect_power_values, rep(0, length(effect_pvals)))
        }
        stderrlist[[j]] = suppressWarnings(coef(summary(fit))[, 2])
        if (!blocking) {
          iterlist[[j]] = fit$iter
        } else {
          iterlist[[j]] = NA
        }
        if (calceffect && !fiterror) {
          effectpvallist[[j]] = effect_pvals
          effect_pvals[is.na(effect_pvals)] = 1
          effect_power_values[effect_pvals < alpha_effect] = effect_power_values[effect_pvals < alpha_effect] + 1
        }
        power_values[pvals < alpha_parameter] = power_values[pvals < alpha_parameter] + 1
      }
      if(interactive() && progress && !advancedoptions$GUI) {
        pb$tick()
      }
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
      if(length(effectpvallist) > 1) {
        if(!isTRUE(do.call("all.equal",lapply(effectpvallist,names)))) {
          stop("effect p-value names shifted during computation, results are not valid: contact the developer")
        }
      }
      names(effect_power_values) = names(effectpvallist[[1]])
    }
  } else {
    if(!progress) {
      progressbarupdates = c()
    }
    if(!advancedoptions$GUI && progress) {
      set_up_progressr_handler(sprintf("Evaluating %s",progress_message), "sims")
    }
    modelmat = model.matrix(model_formula, data=RunMatrixReduced,contrasts = contrastslist)
    packagelist = c()
    if(firth) {
      packagelist = "mbest"
    }
    nc =  future::nbrOfWorkers()
    run_search = function(iterations, is_shiny) {
      prog = progressr::progressor(steps = nsim)
      foreach::foreach (j = seq_along(iterations), .combine = "rbind",
                        .options.future = list(packages = packagelist,
                                               globals = c("extractPvalues", "effectpowermc", "RunMatrixReduced", "is_shiny", "blocking",
                                                           "responses", "contrastslist", "model_formula", "glmfamily", "glmfamilyname", "calceffect",
                                                           "anovatype", "pvalstring", "anovatest", "firth", "effect_terms", "effect_anova", "method",
                                                           "modelmat", "aliasing_checked", "parameter_names", "progressbarupdates",
                                                           "alpha_parameter", "alpha_effect", "prog", "nsim", "num_updates", "nc"),
                                               seed = TRUE)) %dofuture% {
        if(j %in% progressbarupdates) {
          if(is_shiny) {
            prog(sprintf(" (%i workers) ", nc), amount = nsim/num_updates)
          } else {
            prog(amount = nsim/num_updates)
          }
        }
        #simulate the data.
        fiterror = FALSE
        RunMatrixReduced$Y = responses[, j]
        if (blocking) {
          if (glmfamilyname == "gaussian") {
            fit = suppressWarnings(
              suppressMessages(
                lmerTest::lmer(model_formula, data = RunMatrixReduced, contrasts = contrastslist)
              )
            )
            if (calceffect) {
              effect_pvals = effectpowermc(fit, type = "III",
                                           test = "Pr(>Chisq)")
            }
          } else {
            tryCatch({
              fit = suppressWarnings(
                suppressMessages(
                  lme4::glmer(model_formula, data = RunMatrixReduced, family = glmfamily, contrasts = contrastslist)
                )
              )
            }, error = function(e) {
              fiterror = TRUE
            })
            if (calceffect && !fiterror) {
              effect_pvals = effectpowermc(fit, type = anovatype,
                                           test = pvalstring,
                                           test.statistic = anovatest,
                                           model_formula = model_formula, firth = firth,
                                           glmfamily = glmfamilyname, effect_terms = effect_terms,
                                           RunMatrixReduced = RunMatrixReduced, method = method,
                                           contrastslist = contrastslist, effect_anova = effect_anova)
            }
          }
          if(!fiterror) {
            estimates = coef(summary(fit))[, 1]
          }
        } else {
          if (glmfamilyname == "gaussian") {
            fit = lm(model_formula, data = RunMatrixReduced, contrasts = contrastslist)
            if (calceffect) {
              effect_pvals = effectpowermc(fit, type = "III", test = "Pr(>F)", test.statistic = anovatest,
                                           model_formula = model_formula, firth = firth,
                                           glmfamily = glmfamilyname, effect_terms = effect_terms,
                                           RunMatrixReduced = RunMatrixReduced, method = method,
                                           contrastslist = contrastslist, effect_anova = effect_anova)
            }
          } else {
            tryCatch({
              fit = suppressWarnings(
                suppressMessages(
                  glm(model_formula, family = glmfamily, data = RunMatrixReduced, contrasts = contrastslist, method = method)
                )
              )
            }, error = function(e) {
              fiterror = TRUE
            })
            if (calceffect && !fiterror) {
              effect_pvals = effectpowermc(fit, type = anovatype, test = pvalstring, test.statistic = anovatest,
                                           model_formula = model_formula, firth = firth,
                                           glmfamily = glmfamilyname, effect_terms = effect_terms,
                                           RunMatrixReduced = RunMatrixReduced, method = method,
                                           contrastslist = contrastslist, effect_anova = effect_anova)
            }
          }
          if(!fiterror) {
            estimates = suppressWarnings(
              suppressMessages(coef(fit)
              ))
          } else {
            estimates = rep(NA,ncol(modelmat))
          }
        }
        if(!fiterror) {
          #Check for perfect aliasing in design
          if(!aliasing_checked) {
            aliasing_checked = TRUE
            if(!inherits(fit, c("lmerMod","glmerMod","merMod")) && !is.null(alias(fit)$Complete)) {
              alias_mat_fit = alias(fit)$Complete
              if(nrow(alias_mat_fit) > 0) {
                perfectly_aliased_terms = paste0(rownames(alias_mat_fit), collapse = ", ")
                stop(sprintf("Perfectly aliased term(s) included in model (%s). Remove these terms to fit your model, or change your design.",
                             perfectly_aliased_terms))
              }
            }
          }
          #determine whether beta[i] is significant. If so, increment nsignificant
          pvals = extractPvalues(fit, glmfamily = glmfamilyname)
          #reorder since firth correction can change the ordering
          pvals = pvals[order(factor(names(pvals), levels = parameter_names))]
          stopifnot(all(names(pvals) == parameter_names))
          power_values = rep(0, length(pvals))
          if (calceffect) {
            effect_power_values = rep(0, length(effect_pvals))
            names(effect_power_values) = names(effect_pvals)
            effect_power_values[effect_pvals < alpha_effect] = 1
          }
          stderrval = coef(summary(fit))[, 2]
          power_values[pvals < alpha_parameter] = 1
          if (!blocking && !is.null(fit$iter)) {
            iterval = fit$iter
          } else {
            iterval = NA
          }
          if (calceffect) {
            list("parameterpower" = power_values, "effectpower" = effect_power_values, "estimates" = estimates, "pvals" = c(pvals), "effectpvals" = effect_pvals, "strerrval" = stderrval, "iterval" = iterval)
          } else {
            list("parameterpower" = power_values, "estimates" = estimates, "pvals" = pvals, "strerrval" = stderrval, "iterval" = iterval)
          }
        }
      }
    }
    power_estimates = run_search(seq_len(nsim), advancedoptions$GUI)

    power_values = apply(do.call(rbind, power_estimates[, "parameterpower"]), 2, sum) / nsim
    if (calceffect) {
      effect_power_values = apply(do.call(rbind, power_estimates[, "effectpower"]), 2, sum) / nsim
      attr(power_values, "effect_pvals") = do.call(rbind, power_estimates[, "effectpvals"])
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
  attr(retval, "z.matrix.list") = zlist

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
      warning("skpr: Partial or complete separation likely detected in the binomial Monte Carlo simulation. Increase the number of runs in the design or decrease the number of model parameters to improve power.")
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
    tryCatch({
      if (ncol(attr(run_matrix_processed, "modelmatrix")) > 2) {
        amodel = aliasmodel(model, aliaspower)
        if (amodel != model) {
          aliasmatrix = suppressWarnings({
            model.matrix(aliasmodel(model, aliaspower), design, contrasts.arg = contrastslist_cormat)[, -1]
          })
          A = solve(t(modelmatrix_cor) %*% modelmatrix_cor) %*% t(modelmatrix_cor) %*% aliasmatrix
          attr(results, "alias.matrix") = A
          attr(results, "trA") = sum(diag(t(A) %*% A))
        } else {
          attr(results, "alias.matrix") = "No alias matrix calculated: full model specified"
          attr(results, "trA") = "No alias trace calculated: full model specified"
        }
      }
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
    if(calceffect && alpha_adjust) {
      retval$error_adjusted_alpha = c(alpha_effect, alpha_parameter)
    } else {
      retval$error_adjusted_alpha = alpha_parameter
    }
  }

  colnames(estimates) = parameter_names
  if (!blocking) {
    attr(retval, "variance.matrix") = diag(nrow(modelmatrix_cor))
    attr(retval, "I") = IOptimality(modelmatrix_cor, momentsMatrix = mm, blockedVar = diag(nrow(modelmatrix_cor)))
    attr(retval, "D") = 100 * DOptimalityLog(modelmatrix_cor)
  } else {
    attr(retval, "variance.matrix") = V
    attr(retval, "I") = IOptimality(modelmatrix_cor, momentsMatrix = mm, blockedVar = V)
    deffic = DOptimalityBlocked(modelmatrix_cor, blockedVar = V)
    if(!is.infinite(deffic)) {
      attr(retval, "D") =  100 * DOptimalityBlocked(modelmatrix_cor, blockedVar = V) ^ (1 / ncol(modelmatrix_cor)) / nrow(modelmatrix_cor)
    } else {
      attr(retval, "D") =  100 * DOptimalityBlockedLog(modelmatrix_cor, blockedVar = V) ^ (1 / ncol(modelmatrix_cor)) / nrow(modelmatrix_cor)
    }
  }
  if(alpha_adjust) {
    attr(retval, "null_pvals") = attr(nullresults, "pvals")
    attr(retval, "null_effect_pvals") = attr(nullresults, "effect_pvals")
  }
  attr(retval, "generating.model") = generatingmodel
  attr(retval, "runmatrix") = RunMatrixReduced
  attr(retval, "variance.matrix") = V
  attr(retval, "estimates") = estimates
  attr(retval, "pvals") = attr(power_values, "pvals")
  attr(retval, "effect_pvals") = attr(power_values, "effect_pvals")
  attr(retval, "stderrors") = attr(power_values, "stderrors")
  attr(retval, "fisheriterations") = attr(power_values, "fisheriterations")
  attr(retval, "alpha") = alpha
  attr(retval, "blocking") = blocking
  attr(retval, "varianceratios") = varianceratios



  if(advancedoptions$save_simulated_responses) {
    attr(retval, "simulated_responses") = responses
  }
  if(!inherits(retval,"skpr_eval_output")) {
    class(retval) = c("skpr_eval_output", class(retval))
  }
  #Add recommended analysis method
  contrast_string = deparse(substitute(contrasts))
  attr(retval, "contrast_string") = sprintf("`%s`",contrast_string)
  if(calceffect) {
    if(effect_anova) {
      effect_string = sprintf(r"{`car::Anova(fit, type = "III")`}")
    } else {
      effect_string = r"{`lmtest::lrtest(fit, fit_without_effect)`}"
    }
  } else {
    effect_string = ""
  }
  if(glmfamilyname == "gaussian") {
    if(!blocking) {
      attr(retval, "parameter_analysis_method_string") = "`lm(...)`"
      attr(retval, "effect_analysis_method_string") = effect_string
    } else {
      attr(retval, "parameter_analysis_method_string") = "`lmerTest::lmer(...)`"
      attr(retval, "effect_analysis_method_string") = effect_string
    }
  } else if (glmfamilyname == "binomial") {
    if(!blocking) {
      if(!firth) {
        attr(retval, "parameter_analysis_method_string") = r"{glm(..., family = "binomial")`}"
        attr(retval, "effect_analysis_method_string") = effect_string
      } else {
        attr(retval, "parameter_analysis_method_string") = r"{glm(..., family = "binomial", method = mbest::firthglm.fit)`}" #"
        if(calceffect) {
          attr(retval, "effect_analysis_method_string") = r"{lmtest::lrtest(fit, fit_without_effect)`}"
        } else {
          attr(retval, "effect_analysis_method_string") = ""
        }
      }
    } else {
      attr(retval, "parameter_analysis_method_string") = r"{`lme4::glmer(..., family = "binomial")`}"
      if(calceffect) {
        attr(retval, "effect_analysis_method_string") = effect_string
      } else {
        attr(retval, "effect_analysis_method_string") = ""
      }
    }
  } else if (glmfamilyname == "poisson") {
    if(!blocking) {
      attr(retval, "parameter_analysis_method_string") = r"{`glm(..., family = "poisson")`}"
      attr(retval, "effect_analysis_method_string") = effect_string
    } else {
      attr(retval, "parameter_analysis_method_string") = r"{`lme4::glmer(..., family = "poisson")`}"
      attr(retval, "effect_analysis_method_string") = effect_string
    }
  } else if (glmfamilyname == "exponential") {
    if(!blocking) {
      attr(retval, "parameter_analysis_method_string") = r"{`glm(..., family = Gamma(link = "log")); summary(fit, dispersion = 1)`}"
      attr(retval, "effect_analysis_method_string") = effect_string
    } else {
      attr(retval, "parameter_analysis_method_string") = r"{`lme4::glmer(..., family = Gamma(link = "log")); summary(fit, dispersion = 1)`}"
      attr(retval, "effect_analysis_method_string") = effect_string
    }
  } else {
    attr(retval, "parameter_analysis_method_string") = ""
    attr(retval, "effect_analysis_method_string")    = ""
  }
  return(retval)
}
globalVariables("i")
