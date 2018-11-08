#'@title Monte Carlo power evaluation for experimental designs with user-supplied libraries
#'
#'@description Evaluates the power of an experimental design, given its run matrix and the
#'statistical model to be fit to the data, using monte carlo simulation. Simulated data is fit using a
#'user-supplied fitting library and power is estimated by the fraction of times a parameter is significant. Returns
#'a data frame of parameter powers.
#'
#'@param design The experimental design. Internally, \code{eval_design_custom_mc} rescales each numeric column
#'to the range [-1, 1].
#'@param model The statistical model used to fit the data.
#'@param alpha The type-I error.
#'@param nsim The number of simulations.
#'@param rfunction Random number generator function. Should be a function of the form f(X, b), where X is the
#'model matrix and b are the anticipated coefficients.
#'@param fitfunction Function used to fit the data. Should be of the form f(formula, X, contrasts)
#'where X is the model matrix. If contrasts do not need to be specified for the user supplied
#'library, that argument can be ignored.
#'@param pvalfunction Function that returns a vector of p-values from the object returned from the fitfunction.
#'@param coef_function Function that, when applied to a fitfunction return object, returns the estimated coefficients.
#'@param parameternames Vector of parameter names if the coefficients do not correspond simply to the columns in the model matrix
#'(e.g. coefficients from an MLE fit).
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients will be
#'automatically generated based on \code{effectsize}.
#'@param effectsize The signal-to-noise ratio. Default 2. For a gaussian model, and for
#'continuous factors, this specifies the difference in response between the highest
#'and lowest levels of a factor (which are +1 and -1 after normalization).
#'More precisely: If you do not specify \code{anticoef}, the anticipated coefficients will be
#'half of \code{effectsize}. If you do specify \code{anticoef}, \code{effectsize} will be ignored.
#'@param contrasts  Default \code{contr.sum}. Function used to generate the contrasts encoding for categorical variables. If the user has specified their own contrasts
#'for a categorical factor using the contrasts function, those will be used. Otherwise, skpr will use contr.sum.
#'@param parallel If TRUE, uses all cores available to speed up computation of power. Default FALSE.
#'@param parallelpackages A vector of strings listing the external packages to be input into the parallel package.
#'@param ... Additional arguments.
#'@return A data frame consisting of the parameters and their powers. The parameter estimates from the simulations are
#'stored in the 'estimates' attribute.
#'@import foreach doParallel stats
#'@export
#'@examples #To demonstrate how a user can use their own libraries for Monte Carlo power generation,
#'#We will recreate eval_design_survival_mc using the eval_design_custom_mc framework.
#'
#'#To begin, first let us generate the same design and random generation function shown in the
#'#eval_design_survival_mc examples:
#'
#'basicdesign = expand.grid(a = c(-1, 1))
#'design = gen_design(candidateset = basicdesign, model = ~a, trials = 100,
#'                          optimality = "D", repeats = 100)
#'
#'#Random number generating function
#'
#'rsurvival = function(X, b) {
#'  Y = rexp(n = nrow(X), rate = exp(-(X %*% b)))
#'  censored = Y > 1
#'  Y[censored] = 1
#'  return(survival::Surv(time = Y, event = !censored, type = "right"))
#'}
#'
#'#We now need to tell the package how we want to fit our data,
#'#given the formula and the model matrix X (and, if needed, the list of contrasts).
#'#If the contrasts aren't required, "contrastslist" should be set to NULL.
#'#This should return some type of fit object.
#'
#'fitsurv = function(formula, X, contrastslist = NULL) {
#'  return(survival::survreg(formula, data = X, dist = "exponential"))
#'}
#'
#'
#'#We now need to tell the package how to extract the p-values from the fit object returned
#'#from the fit function. This is how to extract the p-values from the survreg fit object:
#'
#'pvalsurv = function(fit) {
#'  return(summary(fit)$table[, 4])
#'}
#'
#'#And now we evaluate the design, passing the fitting function and p-value extracting function
#'#in along with the standard inputs for eval_design_mc.
#'
#'d = eval_design_custom_mc(design = design, model = ~a,
#'                          alpha = 0.05, nsim = 100,
#'                          fitfunction = fitsurv, pvalfunction = pvalsurv,
#'                          rfunction = rsurvival, effectsize = 1)
#'
#'#This has the exact same behavior as eval_design_survival_mc for the exponential distribution.
eval_design_custom_mc = function(design, model, alpha, nsim, rfunction, fitfunction, pvalfunction,
                                 anticoef, effectsize = 2, contrasts = contr.sum,
                                 coef_function = coef,
                                 parameternames = NULL,
                                 parallel = FALSE, parallelpackages = NULL, ...) {
  args = list(...)
  if("RunMatrix" %in% names(args)) {
    stop("RunMatrix argument deprecated. Use `design` instead.")
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

  #----- Convert dots in formula to terms -----#
  model = convert_model_dots(run_matrix_processed,model)

  #----- Rearrange formula terms by order -----#
  model = rearrange_formula_by_order(model)

  #------Normalize/Center numeric columns ------#
  run_matrix_processed = normalize_numeric_runmatrix(run_matrix_processed)

  #Remove skpr-generated REML blocking indicators if present
  run_matrix_processed = remove_skpr_blockcols(run_matrix_processed)

  #---------- Generating model matrix ----------#
  #remove columns from variables not used in the model
  RunMatrixReduced = reduceRunMatrix(run_matrix_processed, model)

  contrastslist = list()
  for (x in names(RunMatrixReduced[lapply(RunMatrixReduced, class) %in% c("character", "factor")])) {
    if (!(x %in% names(presetcontrasts))) {
      contrastslist[[x]] = contrasts
    } else {
      contrastslist[[x]] = presetcontrasts[[x]]
    }
  }

  if (length(contrastslist) < 1) {
    contrastslist = NULL
  }

  ModelMatrix = model.matrix(model, RunMatrixReduced, contrasts.arg = contrastslist)
  #We'll need the parameter and effect names for output
  if (is.null(parameternames)) {
    parameter_names = colnames(ModelMatrix)
  } else {
    parameter_names = parameternames
  }

  # autogenerate anticipated coefficients
  if (!missing(effectsize) && !missing(anticoef)) {
    warning("User defined anticipated coefficnets (anticoef) detected; ignoring effectsize argument.")
  }
  if (missing(anticoef)) {
    anticoef = gen_anticoef(RunMatrixReduced, model) * effectsize / 2
    if (!("(Intercept)" %in% colnames(ModelMatrix))) {
      anticoef = anticoef[-1]
    }
  }
  if (length(anticoef) != dim(ModelMatrix)[2]) {
    stop("Wrong number of anticipated coefficients")
  }


  model_formula = update.formula(model, Y ~ .)
  nparam = ncol(ModelMatrix)
  RunMatrixReduced$Y = 1

  if (!parallel) {
    power_values = rep(0, length(parameter_names))
    estimates = list()
    for (j in 1:nsim) {

      #simulate the data.
      RunMatrixReduced$Y = rfunction(ModelMatrix, anticoef)

      #fit a model to the simulated data.
      fit = fitfunction(model_formula, RunMatrixReduced, contrastslist)

      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = pvalfunction(fit)
      power_values[pvals < alpha] = power_values[pvals < alpha] + 1
      estimates[[j]] = coef_function(fit)
    }
    power_values = power_values / nsim

  } else {
    if (is.null(options("cores")[[1]])) {
      numbercores = parallel::detectCores()
    } else {
      numbercores = options("cores")[[1]]
    }
    cl = parallel::makeCluster(numbercores)
    doParallel::registerDoParallel(cl, cores = numbercores)

    power_estimates = foreach::foreach (i = 1:nsim, .combine = "rbind", .packages = parallelpackages) %dopar% {
      power_values = rep(0, ncol(ModelMatrix))
      #simulate the data.
      RunMatrixReduced$Y = rfunction(ModelMatrix, anticoef)

      #fit a model to the simulated data.
      fit = fitfunction(model_formula, RunMatrixReduced, contrastslist)

      #determine whether beta[i] is significant. If so, increment nsignificant
      pvals = pvalfunction(fit)
      power_values[pvals < alpha] = 1
      estimates = coef_function(fit)

      #We are going to output a tidy data.frame with the results, so just append the effect powers
      #to the parameter powers. We'll use another column of that dataframe to label wether it is parameter
      #or effect power.
      c(power_values, estimates)
    }
    parallel::stopCluster(cl)
    power_values = apply(power_estimates[, 1:nparam], 2, sum) / nsim
    estimates = power_estimates[, (nparam + 1):ncol(power_estimates)]
  }
  #output the results (tidy data format)
  retval = data.frame(parameter = parameter_names,
                      type = "custom.power.mc",
                      power = power_values)
  attr(retval, "estimatesnames") = parameter_names
  attr(retval, "estimates") = estimates
  retval

}
globalVariables("i")
