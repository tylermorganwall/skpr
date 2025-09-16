#'@title Evaluate Power for Survival Design
#'
#'@description Evaluates power for an experimental design in which the response variable may be
#'right- or left-censored. Power is evaluated with a Monte Carlo simulation,
#'using the \code{survival} package and \code{survreg} to fit the data. Split-plot designs are not supported.
#'
#'@param design The experimental design. Internally, all numeric columns will be rescaled to -1 to 1.
#'@param model The model used in evaluating the design. If this is missing and the design
#'was generated with skpr, the generating model will be used. It can be a subset of the model used to
#'generate the design, or include higher order effects not in the original design generation. It cannot include
#'factors that are not present in the experimental design.
#'@param alpha Default `0.05`. The type-I error. p-values less than this will be counted as significant.
#'@param nsim The number of simulations. Default 1000.
#'@param distribution Distribution of survival function to use when fitting the data. Valid choices are described
#'in the documentation for \code{survreg}. \emph{Supported} options are
#'"exponential", "lognormal", or "gaussian". Default "gaussian".
#'@param censorpoint The point after/before (for right-censored or left-censored data, respectively)
#'which data should be labelled as censored. Default NA for no censoring. This argument is
#'used only by the internal random number generators; if you supply your own function to
#'the \code{rfunctionsurv} parameter, then this parameter will be ignored.
#'@param censortype The type of censoring (either "left" or "right"). Default "right".
#'@param rfunctionsurv Random number generator function. Should be a function of the form f(X, b), where X is the
#'model matrix and b are the anticipated coefficients. This function should return a \code{Surv} object from
#'the \code{survival} package. You do not need to provide this argument if \code{distribution} is one of
#' the supported choices and you are satisfied with the default behavior described below.
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients
#'will be automatically generated based on the \code{effectsize} argument.
#'@param effectsize Helper argument to generate anticipated coefficients. See details for more info.
#'If you specify \code{anticoef}, \code{effectsize} will be ignored.
#'@param contrasts Default \code{contr.sum}. Function used to encode categorical variables in the model matrix. If the user has specified their own contrasts
#'for a categorical factor using the contrasts function, those will be used. Otherwise, skpr will use contr.sum.
#'@param parallel Default `FALSE`. If `TRUE`, the power simulation will use all but one of the available cores.
#' If the user wants to set the number of cores manually, they can do this by setting `options("cores")` to the desired number (e.g. `options("cores" = parallel::detectCores())`).
#' NOTE: If you have installed BLAS libraries that include multicore support (e.g. Intel MKL that comes with Microsoft R Open), turning on parallel could result in reduced performance.
#'@param detailedoutput Default `FALSE`. If `TRUE`, return additional information about evaluation in results.
#'@param progress Default `TRUE`. Whether to include a progress bar.
#'@param advancedoptions Default `NULL`. Named list of advanced options. Pass `progressBarUpdater` to include function called in non-parallel simulations that can be used to update external progress bar.
#'`advancedoptions$ci_error_conf` will set the confidence level for power intervals, which are printed when `detailedoutput = TRUE`.
#'@param ... Any additional arguments to be passed into the \code{survreg} function during fitting.
#'@return A data frame consisting of the parameters and their powers. The parameter estimates from the simulations are
#'stored in the 'estimates' attribute. The 'modelmatrix' attribute contains the model matrix and the encoding used for
#'categorical factors. If you manually specify anticipated coefficients, do so in the order of the model matrix.
#'@import foreach doParallel stats iterators doFuture
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
#'"gaussian"                  \tab \code{rnorm(mean = X \%*\% b, sd = 1)}           \cr
#'"exponential"               \tab \code{rexp(rate = exp(-X \%*\% b))}           \cr
#'"lognormal"                 \tab \code{rlnorm(meanlog = X \%*\% b, sdlog = 1)}           \cr
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
#'basicdesign = expand.grid(a = c(-1, 1))
#'design = gen_design(candidateset = basicdesign, model = ~a, trials = 15)
#'
#'#We can then evaluate the power of the design in the same way as eval_design_mc,
#'#now including the type of censoring (either right or left) and the point at which
#'#the data should be censored:
#'
#'eval_design_survival_mc(design = design, model = ~a, alpha = 0.05,
#'                         nsim = 100, distribution = "exponential",
#'                         censorpoint = 5, censortype = "right")
#'
#'#Built-in Monte Carlo random generating functions are included for the gaussian, exponential,
#'#and lognormal distributions.
#'
#'#We can also evaluate different censored distributions by specifying a custom
#'#random generating function and changing the distribution argument.
#'
#'rlognorm = function(X, b) {
#'   Y = rlnorm(n = nrow(X), meanlog = X %*% b, sdlog = 0.4)
#'   censored = Y > 1.2
#'   Y[censored] = 1.2
#'   return(survival::Surv(time = Y, event = !censored, type = "right"))
#'}
#'
#'#Any additional arguments are passed into the survreg function call.  As an example, you
#'#might want to fix the "scale" argument to survreg, when fitting a lognormal:
#'
#'eval_design_survival_mc(design = design, model = ~a, alpha = 0.2, nsim = 100,
#'                         distribution = "lognormal", rfunctionsurv = rlognorm,
#'                         anticoef = c(0.184, 0.101), scale = 0.4)
eval_design_survival_mc = function(
  design,
  model = NULL,
  alpha = 0.05,
  nsim = 1000,
  distribution = "gaussian",
  censorpoint = NA,
  censortype = "right",
  rfunctionsurv = NULL,
  anticoef = NULL,
  effectsize = 2,
  contrasts = contr.sum,
  parallel = FALSE,
  detailedoutput = FALSE,
  progress = TRUE,
  advancedoptions = NULL,
  ...
) {
  if (missing(design)) {
    stop("skpr: No design detected in arguments.")
  }
  if (!is.null(getOption("skpr_progress"))) {
    progress = getOption("skpr_progress")
  }
  if (missing(model) || (is.numeric(model) && missing(alpha))) {
    if (is.numeric(model) && missing(alpha)) {
      alpha = model
    }
    if (is.null(attr(design, "generating_model"))) {
      stop("skpr: No model detected in arguments or in design attributes.")
    } else {
      model = attr(design, "generating_model")
    }
  }
  args = list(...)
  if ("RunMatrix" %in% names(args)) {
    stop("skpr: RunMatrix argument deprecated. Use `design` instead.")
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
  if (is.null(advancedoptions$ci_error_conf)) {
    advancedoptions$ci_error_conf = 0.95
  }
  if (attr(terms.formula(model, data = design), "intercept") == 1) {
    nointercept = FALSE
  } else {
    nointercept = TRUE
  }
  varianceratios = 1
  blocking = FALSE
  #covert to data frame
  processed_design_traits = design_df_to_processed_list(
    design,
    model,
    blocking,
    varianceratios,
    nointercept
  )
  run_matrix_processed = processed_design_traits$design_processed
  model_processed = processed_design_traits$model_processed
  varianceratios = processed_design_traits$varianceratios
  zlist = processed_design_traits$zlist

  #detect pre-set contrasts
  presetcontrasts = list()
  for (x in names(design)[
    lapply(design, class) %in% c("character", "factor")
  ]) {
    if (!is.null(attr(design[[x]], "contrasts"))) {
      presetcontrasts[[x]] = attr(design[[x]], "contrasts")
    }
  }
  #---Develop contrast lists for model matrix---#
  #Variables used later: contrastslist, contrastslist_cormat
  contrast_info = generate_contrast_list(
    run_matrix_processed,
    presetcontrasts,
    contrasts
  )
  contrastslist = contrast_info$contrastslist
  contrastslist_cormat = contrast_info$contrastslist_cormat

  contains_transform = function(expr, transform) {
    if (is.call(expr)) {
      if (as.character(expr[[1]]) == transform) {
        return(TRUE)
      } else {
        return(any(sapply(as.list(expr[-1]), contains_transform)))
      }
    } else {
      return(FALSE)
    }
  }
  #Check for pre-transformed censorpoints
  if (is.null(rfunctionsurv)) {
    if (distribution == "lognormal") {
      if (contains_transform(substitute(censorpoint), "log")) {
        warning(
          "`log` transformation detected in the `censorpoint` input, but this value should be stated in units of the response variable."
        )
      }
    } else if (distribution == "exp") {
      if (contains_transform(substitute(censorpoint), "exp")) {
        warning(
          "`exp` transformation detected in the `censorpoint` input, but this value should be stated in units of the response variable."
        )
      }
    }
  }

  #Generating random generation function for survival. If no censorpoint specified, return all uncensored.
  if (is.na(censorpoint)) {
    censorfunction = function(data, point) rep(FALSE, length(data))
  }
  if (censortype == "left" && !is.na(censorpoint)) {
    censorfunction = function(data, point) data < point
  }
  if (censortype == "right" && !is.na(censorpoint)) {
    censorfunction = function(data, point) data > point
  }

  if (is.null(rfunctionsurv)) {
    if (distribution == "exponential") {
      if (!is.na(censorpoint) && censorpoint <= 0) {
        stop(
          "For an exponential distribution, `censorpoint` must be greater than zero."
        )
      }
      rfunctionsurv = function(X, b) {
        Y = rexp(n = nrow(X), rate = exp(-(X %*% b)))
        condition = censorfunction(Y, censorpoint)
        Y[condition] = censorpoint
        return(survival::Surv(time = Y, event = !condition, type = censortype))
      }
    }
    if (distribution == "lognormal") {
      if (!is.na(censorpoint) && censorpoint <= 0) {
        stop(
          "For an lognormal distribution, `censorpoint` must be greater than zero."
        )
      }
      rfunctionsurv = function(X, b) {
        Y = rlnorm(n = nrow(X), meanlog = X %*% b, sdlog = 1)
        condition = censorfunction(Y, censorpoint)
        Y[condition] = censorpoint
        return(survival::Surv(time = Y, event = !condition, type = censortype))
      }
    }
    if (distribution == "gaussian") {
      rfunctionsurv = function(X, b) {
        Y = rnorm(n = nrow(X), mean = X %*% b, sd = 1)
        condition = censorfunction(Y, censorpoint)
        Y[condition] = censorpoint
        return(survival::Surv(time = Y, event = !condition, type = censortype))
      }
    }
  }
  candidate_set = attr(design, "candidate_set")

  model_matrix = model.matrix(
    model_processed,
    run_matrix_processed,
    contrasts.arg = contrastslist
  )
  #We'll need the parameter and effect names for output
  parameter_names = colnames(model_matrix)

  # autogenerate anticipated coefficients
  if (!missing(anticoef) && !missing(effectsize)) {
    warning(
      "User defined anticipated coefficients (anticoef) detected; ignoring effectsize argument."
    )
  }
  if (missing(anticoef)) {
    default_coef = gen_anticoef(
      run_matrix_processed,
      model_processed,
      nointercept
    )
    anticoef = anticoef_from_delta_surv(default_coef, effectsize, distribution)
    if (!("(Intercept)" %in% colnames(model_matrix))) {
      anticoef = anticoef[-1]
    }
  }
  if (length(anticoef) != dim(model_matrix)[2]) {
    stop("skpr: Wrong number of anticipated coefficients")
  }

  nparam = ncol(model_matrix)
  run_matrix_processed$Y = 1

  #---survival specific, since no contrasts arg----#
  for (i in seq_len(ncol(run_matrix_processed))) {
    if (inherits(run_matrix_processed[, i], c("factor", "character"))) {
      run_matrix_processed[, i] = as.factor(run_matrix_processed[, i])
      contrasts(run_matrix_processed[, i]) = contr.sum(nlevels(
        as.factor(run_matrix_processed[, i])
      ))
    }
  }

  #---------------- Run Simulations ---------------#

  num_updates = min(c(nsim, 200))
  progressbarupdates = floor(seq(1, nsim, length.out = num_updates))
  progresscurrent = 1
  pvallist = list()
  estimates = matrix(0, nrow = nsim, ncol = nparam)
  if (!parallel) {
    power_values = rep(0, ncol(model_matrix))
    if (interactive() && progress) {
      pb = progress::progress_bar$new(
        format = sprintf(
          "  Calculating Power [:bar] (:current/:total, :tick_rate sim/s) ETA: :eta"
        ),
        total = nsim,
        clear = TRUE,
        width = 100
      )
    }
    for (j in seq_len(nsim)) {
      if (advancedoptions$GUI && !is.null(progressBarUpdater)) {
        #This code is to slow down the number of updates in the Shiny app--if there
        #are too many updates, the progress bar will lag behind the actual computation
        if (j %in% progressbarupdates) {
          progressBarUpdater(1 / num_updates)
        }
      }
      #simulate the data.
      anticoef_adjusted = anticoef

      run_matrix_processed$Y = rfunctionsurv(model_matrix, anticoef_adjusted)

      model_formula = update.formula(model_processed, Y ~ .)
      #fit a model to the simulated data.
      surv_mat = as.matrix(run_matrix_processed$Y)
      number_censored = sum(surv_mat[, 2] == 0)
      if (number_censored == nrow(run_matrix_processed)) {
        pvals = rep(1, length(parameter_names))
        names(pvals) = parameter_names
        estimates[j, ] = NA
      } else {
        fiterror = FALSE
        tryCatch(
          {
            fit = survival::survreg(
              model_formula,
              data = run_matrix_processed,
              dist = distribution,
              ...
            )
          },
          error = function(e) {
            fiterror <<- TRUE
          },
          warning = function(w) {
            if (
              grepl(
                "Ran out of iterations and did not converge",
                as.character(w)
              )
            ) {
              fiterror <<- TRUE
            }
          }
        )

        #determine whether beta[i] is significant. If so, increment nsignificant
        if (!fiterror && exists("fit")) {
          pvals = extractPvalues(fit)[seq_len(ncol(model_matrix))]
          vals = pvals[order(factor(names(pvals), levels = parameter_names))]
          pvals[is.na(pvals)] = 1
          if (!all(names(pvals) == parameter_names)) {
            param_name_err = paste0(
              sprintf("`%s` ~ `%s`", names(pvals), parameter_names),
              collapse = "\n"
            )
            stop(
              "Parameter names not equalto to pvalue names:\n",
              param_name_err
            )
          }
          estimates[j, ] = coef(fit)
        } else {
          pvals = rep(1, length(parameter_names))
          names(pvals) = parameter_names
          estimates[j, ] = NA
        }
      }
      pvallist[[j]] = pvals
      power_values[pvals < alpha] = power_values[pvals < alpha] + 1
    }
    power_values = power_values / nsim
    pvals = do.call(rbind, pvallist)
    if (interactive() && progress && !advancedoptions$GUI) {
      pb$tick()
    }
  } else {
    if (!getOption("skpr_progress", TRUE)) {
      progressbarupdates = c()
    }
    if (!advancedoptions$GUI && progress) {
      set_up_progressr_handler("Evaluating", "sims")
    }
    nc = future::nbrOfWorkers()
    run_search = function(iterations, is_shiny, surv_args) {
      prog = progressr::progressor(steps = nsim)
      foreach::foreach(
        i = iterations,
        .errorhandling = "remove",
        .options.future = list(
          packages = "survival",
          globals = c(
            "extractPvalues",
            "rfunctionsurv",
            "parameter_names",
            "progress",
            "progressbarupdates",
            "model_processed",
            "distribution",
            "run_matrix_processed",
            "model_matrix",
            "anticoef",
            "nc",
            "prog",
            "is_shiny",
            "num_updates",
            "nsim",
            "alpha",
            "surv_args"
          ),
          seed = TRUE
        )
      ) %dofuture%
        {
          if (i %in% progressbarupdates) {
            if (is_shiny) {
              prog(sprintf(" (%i workers) ", nc), amount = nsim / num_updates)
            } else {
              prog(amount = nsim / num_updates)
            }
          }
          power_values = rep(0, ncol(model_matrix))
          #simulate the data.

          run_matrix_processed$Y = rfunctionsurv(model_matrix, anticoef)

          model_formula = update.formula(model_processed, Y ~ .)

          surv_args$formula = model_formula
          surv_args$data = run_matrix_processed
          surv_args$dist = distribution

          surv_mat = as.matrix(run_matrix_processed$Y)
          number_censored = sum(surv_mat[, 2] == 0)
          if (number_censored == nrow(run_matrix_processed)) {
            pvals = rep(1, length(parameter_names))
            names(pvals) = parameter_names
            estimates = rep(NA, length(parameter_names))
          } else {
            fiterror = FALSE
            tryCatch(
              {
                fit = do.call("survreg", args = surv_args)
              },
              error = function(e) {
                fiterror <<- TRUE
              },
              warning = function(w) {
                if (
                  grepl(
                    "Ran out of iterations and did not converge",
                    as.character(w)
                  )
                ) {
                  fiterror <<- TRUE
                }
              }
            )

            #determine whether beta[i] is significant. If so, increment nsignificant
            if (!fiterror && exists("fit")) {
              pvals = extractPvalues(fit)[seq_len(ncol(model_matrix))]
              vals = pvals[order(factor(
                names(pvals),
                levels = parameter_names
              ))]
              pvals[is.na(pvals)] = 1
              stopifnot(all(names(pvals) == parameter_names))
              estimates = coef(fit)
            } else {
              pvals = rep(1, length(parameter_names))
              names(pvals) = parameter_names
              estimates = rep(NA, length(parameter_names))
            }
          }

          power_values[pvals < alpha] = 1
          list(
            "parameterpower" = power_values,
            "estimates" = estimates,
            "pvals" = pvals
          )
        }
    }
    power_estimates = run_search(seq_len(nsim), advancedoptions$GUI, args)
    power_values = apply(
      do.call("rbind", lapply(power_estimates, \(x) x$parameterpower)),
      2,
      sum
    ) /
      nsim
    pvals = do.call("rbind", lapply(power_estimates, \(x) x$pvals))
    estimates = do.call("rbind", lapply(power_estimates, \(x) x$estimates))
  }
  #output the results (tidy data format)
  results = data.frame(
    parameter = parameter_names,
    type = "parameter.power.mc",
    power = power_values
  )
  colnames(estimates) = parameter_names
  attr(results, "estimates") = estimates
  attr(results, "model_matrix") = model_matrix
  attr(results, "anticoef") = anticoef
  attr(results, "pvals") = pvals
  attr(results, "alpha") = alpha
  attr(results, "runmatrix") = run_matrix_processed
  modelmatrix_cor = model.matrix(
    model_processed,
    run_matrix_processed,
    contrasts.arg = contrastslist_cormat
  )
  attr(results, "model_matrix_cor") = modelmatrix_cor
  if (detailedoutput) {
    if (nrow(results) != length(anticoef)) {
      results$anticoef = c(rep(NA, nrow(results) - length(anticoef)), anticoef)
    } else {
      results$anticoef = anticoef
    }
    results$alpha = alpha
    results$trials = nrow(run_matrix_processed)
    results$nsim = nsim
    results = add_ci_bounds_mc_power(
      results,
      nsim = nsim,
      conf = advancedoptions$ci_error_conf
    )
    attr(results, "mc.conf.int") = advancedoptions$ci_error_conf
  }
  if (!inherits(results, "skpr_eval_output")) {
    class(results) = c("skpr_eval_output", class(results))
  }
  attr(results, "candidate_set") = candidate_set
  attr(results, "contrastslist") = contrastslist

  return(results)
}
globalVariables("i")
