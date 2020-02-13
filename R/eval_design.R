#'@title Calculate Power of an Experimental Design
#'
#'@description Evaluates the power of an experimental design, for normal response variables,
#'given the design's run matrix and the statistical model to be fit to the data.
#'Returns a data frame of parameter and effect powers. Designs can
#'consist of both continuous and categorical factors. By default, \code{eval_design}
#'assumes a signal-to-noise ratio of 2, but this can be changed with the
#'\code{effectsize} or \code{anticoef} parameters.
#'
#'@param design The experimental design. Internally, \code{eval_design} rescales each numeric column
#'to the range [-1, 1], so you do not need to do this scaling manually.
#'@param model The model used in evaluating the design. If this is missing and the design
#'was generated with skpr, the generating model will be used. It can be a subset of the model used to
#'generate the design, or include higher order effects not in the original design generation. It cannot include
#'factors that are not present in the experimental design.
#'@param alpha Default `0.05`. The specified type-I error.
#'@param blocking Default `NULL`. If `TRUE`, \code{eval_design} will look at the rownames (or blocking columns) to determine blocking structure. Default FALSE.
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients
#'will be automatically generated based on the \code{effectsize} argument.
#'@param effectsize Default `2`. The signal-to-noise ratio. For continuous factors, this specifies the
#' difference in response between the highest and lowest levels of the factor (which are -1 and +1 after \code{eval_design}
#' normalizes the input data), assuming that the root mean square error is 1. If you do not specify \code{anticoef},
#' the anticipated coefficients will be half of \code{effectsize}. If you do specify \code{anticoef}, \code{effectsize} will be ignored.
#'@param varianceratios Default `NULL`. The ratio of the whole plot variance to the run-to-run variance.
#'If not specified during design generation, this will default to 1. For designs with more than one subplot
#'this ratio can be a vector specifying the variance ratio for each subplot (comparing to the run-to-run variance).
#'Otherwise, it will use a single value for all strata.
#'@param contrasts Default \code{contr.sum}. The function to use to encode the categorical factors in the model matrix. If the user has specified their own contrasts
#'for a categorical factor using the contrasts function, those will be used. Otherwise, skpr will use contr.sum.
#'@param detailedoutput If `TRUE``, return additional information about evaluation in results. Default FALSE.
#'@param conservative Specifies whether default method for generating
#'anticipated coefficents should be conservative or not. `TRUE` will give the most conservative
#'estimate of power by setting all but one (or multiple if they are equally low) level in each categorical factor's anticipated coefficients
#'to zero. Default `FALSE`.
#'@param reorder_factors Default `FALSE`. If `TRUE`, the levels will be reordered to generate the most conservative calculation of effect power.
#'The function searches through all possible reference levels for a given factor and chooses the one that results in the lowest effect power.
#'The reordering will be presenting in the output when `detailedoutput = TRUE`.
#'@param advancedoptions Default `NULL`. A named list with parameters to specify additional attributes to calculate. Options: `aliaspower`
#'gives the degree at which the Alias matrix should be calculated.
#'@param ... Additional arguments.
#'@return A data frame with the parameters of the model, the type of power analysis, and the power. Several
#'design diagnostics are stored as attributes of the data frame. In particular,
#'the \code{modelmatrix} attribute contains the model matrix that was used for power evaluation. This is
#'especially useful if you want to specify the anticipated coefficients to use for power evaluation. The model
#'matrix provides the order of the model coefficients, as well as the
#'encoding used for categorical factors.
#'@details This function evaluates the power of experimental designs.
#'
#'If the design is has no blocking or restrictions on randomization, the model assumed is:
#'
#'\eqn{y = X \beta + \epsilon}.
#'
#'If the design is a split-plot design, the model is as follows:
#'
#'\ifelse{html}{\eqn{y = X \beta + Z b}\out{<sub>i</sub>} + \eqn{\epsilon}\out{<sub>ij</sub>}}{\eqn{y = X \beta + Z b_{i} + \epsilon_{ij}}},
#'
#'Here, \eqn{y} is the vector of experimental responses, \eqn{X} is the model matrix, \eqn{\beta} is
#'the vector of model coefficients, \eqn{Z_{i}} are the blocking indicator,
#'\eqn{b_{i}} is the random variable associated with the \eqn{i}th block, and \eqn{\epsilon}
#'is a random variable normally distributed with zero mean and unit variance (root-mean-square error is 1.0).
#'
#'\code{eval_design} calculates both parameter power as well as effect power, defined as follows:
#'
#'1) Parameter power is the probability of rejecting the hypothesis \eqn{H_0 : \beta_i = 0}, where \eqn{\beta_i} is a single parameter
#'in the model
#'2) Effect power is the probability of rejecting the hypothesis \eqn{H_0 : \beta_{1} = \beta_{2} = ... = \beta_{n} 0} for all \eqn{n} coefficients
#'for a categorical factor.
#'
#'The two power types are equivalent for continuous factors and two-level categorical factors,
#'but they will differ for categorical factors with three or more levels.
#'
#'For split-plot designs, the degrees of freedom are allocated to each term according to the algorithm
#'given in "Mixed-Effects Models in S and S-PLUS" (Pinheiro and Bates, pp. 91).
#'
#'When using \code{conservative = TRUE}, \code{eval_design} first evaluates the power with the default (or given) coefficients. Then,
#'for each multi-level categorical, it sets all coefficients to zero except the level that produced the lowest power,
#'and then re-evaluates the power with this modified set of anticipated coefficients. If there are two or more
#'equal power values in a multi-level categorical, two of the lowest equal terms are given opposite sign anticipated coefficients
#'and the rest (for that categorical factor) are set to zero.
#'@export
#'@examples #Generating a simple 2x3 factorial to feed into our optimal design generation
#'#of an 11-run design.
#'factorial = expand.grid(A = c(1, -1), B = c(1, -1), C = c(1, -1))
#'
#'optdesign = gen_design(candidateset = factorial,
#'                       model= ~A + B + C, trials = 11, optimality = "D", repeats = 100)
#'
#'#Now evaluating that design (with default anticipated coefficients and a effectsize of 2):
#'eval_design(design = optdesign, model= ~A + B + C, alpha = 0.2)
#'
#'#Evaluating a subset of the design (which changes the power due to a different number of
#'#degrees of freedom)
#'eval_design(design = optdesign, model= ~A + C, alpha = 0.2)
#'
#'#We do not have to input the model if it's the same as the model used
#'#During design generation. Here, we also use the default value for alpha (`0.05`)
#'eval_design(optdesign)
#'
#'#Halving the signal-to-noise ratio by setting a different effectsize (default is 2):
#'eval_design(design = optdesign, model= ~A + B + C, alpha = 0.2, effectsize = 1)
#'
#'#With 3+ level categorical factors, the choice of anticipated coefficients directly changes the
#'#final power calculation. For the most conservative power calculation, that involves
#'#setting all anticipated coefficients in a factor to zero except for one. We can specify this
#'#option with the "conservative" argument.
#'
#'factorialcoffee = expand.grid(cost = c(1, 2),
#'                               type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
#'                               size = as.factor(c("Short", "Grande", "Venti")))
#'
#'designcoffee = gen_design(factorialcoffee,
#'                          ~cost + size + type, trials = 29, optimality = "D", repeats = 100)
#'
#'#Evaluate the design, with default anticipated coefficients (conservative is FALSE by default).
#'eval_design(designcoffee)
#'
#'#Evaluate the design, with conservative anticipated coefficients:
#'eval_design(designcoffee, conservative = TRUE)
#'
#'#which is the same as the following, but now explicitly entering the coefficients:
#'eval_design(designcoffee, anticoef = c(1, 1, 1, 0, 0, 1, 0))
#'
#'#You can also evaluate the design with higher order effects, even if they were not
#'#used in design generation:
#'eval_design(designcoffee, model = ~cost + size + type + cost * type)
#'
#'
#'#Generating and evaluating a split plot design:
#'splitfactorialcoffee = expand.grid(caffeine = c(1, -1),
#'                                   cost = c(1, 2),
#'                                   type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
#'                                   size = as.factor(c("Short", "Grande", "Venti")))
#'
#'coffeeblockdesign = gen_design(splitfactorialcoffee, ~caffeine, trials = 12)
#'coffeefinaldesign = gen_design(splitfactorialcoffee,
#'                               model = ~caffeine + cost + size + type, trials = 36,
#'                               splitplotdesign = coffeeblockdesign, blocksizes = 3)
#'
#'#Evaluating design (blocking is automatically detected)
#'eval_design(coffeefinaldesign, 0.2, blocking = TRUE)
#'
#'#Manually turn blocking off to see completely randomized design power
#'eval_design(coffeefinaldesign, 0.2, blocking = FALSE)
#'
#'#We can also evaluate the design with a custom ratio between the whole plot error to
#'#the run-to-run error.
#'eval_design(coffeefinaldesign, 0.2, varianceratios = 2)
#'
#'#If the design was generated outside of skpr and thus the row names do not have the
#'#blocking structure encoded already, the user can add these manually. For a 12-run
#'#design with 4 blocks, here is a vector indicated the blocks:
#'
#'blockcolumn = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
#'
#'#If we wanted to add this blocking structure to the design coffeeblockdesign, we would
#'#add a column with the format "Block1", "Block2", "Block3" ... and each one will be treated
#'#as a separate blocking layer.
#'
#'coffeeblockdesign$Block1 = blockcolumn
#'
#'#By default, skpr will throw out the blocking columns unless the user specifies `blocking = TRUE`.
#'eval_design(coffeeblockdesign, blocking=TRUE)
eval_design = function(design, model = NULL, alpha = 0.05,
                       blocking = NULL, anticoef = NULL,
                       effectsize = 2, varianceratios = NULL,
                       contrasts = contr.sum, conservative = FALSE, reorder_factors = FALSE,
                       detailedoutput = FALSE, advancedoptions = NULL, ...) {
  if(missing(design)) {
    stop("No design detected in arguments.")
  }
  if(missing(model) || (is.numeric(model) && missing(alpha))) {
    if(is.numeric(model) && missing(alpha)) {
      alpha = model
    }
    if(is.null(attr(design,"generating.model"))) {
      stop("No model detected in arguments or in design attributes.")
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
  input_design = design
  args = list(...)
  if ("RunMatrix" %in% names(args)) {
    stop("RunMatrix argument deprecated. Use `design` instead.")
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

  #detect pre-set contrasts
  presetcontrasts = list()
  for (x in names(design)[lapply(design, class) %in% c("character", "factor")]) {
    if (!is.null(attr(design[[x]], "contrasts"))) {
      presetcontrasts[[x]] = attr(design[[x]], "contrasts")
    }
  }
  # reorder levels for the conservative calculation (if not a balanced design)
  if (conservative) {
    for (x in names(design)[lapply(design, class) %in% c("character", "factor")]) {
      number_levels = table(design[[x]])
      if(length(unique(number_levels)) != 1) {
        if(identical(contrasts, contr.sum)) {
          number_levels = table(design[[x]])
          order_levels = names(number_levels)[order(number_levels)]
          design[[x]] = factor(design[[x]], levels = rev(order_levels))
        } else if (identical(contrasts, contr.treatment)) {
          number_levels = table(design[[x]])
          order_levels = names(number_levels)[order(number_levels)]
          design[[x]] = factor(design[[x]], levels = order_levels)
        }
      }
    }
  }
  if(is.null(advancedoptions$aliaspower)) {
    aliaspower = 2
  } else {
    if(!is.numeric(advancedoptions$aliaspower)) {
      stop("advancedoptions$aliaspower must be a positive integer")
    }
    aliaspower = advancedoptions$aliaspower
  }
  nointercept = attr(stats::terms.formula(model, data = design), "intercept") == 0
  #covert tibbles
  run_matrix_processed = as.data.frame(design)

  #Detect externally generated blocking columns and convert to rownames
  run_matrix_processed = convert_blockcolumn_rownames(run_matrix_processed, blocking,
                                                      varianceratios, user_specified_varianceratio)
  varianceratios = attr(run_matrix_processed,"tempvar")
  attr(run_matrix_processed,"tempvar") = NULL
  zlist = attr(run_matrix_processed, "z.matrix.list")

  #Remove skpr-generated REML blocking columns if present
  run_matrix_processed = remove_skpr_blockcols(run_matrix_processed)

  #----- Convert dots in formula to terms -----#
  model = convert_model_dots(run_matrix_processed, model)

  #----- Rearrange formula terms by order -----#
  model = rearrange_formula_by_order(model)
  if (nointercept) {
    model = update.formula(model, ~-1 + . )
  }

  #---- Reduce run matrix to terms in model ---#
  run_matrix_processed = reduceRunMatrix(run_matrix_processed, model)

  #---Develop contrast lists for model matrix---#
  #Variables used later: contrastslist, contrastslist_cormat
  contrastslist = list()
  contrastslist_cormat = list()
  for (x in names(run_matrix_processed[lapply(run_matrix_processed, class) %in% c("character", "factor")])) {
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

  #------Normalize/Center numeric columns ------#
  run_matrix_processed = normalize_numeric_runmatrix(run_matrix_processed)

  #-Generate Model Matrix & Anticipated Coefficients-#
  #Variables used later: anticoef
  attr(run_matrix_processed, "modelmatrix") = model.matrix(model, run_matrix_processed, contrasts.arg = contrastslist)

  if (!missing(anticoef) && !missing(effectsize)) {
    warning("User defined anticipated coefficients (anticoef) detected; ignoring effectsize argument.")
  }
  if (missing(anticoef)) {
    default_coef = gen_anticoef(run_matrix_processed, model, nointercept)
    anticoef = anticoef_from_delta(default_coef, effectsize, "gaussian")
    if (nointercept) {
      anticoef = anticoef[-1]
    }
  }
  if (length(anticoef) != dim(attr(run_matrix_processed, "modelmatrix"))[2]) {
    stop("Wrong number of anticipated coefficients")
  }

  #-----Generate V inverse matrix-----X
  #Variables used later: V, vinv, degrees_of_freedom, parameter_names
  if (blocking) {
    V = convert_rownames_to_covariance(run_matrix_processed, varianceratios, user_specified_varianceratio)
    varianceratios = attr(V,"tempvar")
    attr(run_matrix_processed,"tempvar") = NULL
    vinv = solve(V)
    #Below code detects the split-plot columns, and calculates the adjusted degrees of freedom for each term
    degrees_of_freedom = calculate_degrees_of_freedom(run_matrix_processed, nointercept, model, contrasts)
    if (!nointercept) {
      extract_intnames_formula = ~1
      parameter_names = list()
      parameter_names[["(Intercept)"]] = "(Intercept)"
    } else {
      extract_intnames_formula = ~-1
      parameter_names = list()
    }
    for(i in (length(parameter_names)+1):length(degrees_of_freedom)) {
      currentterm = names(degrees_of_freedom)[i]
      extract_intnames_formula = update.formula(extract_intnames_formula, as.formula(paste0("~. + ", currentterm)))
      newcolnames = suppressWarnings(colnames(model.matrix(extract_intnames_formula, data = design, contrasts.arg = contrastslist)))
      parameter_names[[currentterm]] = newcolnames[!(newcolnames %in% unlist(parameter_names))]
    }
  } else {
    vinv = NULL
    degrees_of_freedom = NULL
    parameter_names = NULL
  }

  factornames = attr(terms(model), "term.labels")
  levelvector = calculate_level_vector(run_matrix_processed, model, nointercept)
  effectresults = effectpower(run_matrix_processed, levelvector, anticoef,
                              alpha, vinv = vinv, degrees = degrees_of_freedom)
  parameterresults = parameterpower(run_matrix_processed, levelvector, anticoef,
                                    alpha, vinv = vinv, degrees = degrees_of_freedom, parameter_names = parameter_names)

  typevector = c(rep("effect.power", length(effectresults)), rep("parameter.power", length(parameterresults)))
  if (!nointercept) {
    effectnamevector = c("(Intercept)", factornames)
  } else {
    effectnamevector = factornames
  }
  parameternamevector = colnames(attr(run_matrix_processed, "modelmatrix"))
  namevector = c(effectnamevector, parameternamevector)
  powervector = c(effectresults, parameterresults)

  results = data.frame(parameter = namevector, type = typevector, power = powervector)

  if (length(namevector) != length(typevector)) {
    warning("Number of names does not equal number of power calculations")
  }

  attr(results, "modelmatrix") = attr(run_matrix_processed, "modelmatrix")
  attr(results, "anticoef") = anticoef

  modelmatrix_cor = model.matrix(model, run_matrix_processed, contrasts.arg = contrastslist_cormat)
  if (ncol(modelmatrix_cor) > 2) {
    if (!blocking) {
      V = diag(nrow(modelmatrix_cor))
    }
    if (!nointercept) {
      correlation.matrix = abs(cov2cor(solve(t(modelmatrix_cor) %*% solve(V) %*% modelmatrix_cor))[-1, -1])
      colnames(correlation.matrix) = colnames(modelmatrix_cor)[-1]
      rownames(correlation.matrix) = colnames(modelmatrix_cor)[-1]
    } else {
      correlation.matrix = abs(cov2cor(solve(t(modelmatrix_cor) %*% solve(V) %*% modelmatrix_cor)))
      colnames(correlation.matrix) = colnames(modelmatrix_cor)
      rownames(correlation.matrix) = colnames(modelmatrix_cor)
    }
    attr(results, "correlation.matrix") = round(correlation.matrix, 8)
    tryCatch({
      if (ncol(attr(run_matrix_processed, "modelmatrix")) > 2) {
        amodel = aliasmodel(model, aliaspower)
        if (amodel != model) {
          aliasmatrix = suppressWarnings({
            model.matrix(aliasmodel(model, aliaspower), design, contrasts.arg = contrastslist)[, -1]
          })
          A = solve(t(attr(run_matrix_processed, "modelmatrix")) %*%
                      attr(run_matrix_processed, "modelmatrix")) %*%
                    t(attr(run_matrix_processed, "modelmatrix")) %*% aliasmatrix
          attr(results, "alias.matrix") = A
          attr(results, "trA") = sum(diag(t(A) %*% A))
        } else {
          attr(results, "alias.matrix") = "No alias matrix calculated: full model specified"
          attr(results, "trA") = "No alias trace calculated: full model specified"
        }
      }
    }, error = function(e) {})
  }
  attr(results, "generating.model") = model
  attr(results, "runmatrix") = run_matrix_processed
  attr(results, "model.matrix") = modelmatrix_cor
  attr(results, "blocking") = blocking
  attr(results, "varianceratios") = varianceratios
  attr(results, "alpha") = alpha

  levelvector = sapply(lapply(run_matrix_processed, unique), length)
  classvector = sapply(lapply(run_matrix_processed, unique), class) == "factor"
  mm = gen_momentsmatrix(colnames(attr(run_matrix_processed, "modelmatrix")), levelvector, classvector)

  attr(results, "moment.matrix") = mm
  attr(results, "A") = AOptimality(attr(run_matrix_processed, "modelmatrix"))

  if (!blocking) {
    attr(results, "variance.matrix") = diag(nrow(modelmatrix_cor)) * varianceratios
    attr(results, "I") = IOptimality(modelmatrix_cor, momentsMatrix = mm, blockedVar = diag(nrow(modelmatrix_cor)))
    deffic = DOptimality(modelmatrix_cor)
    if(!is.infinite(deffic)) {
      attr(results, "D") =  100 * DOptimality(modelmatrix_cor) ^ (1 / ncol(modelmatrix_cor)) / nrow(modelmatrix_cor)
    } else {
      attr(results, "D") =  100 * DOptimalityLog(modelmatrix_cor) ^ (1 / ncol(modelmatrix_cor)) / nrow(modelmatrix_cor)
    }
  } else {
    attr(results, "z.matrix.list") = zlist
    attr(results, "variance.matrix") = V
    attr(results, "I") = IOptimality(modelmatrix_cor, momentsMatrix = mm, blockedVar = V)
    deffic = DOptimalityBlocked(modelmatrix_cor, blockedVar = V)
    if(!is.infinite(deffic)) {
      attr(results, "D") =  100 * DOptimalityBlocked(modelmatrix_cor, blockedVar = V) ^ (1 / ncol(modelmatrix_cor)) / nrow(modelmatrix_cor)
    } else {
      attr(results, "D") =  100 * DOptimalityBlockedLog(modelmatrix_cor, blockedVar = V) ^ (1 / ncol(modelmatrix_cor)) / nrow(modelmatrix_cor)
    }
  }
  if (detailedoutput) {
    if (nrow(results) != length(anticoef)){
      results$anticoef = c(rep(NA, nrow(results) - length(anticoef)), anticoef)
    } else {
      results$anticoef = anticoef
    }
    results$alpha = alpha
    results$trials = nrow(run_matrix_processed)
  }



  #For conservative coefficients, look for lowest power results from non-conservative calculation and set them to one
  #and the rest to zero. (If equally low results, apply 1 -1 pattern to lowest)

  if (conservative) {
    #at this point, since we are going to specify anticoef, do not use the effectsize argument
    #in the subsequent call. Do replicate the magnitudes from the original anticoef
    conservative_anticoef = calc_conservative_anticoef(results, effectsize)
    results = eval_design(design = design, model = model, alpha = alpha, blocking = blocking,
                anticoef = conservative_anticoef,
                detailedoutput = detailedoutput,
                varianceratios = varianceratios, contrasts = contrasts, conservative = FALSE, reorder_factors = reorder_factors)
  }

  if(reorder_factors) {
    if(detailedoutput) {
      results$reordered_factors = NA
    }
    results_temp = results
    sum_effect_power = sum(with(results_temp, power[type == "effect.power"]))
    for(i in seq_len(ncol(design))) {
      if(is.factor(design[,i])) {
        number_factors = length(levels(design[,i]))
        temp_design = design
        cycle = list()
        cycle[[1]] = levels(temp_design[,i])
        order_cycle = seq_len(number_factors)
        for(j in seq_len(number_factors)[-1]) {
          order_cycle = c(order_cycle[-1],order_cycle[1])
          cycle[[j]] = levels(temp_design[,i])[order_cycle]
        }
        for(j in seq_len(number_factors)) {
          temp_design[,i] = factor(temp_design[,i],cycle[[j]])
          adjusted_sum_power = eval_design(design = temp_design, model = model, alpha = alpha, blocking = blocking,
                                           detailedoutput = detailedoutput,
                                           varianceratios = varianceratios, contrasts = contrasts, conservative = conservative,
                                           reorder_factors = FALSE)
          new_sum_power = sum(with(adjusted_sum_power, power[type == "effect.power"]))
          if(new_sum_power < sum_effect_power) {
            design[,i] = factor(design[,i],cycle[[j]])
            results_temp = adjusted_sum_power
            sum_effect_power = new_sum_power
            if(detailedoutput) {
              results_temp$reordered_factors[results_temp$parameter == names(temp_design)[i]] = paste0(cycle[[j]], collapse = " ")
            }
          }
        }
      }
    }
    results = results_temp
  }
  if(!inherits(results,"skpr_eval_output")) {
    class(results) = c("skpr_eval_output", class(results))
  }
  return(results)
}
