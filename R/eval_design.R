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
#'@param model The model used in evaluating the design. It can be a subset of the model used to
#'generate the design, or include higher order effects not in the original design generation. It cannot include
#'factors that are not present in the experimental design.
#'@param alpha The specified type-I error.
#'@param blocking Default FALSE. If TRUE, \code{eval_design} will look at the rownames to determine blocking structure.
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients
#'will be automatically generated based on the \code{effectsize} argument.
#'@param effectsize The signal-to-noise ratio. Default 2. For continuous factors, this specifies the
#' difference in response between the highest and lowest levels of the factor (which are -1 and +1 after \code{eval_design}
#' normalizes the input data), assuming that the root mean square error is 1. If you do not specify \code{anticoef},
#' the anticipated coefficients will be half of \code{effectsize}. If you do specify \code{anticoef}, \code{effectsize} will be ignored.
#'@param varianceratios Default 1. The ratio of the whole plot variance to the run-to-run variance. For designs with more than one subplot
#'this ratio can be a vector specifying the variance ratio for each subplot. Otherwise, it will use a single value for all strata.
#'@param contrasts Default \code{contr.sum}. The function to use to encode the categorical factors in the model matrix. If the user has specified their own contrasts
#'for a categorical factor using the contrasts function, those will be used. Otherwise, skpr will use contr.sum.
#'@param detailedoutput If TRUE, return additional information about evaluation in results. Default FALSE.
#'@param conservative Specifies whether default method for generating
#'anticipated coefficents should be conservative or not. TRUE will give the most conservative
#'estimate of power by setting all but one level in each categorical factor's anticipated coefficients
#'to zero. Default FALSE.
#'@param ... Additional arguments.
#'@return A data frame with the parameters of the model, the type of power analysis, and the power. Several
#'design diagnostics are stored as attributes of the data frame. In particular,
#'the \code{modelmatrix} attribute contains the model matrix that was used for power evaluation. This is
#'especially useful if you want to specify the anticipated coefficients to use for power evaluation. The model
#'matrix provides the order of the model coefficients, as well as the
#'encoding used for categorical factors.
#'@details This function evaluates the power of experimental designs.
#'
#'Power is calculated under a linear regression framework: you intend to fit a
#'linear model to the data, of the form
#'
#'\eqn{y = X \beta + \epsilon}, (plus blocking terms, if applicable)
#'
#'where \eqn{y} is the vector of experimental responses, \eqn{X} is the model matrix, \eqn{\beta} is
#'the vector of model coefficients, and \eqn{\epsilon} is the statistical noise. \code{eval_design}
#'assumes that \eqn{\epsilon} is
#'normally distributed with zero mean and unit variance (root-mean-square error is 1), and
#'calculates both parameter power and effect power.
#'Parameter power is the probability of rejecting the hypothesis \eqn{\beta_i = 0}, where \eqn{\beta_i} is a single parameter
#'in the model,
#'while effect power is the probability of rejecting the hypothesis \eqn{\beta_{i} = 0}, where \eqn{\beta_{i}} is the set of all
#'parameters associated with the effect in question. The two powers are equivalent for continuous factors and
#'two-level categorical factors, but they can be different for categorical factors with three or more levels.
#'
#'When using \code{conservative = TRUE}, \code{eval_design} first evaluates the power with default coefficients. Then,
#'for each multi-level categorical, it sets all coefficients to zero except the level that produced the lowest power,
#'and then re-evaluates the power with this modified set of anticipated coefficients.
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
#'#(Setting detailedoutput = TRUE provides information on the anticipated
#'#coefficients that were used:)
#'eval_design(designcoffee, model = ~cost + size + type, alpha = 0.05, detailedoutput = TRUE)
#'
#'#Evaluate the design, with conservative anticipated coefficients:
#'eval_design(designcoffee, model = ~cost + size + type, alpha = 0.05, detailedoutput = TRUE,
#'             conservative = TRUE)
#'
#'#which is the same as the following, but now explicitly entering the coefficients:
#'eval_design(designcoffee, model = ~cost + size + type, alpha = 0.05,
#'             anticoef = c(1, 1, 1, 0, 0, 1, 0), detailedoutput = TRUE)
#'
#'
#'#If the defaults do not suit you, enter the anticipated coefficients in manually.
#'eval_design(designcoffee,
#'            model = ~cost + size + type, alpha = 0.05, anticoef = c(1, 1, 0, 0, 1, 0, 1))
#'
#'#You can also evaluate the design with higher order effects, even if they were not
#'#used in design generation:
#'eval_design(designcoffee, model = ~cost + size + type + cost * type, alpha = 0.05)
#'
#'#Split plot designs can also be evaluated by setting the blocking parameter as TRUE.
#'
#'#Generating split plot design
#'splitfactorialcoffee = expand.grid(caffeine = c(1, -1),
#'                                   cost = c(1, 2),
#'                                   type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
#'                                   size = as.factor(c("Short", "Grande", "Venti")))
#'
#'coffeeblockdesign = gen_design(splitfactorialcoffee, ~caffeine, trials = 12)
#'coffeefinaldesign = gen_design(splitfactorialcoffee,
#'                               model = ~caffeine + cost + size + type, trials = 36,
#'                               splitplotdesign = coffeeblockdesign, splitplotsizes = 3)
#'
#'#Evaluating design
#'eval_design(coffeefinaldesign, ~cost + size + type + caffeine, 0.2, blocking = TRUE)
#'
#'#We can also evaluate the design with a custom ratio between the whole plot error to
#'#the run-to-run error.
#'eval_design(coffeefinaldesign, ~caffeine + cost + size + type + caffeine, 0.2, blocking = TRUE,
#'             varianceratios = 2)
#'
#'#If the design was generated outside of skpr and thus the row names do not have the
#'#blocking structure encoded already, the user can add these manually. For a 12-run
#'#design with 4 blocks, the rownames will be as follows:
#'
#'manualrownames = paste(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4), rep(c(1, 2, 3), 4), sep = ".")
#'
#'#If we wanted to add this blocking structure to the design coffeeblockdesign, we would
#'#simply set the rownames to this character vector:
#'
#'rownames(coffeeblockdesign) = manualrownames
#'
#'#Deeper levels of blocking can be specified with additional periods.
eval_design = function(design, model, alpha, blocking = FALSE, anticoef = NULL,
                       effectsize = 2, varianceratios = 1,
                       contrasts = contr.sum, conservative = FALSE,
                       detailedoutput = FALSE, ...) {
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
  nointercept = attr(stats::terms.formula(model,data=design),"intercept") == 0
  #covert tibbles
  run_matrix_processed = as.data.frame(design)

  #Detect externally generated blocking columns and convert to rownames
  run_matrix_processed = convert_blockcolumn_rownames(run_matrix_processed, blocking, varianceratios)
  zlist = attr(run_matrix_processed,"z.matrix.list")

  #Remove skpr-generated REML blocking columns if present
  run_matrix_processed = remove_skpr_blockcols(run_matrix_processed)

  #----- Convert dots in formula to terms -----#
  model = convert_model_dots(run_matrix_processed,model)

  #----- Rearrange formula terms by order -----#
  model = rearrange_formula_by_order(model)
  if(nointercept) {
    attr(stats::terms.formula(model),"intercept") = 0
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
    default_coef = gen_anticoef(run_matrix_processed, model)
    anticoef = anticoef_from_delta(default_coef, effectsize, "gaussian")
    if (nointercept) {
      anticoef = anticoef[-1]
    }
  }
  if (length(anticoef) != dim(attr(run_matrix_processed, "modelmatrix"))[2]) {
    stop("Wrong number of anticipated coefficients")
  }


  #-----Generate V inverse matrix-----X
  #Variables used later: V, vinv
  if (blocking) {

    blocklist = strsplit(rownames(run_matrix_processed), ".", fixed = TRUE)

    existingBlockStructure = do.call(rbind, blocklist)
    blockgroups = apply(existingBlockStructure, 2, blockingstructure)

    blockMatrixSize = nrow(run_matrix_processed)
    V = diag(blockMatrixSize)
    blockcounter = 1
    if (length(blockgroups) == 1 | is.matrix(blockgroups)) {
      stop("No blocking detected. Specify block structure in row names or set blocking = FALSE")
    }
    if (length(blockgroups) > 2 && length(varianceratios) == 1) {
      varianceratios = rep(varianceratios, length(blockgroups) - 1)
    }
    if (length(blockgroups) > 2 && length(varianceratios) != 1 && length(blockgroups) - 1 != length(varianceratios)) {
      stop("Wrong number of variance ratio specified. Either specify value for all blocking levels or one value for all blocks.")
    }
    blockgroups = blockgroups[-length(blockgroups)]
    for (block in blockgroups) {
      V[1:block[1], 1:block[1]] =  V[1:block[1], 1:block[1]] + varianceratios[blockcounter]
      placeholder = block[1]
      for (i in 2:length(block)) {
        V[(placeholder + 1):(placeholder + block[i]), (placeholder + 1):(placeholder + block[i])] =
          V[(placeholder + 1):(placeholder + block[i]), (placeholder + 1):(placeholder + block[i])] + varianceratios[blockcounter]
        placeholder = placeholder + block[i]
      }
      blockcounter = blockcounter + 1
    }
    vinv = solve(V)
    #Below code detects the split-plot columns, and adjusts the degrees of freedom

    numberblocks = c(1,unlist(lapply(blockgroups,length)),nrow(run_matrix_processed))
    lowest_level = length(lapply(blockgroups,length)) + 1

    splitlayer = rep(lowest_level,ncol(run_matrix_processed))
    for(i in 1:length(blockgroups)) {
      currentrow = 1
      isblockcol = rep(TRUE,ncol(run_matrix_processed))
      temp_block_str = blockgroups[[i]]
      for(j in 1:length(temp_block_str)) {
        if(temp_block_str[j] - 1 > 0) {
          for(k in 0:(temp_block_str[j] - 2)) {
            isblockcol = isblockcol & run_matrix_processed[currentrow + k,] == run_matrix_processed[currentrow + 1 + k,]
          }
        }
        currentrow = currentrow + temp_block_str[j]
      }
      splitlayer[isblockcol & splitlayer == lowest_level] = i
    }
    isblockinglayer = rep(FALSE,length(numberblocks))
    isblockinglayer[splitlayer+1] = TRUE
    if (!nointercept) {
      m = c()
      currentlayer = 1
      for(i in 1:(max(splitlayer)+1)) {
        if(isblockinglayer[i]) {
          layercols = which(currentlayer == splitlayer+1)
          mtemp = numberblocks[i] - numberblocks[i-1]
          for(i in 1:length(layercols)) {
            if(is.numeric(run_matrix_processed[,layercols[i]])) {
              mtemp = mtemp - 1
            } else {
              cat_degrees = length(unique(run_matrix_processed[,layercols[i] ])) - 1
              mtemp = mtemp - cat_degrees
            }
          }
          m = c(m,mtemp)
          currentlayer = currentlayer + 1
        } else {
          if(i == 1) {
            m = c(m,numberblocks[i])
          } else {
            m = c(m,numberblocks[i]-numberblocks[i-1])
          }
          currentlayer = currentlayer + 1
        }
      }
    } else {
      m = c()
      currentlayer = 1
      for(i in 1:(max(splitlayer)+1)) {
        if(isblockinglayer[i]) {
          layercols = which(currentlayer == splitlayer+1)
          mtemp = numberblocks[i] - numberblocks[i-1]
          for(i in 1:length(layercols)) {
            if(is.numeric(run_matrix_processed[,layercols[i]])) {
              mtemp = mtemp - 1
            } else {
              cat_degrees = length(unique(run_matrix_processed[,layercols[i] ])) - 1
              mtemp = mtemp - cat_degrees
            }
          }
          m = c(m,mtemp)
          currentlayer = currentlayer + 1
        } else {
          if(i == 1) {
            m = c(m,0)
          } else {
            m = c(m,numberblocks[i]-numberblocks[i-1])
          }
          currentlayer = currentlayer + 1
        }
      }
    }
    degrees_of_freedom = m[isblockinglayer]
    degrees_of_freedom = calc_interaction_degrees(run_matrix_processed, model, contrasts, splitlayer, degrees_of_freedom)
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
  #This returns if everything is continuous (no categorical)
  if (!any(table(attr(attr(run_matrix_processed, "modelmatrix"), "assign")[-1]) != 1)) {
    factornames = attr(terms(model), "term.labels")
    factormatrix = attr(terms(model), "factors")
    interactionterms = factornames[apply(factormatrix, 2, sum) > 1]
    higherorderterms = factornames[!(gsub("`", "", factornames, fixed = TRUE) %in% colnames(run_matrix_processed)) &
                                     !(apply(factormatrix, 2, sum) > 1)]
    levelvector = sapply(lapply(run_matrix_processed, unique), length)
    levelvector[lapply(run_matrix_processed, class) == "numeric"] = 2
    if ("(Intercept)" %in% colnames(attr(run_matrix_processed, "modelmatrix"))) {
      levelvector = c(1, levelvector - 1)
    } else {
      levelvector = levelvector - 1
    }
    higherorderlevelvector = rep(1, length(higherorderterms))
    names(higherorderlevelvector) = higherorderterms
    levelvector = c(levelvector, higherorderlevelvector)
    effectresults = rep(parameterpower(run_matrix_processed, levelvector, anticoef, alpha,
                                       vinv = vinv, degrees = degrees_of_freedom,parameter_names = parameter_names), 2)
    typevector = c(rep("effect.power", length(effectresults) / 2), rep("parameter.power", length(effectresults) / 2))
    namevector = rep(colnames(attr(run_matrix_processed, "modelmatrix")), 2)

    results = data.frame(parameter = namevector, type = typevector, power = effectresults)

    attr(results, "modelmatrix") = attr(run_matrix_processed, "modelmatrix")
    attr(results, "anticoef") = anticoef

    modelmatrix_cor = model.matrix(model, run_matrix_processed, contrasts.arg = contrastslist_cormat)
    if (ncol(modelmatrix_cor) > 2) {
      correlation.matrix = abs(cov2cor(t(modelmatrix_cor) %*% modelmatrix_cor)[-1, -1])
      colnames(correlation.matrix) = colnames(modelmatrix_cor)[-1]
      rownames(correlation.matrix) = colnames(modelmatrix_cor)[-1]
      attr(results, "correlation.matrix") = round(correlation.matrix, 8)
    }

    attr(results, "generating.model") = model
    attr(results, "runmatrix") = run_matrix_processed

    levelvector = sapply(lapply(run_matrix_processed, unique), length)
    classvector = sapply(lapply(run_matrix_processed, unique), class) == "factor"
    mm = gen_momentsmatrix(colnames(attr(run_matrix_processed, "modelmatrix")), levelvector, classvector)

    attr(results, "moment.matrix") = mm
    attr(results, "A") = AOptimality(attr(run_matrix_processed, "modelmatrix"))

    if (!blocking) {
      attr(results, "variance.matrix") = diag(nrow(modelmatrix_cor))
      attr(results, "I") = IOptimality(modelmatrix_cor, momentsMatrix = mm,
                                       blockedVar = diag(nrow(modelmatrix_cor)))
      attr(results, "D") = 100 * DOptimality(modelmatrix_cor) ^ (1 / ncol(modelmatrix_cor)) / nrow(modelmatrix_cor)
    } else {
      attr(results, "z.matrix.list") = zlist
      attr(results, "variance.matrix") = V
      attr(results, "I") = IOptimality(modelmatrix_cor, momentsMatrix = mm, blockedVar = V)
      attr(results, "D") = 100 * DOptimalityBlocked(modelmatrix_cor, blockedVar = V) ^ (1 / ncol(modelmatrix_cor)) / nrow(modelmatrix_cor)
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


    return(results)
  } else {
    factornames = attr(terms(model), "term.labels")
    factormatrix = attr(terms(model), "factors")
    interactionterms = factornames[apply(factormatrix, 2, sum) > 1]
    higherorderterms = factornames[!(gsub("`", "", factornames, fixed = TRUE) %in% colnames(run_matrix_processed)) &
                                   !(apply(factormatrix, 2, sum) > 1)]
    levelvector = sapply(lapply(run_matrix_processed, unique), length)
    levelvector[lapply(run_matrix_processed, class) == "numeric"] = 2
    if ("(Intercept)" %in% colnames(attr(run_matrix_processed, "modelmatrix"))) {
      levelvector = c(1, levelvector - 1)
    } else {
      levelvector = levelvector - 1
    }
    higherorderlevelvector = rep(1, length(higherorderterms))
    names(higherorderlevelvector) = higherorderterms
    levelvector = c(levelvector, higherorderlevelvector)

    for (interaction in interactionterms) {
      numberlevels = 1
      for (term in unlist(strsplit(interaction, split = "(\\s+)?:(\\s+)?|(\\s+)?\\*(\\s+)?"))) {
        numberlevels = numberlevels * levelvector[gsub("`", "", term, fixed = TRUE)]
      }
      levelvector = c(levelvector, numberlevels)
    }

    effectresults = effectpower(run_matrix_processed, levelvector, anticoef,
                                alpha, vinv = vinv, degrees = degrees_of_freedom)
    parameterresults = parameterpower(run_matrix_processed, levelvector, anticoef,
                                      alpha, vinv = vinv, degrees = degrees_of_freedom, parameter_names = parameter_names)

    typevector = c(rep("effect.power", length(effectresults)), rep("parameter.power", length(parameterresults)))
    if ("(Intercept)" %in% colnames(attr(run_matrix_processed, "modelmatrix"))) {
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
      if ("(Intercept)" %in% colnames(modelmatrix_cor)) {
        correlation.matrix = abs(cov2cor(solve(t(modelmatrix_cor) %*% solve(V) %*% modelmatrix_cor))[-1, -1])
        colnames(correlation.matrix) = colnames(modelmatrix_cor)[-1]
        rownames(correlation.matrix) = colnames(modelmatrix_cor)[-1]
      } else {
        correlation.matrix = abs(cov2cor(solve(t(modelmatrix_cor) %*% solve(V) %*% modelmatrix_cor)))
        colnames(correlation.matrix) = colnames(modelmatrix_cor)
        rownames(correlation.matrix) = colnames(modelmatrix_cor)
      }

      attr(results, "correlation.matrix") = round(correlation.matrix, 8)
    }
    attr(results, "generating.model") = model
    attr(results, "run.matrix") = run_matrix_processed
    attr(results, "model.matrix") = modelmatrix_cor

    levelvector = sapply(lapply(run_matrix_processed, unique), length)
    classvector = sapply(lapply(run_matrix_processed, unique), class) == "factor"
    mm = gen_momentsmatrix(colnames(attr(run_matrix_processed, "modelmatrix")), levelvector, classvector)

    attr(results, "moment.matrix") = mm
    attr(results, "A") = AOptimality(attr(run_matrix_processed, "modelmatrix"))

    if (!blocking) {
      attr(results, "variance.matrix") = diag(nrow(modelmatrix_cor))
      attr(results, "I") = IOptimality(modelmatrix_cor, momentsMatrix = mm, blockedVar = diag(nrow(modelmatrix_cor)))
      attr(results, "D") = 100 * DOptimality(modelmatrix_cor) ^ (1 / ncol(modelmatrix_cor)) / nrow(modelmatrix_cor)
    } else {
      attr(results, "z.matrix.list") = zlist
      attr(results, "variance.matrix") = V
      attr(results, "I") = IOptimality(modelmatrix_cor, momentsMatrix = mm, blockedVar = V)
      attr(results, "D") = 100 * DOptimalityBlocked(modelmatrix_cor, blockedVar = V) ^ (1 / ncol(modelmatrix_cor)) / nrow(modelmatrix_cor)
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

    if (conservative == TRUE) {
      #at this point, since we are going to specify anticoef, do not use the effectsize argument
      #in the subsequent call. Do replicate the magnitudes from the original anticoef
      conservative_anticoef = calc_conservative_anticoef(results,effectsize)
      results = eval_design(design = design, model = model, alpha = alpha, blocking = blocking,
                  anticoef = conservative_anticoef,
                  detailedoutput = detailedoutput,
                  varianceratios = varianceratios, contrasts = contrasts, conservative = FALSE)
    }
    return(results)
  }
}
