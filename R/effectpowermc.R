#'@title Fit Anova for Effect Power Calculation in Monte Carlo
#'
#'@description Calculates the p-values for the effect power calculation in Monte Carlo
#'
#'@param fit Fit from regression
#'@param type Default `III`
#'@param test Default `Pr(>Chisq)`.
#'@param ... Additional arguments to pass to car::Anova
#'@return p-values
#'@keywords internal
effectpowermc = function(fit, type="III", test = "Pr(>Chisq)",
                         model_formula = NULL, firth = FALSE, glmfamily = "gaussian", effect_terms = NULL,
                         RunMatrixReduced = NULL, method = NULL, contrastslist = contrastslist,
                         effect_anova = FALSE, ...) {
  output = new.env(parent = emptyenv())
  output$effectnames = NA
  if(glmfamily != "binomial") {
    firth = FALSE
  }
  if (class(fit)[1] == "lmerModLmerTest") {
    test = "Pr(>F)"
    anovafit = suppressMessages(anova(fit, type = type, ... ))
    output$effectnames = rownames(anovafit)
    output$effect_pvals = as.vector(as.matrix(anovafit[test]))
  } else if (!effect_anova || (firth && glmfamily == "binomial")) {
    if(!(length(find.package("lmtest", quiet = TRUE)) > 0)) {
      stop("{lmtest} package required when specifying `effect_anova = FALSE` ",
           "to use a likelihood ratio test for effect power")
    }
    if(effect_anova) {
      warning(r"(skpr uses a likelihood ratio test (instead of a type-III ANOVA) for",
      "effect power when `firth = TRUE` and `glmfamily = "binomial"`: setting `effect_lr = TRUE`.)")
    }
    if(df.residual(fit) == 0) {
      stop("skpr: Model saturated--no degrees of freedom to estimate power.")
    }
    fterms = terms.formula(model_formula)
    factor_matrix       = attr(fterms,"factors")
    factor_terms = attr(fterms,"term.labels")
    term_order = attr(fterms,"order")
    if(any(term_order > 1)) {
      warning("skpr: For effect power calculated via a likelihood ratio test, power estimates ",
              "for lower-order terms are calculated independently of higher-order interactions. ",
              "This is because interaction effects cannot be estimated without the lower-order ",
              "terms, and thus estimates from the reduced and full models are effectively the same. ",
              "This means that the effect power ",
              "values reported for lower-order terms do not account for higher-order interactions, ",
              "and could potentially be lower ",
              "if those higher-order terms are included during the actual analysis.")
    }
    if(min(term_order) != 1) {
      stop("skpr: No main effect terms found--this model will not produce well-formed power estimates.")
    }
    term_order_list = split(factor_terms, term_order)
    hierarchy_terms = lapply(term_order_list, paste, collapse = " + ")
    output$effect_pvals = rep(1,length(factor_terms))
    output$effectnames = factor_terms
    higher_order_terms = term_order_list
    model_hierarchy = vector("list", length = length(hierarchy_terms))
    model_hierarchy[[1]] = sprintf("Y ~ %s", hierarchy_terms[[1]])
    if(max(term_order) > 1) {
      for(ii in seq_len(length(model_hierarchy))[[-1]]) {
        model_hierarchy[[ii]] = as.formula(sprintf("%s + %s", model_hierarchy[[ii-1]],  hierarchy_terms[[ii]]),
                                         env = environment(model_formula))

      }
    }
    for(k in seq_len(length(factor_terms))) {
      current_model = model_hierarchy[[ term_order[k] ]]
      new_formula         = update.formula(current_model,
                                           as.formula(sprintf("Y ~ . - %s",factor_terms[k]),
                                                      env = environment(model_formula)))
      if(glmfamily == "gaussian") {
        fit_reduced = suppressWarnings(suppressMessages({
          lm(new_formula, data = RunMatrixReduced, contrasts = contrastslist)
        }))
      } else {
        fit_reduced = suppressWarnings(suppressMessages({
          glm(new_formula, family = glmfamily, data = RunMatrixReduced, contrasts = contrastslist, method = method)
        }))
      }
      lr_results = lmtest::lrtest(fit, fit_reduced)
      if(lr_results$`#Df`[1] != lr_results$`#Df`[2]) {
        output$effect_pvals[k] = lr_results$`Pr(>Chisq)`[2]
      } else {
        output$effect_pvals[k] = 1.0
      }
    }
  } else {
    tryCatch({
      anovafit = suppressWarnings(
        suppressMessages(
          car::Anova(fit, type = type, ... )
        )
      )
      output$effectnames = rownames(anovafit)
      output$effect_pvals = as.vector(as.matrix(anovafit[test]))
    }, error = function(e) {
      if(any(grepl("residual sum of squares", as.character(e)))) {
        fit2 = fit
        fit2$residuals = fit2$residuals + 1
        anovafit2 = suppressWarnings(
          suppressMessages(
            car::Anova(fit2, type = type, ... )
          )
        )
        output$effectnames = rownames(anovafit2)
        output$effect_pvals = rep(1,length(output$effectnames))
      } else {
        output$effectnames = rownames(coef(summary(fit)))
        output$effect_pvals = rep(NA,length(output$effectnames))
      }
      return(e)
    })
  }
  if (all(is.na(output$effectnames))) {
    if(df.residual(fit) == 0) {
      stop("skpr: Model saturated--no residual degrees of freedom to fit the model and estimate power.")
    } else {
      stop("skpr: Effect power not supported for fit type: ", class(fit))
    }
  }
  if ("Residuals" %in% output$effectnames) {
    output$effect_pvals = output$effect_pvals[output$effectnames != "Residuals"]
    names(output$effect_pvals) = output$effectnames[output$effectnames != "Residuals"]
  } else {
    names(output$effect_pvals) = output$effectnames
  }
  output$effect_pvals
}
