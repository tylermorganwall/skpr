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
effectpowermc = function(
  fit,
  type = "III",
  test = "Pr(>Chisq)",
  model_formula = NULL,
  firth = FALSE,
  glmfamily = "gaussian",
  effect_terms = NULL,
  RunMatrixReduced = NULL,
  method = NULL,
  contrastslist = contrastslist,
  effect_anova = FALSE,
  ...
) {
  output = new.env(parent = emptyenv())
  output$effectnames = NA
  if (glmfamily != "binomial") {
    firth = FALSE
  }
  if (class(fit)[1] == "lmerModLmerTest") {
    test = "Pr(>F)"
    anovafit = suppressMessages(anova(fit, type = type, ...))
    output$effectnames = rownames(anovafit)
    output$effect_pvals = as.vector(as.matrix(anovafit[test]))
  } else if (!effect_anova || (firth && glmfamily == "binomial")) {
    if (!(length(find.package("lmtest", quiet = TRUE)) > 0)) {
      stop(
        "{lmtest} package required when specifying `effect_anova = FALSE` ",
        "to use a likelihood ratio test for effect power"
      )
    }
    if (df.residual(fit) == 0) {
      stop("skpr: Model saturated--no degrees of freedom to estimate power.")
    }
    model_matrix = model.matrix(
      model_formula,
      RunMatrixReduced,
      contrasts.arg = contrastslist
    )
    effects_from_model = attr(model_matrix, "assign")
    effects_labeled = c(
      "(Intercept)",
      attr(terms.formula(model_formula), "term.labels")
    )
    unique_terms = unique(effects_from_model)
    names(unique_terms) = effects_labeled
    stopifnot(length(effects_labeled) == length(unique_terms))
    output$effectnames = effects_labeled
    output$effect_pvals = rep(NA, length(effects_labeled))
    # We need to use the lower-level glm.fit interface: the regular glm() interface re-parameterizes the model
    # when main effects aren't present, which means you can't test the significance of lower-level categorical
    # effects when higher-level interactions exist.
    for (i in seq_len(length(unique_terms))) {
      cols_to_remove = which(unique_terms[i] == effects_from_model)
      family = switch(
        glmfamily,
        "gaussian" = gaussian(),
        "binomial" = binomial(),
        "poisson" = poisson(),
        "gaussian" = Gamma(link = "log"),
        stop(sprintf("glm family `%s` not found.", glmfamily))
      )
      if (!firth) {
        fit_raw = glm.fit(
          x = model_matrix[, -cols_to_remove],
          y = fit$data$Y,
          family = family
        )
      } else {
        fit_raw = mbest::firthglm.fit(
          x = model_matrix[, -cols_to_remove],
          y = fit$data$Y,
          family = family
        )
      }
      fit_reduced = structure(fit_raw, class = c(fit$class, c("glm", "lm")))
      lr_results = lmtest::lrtest(fit, fit_reduced)
      if (lr_results$`#Df`[1] != lr_results$`#Df`[2]) {
        output$effect_pvals[i] = lr_results$`Pr(>Chisq)`[2]
      } else {
        output$effect_pvals[i] = 1.0
      }
    }
    stopifnot(all(!is.na(output$effect_pvals)))
  } else {
    tryCatch(
      {
        anovafit = suppressWarnings(
          suppressMessages(
            car::Anova(fit, type = type, ...)
          )
        )
        output$effectnames = rownames(anovafit)
        output$effect_pvals = as.vector(as.matrix(anovafit[test]))
      },
      error = function(e) {
        if (any(grepl("residual sum of squares", as.character(e)))) {
          fit2 = fit
          fit2$residuals = fit2$residuals + 1
          anovafit2 = suppressWarnings(
            suppressMessages(
              car::Anova(fit2, type = type, ...)
            )
          )
          output$effectnames = rownames(anovafit2)
          output$effect_pvals = rep(1, length(output$effectnames))
        } else {
          output$effectnames = rownames(coef(summary(fit)))
          output$effect_pvals = rep(NA, length(output$effectnames))
        }
        return(e)
      }
    )
  }
  if (all(is.na(output$effectnames))) {
    if (df.residual(fit) == 0) {
      stop(
        "skpr: Model saturated--no residual degrees of freedom to fit the model and estimate power."
      )
    } else {
      stop("skpr: Effect power not supported for fit type: ", class(fit))
    }
  }
  if ("Residuals" %in% output$effectnames) {
    output$effect_pvals = output$effect_pvals[output$effectnames != "Residuals"]
    names(output$effect_pvals) = output$effectnames[
      output$effectnames != "Residuals"
    ]
  } else {
    names(output$effect_pvals) = output$effectnames
  }
  output$effect_pvals
}
