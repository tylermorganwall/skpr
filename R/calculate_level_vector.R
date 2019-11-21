#'@title Calculate level vector
#'
#'@description Calculate level vector
#'
#'@param design Design to be analyzed.
#'@param model The model.
#'@param nointercept Whether an intercept is included.
#'@return A vector of levels.
#'@keywords internal
#'@examples
#'#We can pass either the output of gen_design or eval_design to plot_correlations
calculate_level_vector = function(design, model, nointercept) {
  factornames = attr(terms(model), "term.labels")
  factormatrix = attr(terms(model), "factors")
  interactionterms = factornames[apply(factormatrix, 2, sum) > 1]
  higherorderterms = factornames[!(gsub("`", "", factornames, fixed = TRUE) %in% colnames(design)) &
                                   !(apply(factormatrix, 2, sum) > 1)]
  levelvector = sapply(lapply(design, unique), length)
  levelvector[lapply(design, class) == "numeric"] = 2
  if (!nointercept) {
    levelvector = c(1, levelvector - 1)
  } else {
    levelvector = levelvector - 1
    for (i in 1:ncol(design)) {
      if (class(design[, i]) %in% c("character", "factor")) {
        levelvector[i] = levelvector[i] + 1
        break
      }
    }
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
  levelnames = names(levelvector)
  if(length(interactionterms) > 0) {
    levelnames[(length(levelnames)-length(interactionterms)+1):length(levelnames)] = interactionterms
  }
  levelvector = stats::setNames(levelvector, levelnames)
  levelvector
}

