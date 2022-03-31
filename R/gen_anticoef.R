#'@title Generates Anticipated Coefficients
#'
#'@description Generates Anticipated Coefficients
#'
#'@param RunMatrix The run matrix
#'@param model The model
#'@param nointercept TRUE if intercept not in model
#'@return Anticipated coefficients.
#'@keywords internal
gen_anticoef = function(RunMatrix, model, nointercept) {
  #calculate levels for anticipated coefficients, with or without higher order effects
  if (nointercept) {
    first_intercept_cat = TRUE
  } else {
    first_intercept_cat = FALSE
  }
  levels = sapply(lapply(RunMatrix, unique), length) - 1
  type = sapply(RunMatrix, class)
  notlinear = attr(terms(model), "order") > 1
  notlinear_notinteraction = grepl("^", attr(terms(model), "term.labels"), fixed = TRUE) & !grepl(":", attr(terms(model), "term.labels"), fixed = TRUE)
  higherorder = attr(terms(model), "term.labels")[notlinear_notinteraction]
  levels = c(levels, rep(1, length(higherorder)))
  type = c(type, rep("numeric", length(higherorder)))
  nonlinearterms = attr(terms(model), "term.labels")[notlinear]
  for (term in nonlinearterms) {
    higherlevel = 1
    highertype = "numeric"
    factors = gsub("`", "", strsplit(term, ":")[[1]], fixed = TRUE)
    for (i in factors) {
      if (inherits(RunMatrix[[i]], c("character", "factor"))) {
        higherlevel = levels[i] * higherlevel
        highertype = "factor"
      }
    }
    levels = c(levels, higherlevel)
    type = c(type, highertype)
  }

  anticoef = c(1)

  for (i in 1:length(levels)) {
    if (type[i] %in% c("character", "factor") && levels[i] %% 2 == 0) {
      if (!first_intercept_cat) {
        anticoef = c(anticoef, rep(c(1, -1), levels[i] / 2))
      } else {
        anticoef = c(anticoef, 1, rep(c(-1, 1), levels[i] / 2))
        first_intercept_cat = FALSE
      }
    }
    if (type[i] %in% c("character", "factor") && levels[i] %% 2 == 1) {
      if (!first_intercept_cat) {
        anticoef = c(anticoef, 1, rep(c(-1, 1), (levels[i] - 1) / 2))
      } else {
        anticoef = c(anticoef, rep(c(1, -1), (levels[i] - 1) / 2))
        first_intercept_cat = FALSE
      }
    }
    if (type[i] == "numeric" || type[i] == "integer") {
      anticoef = c(anticoef, 1)
    }
  }
  return(anticoef)
}
