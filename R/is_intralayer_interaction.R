#'@title Layer Interaction
#'
#'@description Determines if a factor is a intra-layer interaction
#'
#'@param design The design matrix
#'@param model The model
#'@param split_layers The layer of split plots for each main effect term
#'@return List of booleans for each subplot strata layer
#'@keywords internal
#'
is_intralayer_interaction = function(design, model, split_layers) {
  model = as.formula(paste0("~", paste(attr(terms.formula(model), "term.labels"), collapse = " + ")))
  splitterms = unlist(strsplit(as.character(model)[-1], split = " + ", fixed = TRUE))
  ismaineffect = rep(FALSE, length(splitterms))
  ismaineffect[1:length(split_layers)] = TRUE
  interactions = list()
  if (max(split_layers) > 0) {
    for (i in 1:max(split_layers, na.rm = TRUE)) {
      wholeplotterms = colnames(design)[split_layers == i]
      wholeorwholeinteraction = rep(FALSE, length(splitterms))
      for (term in wholeplotterms) {
        regex = paste0("(\\b", term, "\\b)|(\\b", term, ":)|(:", term, "\\b)|(\\b", term, "\\s\\*)|(\\*\\s", term, "\\b)")
        wholeorwholeinteraction = wholeorwholeinteraction | grepl(regex, splitterms, perl = TRUE)
      }
      interactions[[i]] = wholeorwholeinteraction & !ismaineffect
    }
  } else {
    for(i in seq_along(1:max(split_layers, na.rm=TRUE))) {
      interactions[[i]] = FALSE
    }
  }
  interactions
}
