#'@title Calculate Interaction Degrees of Freedom
#'
#'@description Calculate interaction degrees of freedom for split-plot designs
#'
#'@param design The design matrix
#'@param model The model
#'@param contrast The contrast
#'@param split_layers The layer of split plots for each main effect term
#'@param split_degrees The number of degrees of freedom for each main effect term
#'@return Degrees of freedom vector
#'@keywords internal
calc_interaction_degrees = function(design, model, contrast, nointercept,
                                    split_layers, split_degrees, split_plot_structure) {
  contrastslistspd = list()
  for (x in names(design[lapply(design, class) %in% c("factor", "character")])) {
    contrastslistspd[[x]] = contrast
  }

  names(split_layers) = colnames(design)

  if (length(contrastslistspd) == 0) {
    contrastslistspd = NULL
  }

  model = as.formula(paste0("~", paste(attr(terms.formula(model), "term.labels"), collapse = " + ")))
  if(!nointercept) {
    character_model = as.character(model)[-1]
  } else {
    character_model = paste(c(as.character(model)[-1]," + -1"), collapse="")
  }
  splitterms = unlist(strsplit(character_model, split = " + ", fixed = TRUE))
  ismaineffect = rep(FALSE, length(splitterms))
  ismaineffect[1:length(split_layers)] = TRUE
  names(ismaineffect) = splitterms
  interactions = list()
  if(max(split_layers) > 0) {
    for(i in 1:max(split_layers, na.rm = TRUE)) {
      wholeplotterms = colnames(design)[split_layers == i]
      wholeorwholeinteraction = rep(FALSE, length(splitterms))
      for (term in wholeplotterms) {
        regex = paste0("(\\b", term, "\\b)|(\\b", term, ":)|(:", term, "\\b)|(\\b", term, "\\s\\*)|(\\*\\s", term, "\\b)")
        wholeorwholeinteraction = wholeorwholeinteraction | grepl(regex, splitterms, perl = TRUE)
      }
      interactions[[i]] = wholeorwholeinteraction
    }
  } else {
    for(i in seq_along(1:max(split_layers, na.rm = TRUE))) {
      interactions[[i]] = FALSE
    }
  }
  if(!nointercept) {
    degrees_of_freedom = rep(min(split_degrees), length(splitterms)+1)
    for(i in 1:length(split_layers)) {
      degrees_of_freedom[i+1] = split_degrees[split_layers[i]+1]
    }
    names(degrees_of_freedom) = c("(Intercept)", splitterms)
  } else {
    degrees_of_freedom = rep(min(split_degrees), length(splitterms))
    for(i in 1:length(split_layers)) {
      degrees_of_freedom[i] = split_degrees[split_layers[i]]
    }
    names(degrees_of_freedom) = c(splitterms)
  }
  #get higher order terms
  for(term in colnames(design)) {
    arith_terms = splitterms[grepl(paste0("(I\\(", term, "\\^.+\\))"), splitterms, perl = TRUE)]
    for(arith_term in arith_terms) {
      degrees_of_freedom[arith_term] = degrees_of_freedom[term]
    }
  }
  #Effect terms
  for(i in seq_along(1:max(split_layers, na.rm = TRUE))) {
    subplotterms = colnames(design)
    subplotterms = subplotterms[!(subplotterms %in% colnames(design)[split_layers >= i])]
    wholeplotterms = colnames(design)[split_layers < i]

    regularmodel = rep(FALSE, length(splitterms))
    for (term in subplotterms) {
      regex = paste0("(\\b", term, "\\b)|(\\b", term, ":)|(:", term, "\\b)|(\\b", term, "\\s\\*)|(\\*\\s", term, "\\b)")
      regularmodel = regularmodel | grepl(regex, splitterms, perl = TRUE)
    }
    #get whole:whole interaction terms
    wholewholeinteractionterms = splitterms[!ismaineffect & interactions[[i]]]
    for(term in wholewholeinteractionterms) {
      subterms = unlist(strsplit(term, split = ":", fixed = TRUE))
      maxdegree = c()
      for(subterm in subterms) {
        if(!subterm %in% wholeplotterms) {
          maxdegree = max(c(maxdegree, degrees_of_freedom[subterm]))
        }
      }
      degrees_of_freedom[term] = maxdegree
    }
    #get whole:non-whole interaction terms
    wholeinteractionterms = splitterms[regularmodel & interactions[[i]]]
    for(term in wholeinteractionterms) {
      subterms = unlist(strsplit(term, split = ":", fixed = TRUE))
      maxdegree = c()
      for(subterm in subterms) {
        if(!subterm %in% wholeplotterms) {
          maxdegree = max(c(maxdegree, degrees_of_freedom[subterm]))
        }
      }
      degrees_of_freedom[term] = maxdegree
    }
  }
  degrees_of_freedom
}
