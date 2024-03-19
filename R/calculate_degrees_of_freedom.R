#'@title Calculate Degrees of Freedom
#'
#'@description Calculates the degrees of freedom adjustment for models with random effects from Penheiro and Bates, pg. 91
#'@param run_matrix_processed The design
#'@param nointercept Whether the intercept term is present
#'@return Vector of numbers indicating split plot layers
#'@keywords internal
calculate_degrees_of_freedom = function(run_matrix_processed, nointercept, model, contrasts) {
  blocklist = strsplit(rownames(run_matrix_processed), ".", fixed = TRUE)
  existingBlockStructure = do.call(rbind, blocklist)
  blockgroups = get_block_groups(existingBlockStructure)
  # Check block nesting structure
  block_nesting = c(calculate_block_nesting(blockgroups, existingBlockStructure))
  blockgroups = blockgroups[-length(blockgroups)]
  #Check if all blocks are uniquely nested--if not, then it does not have a split-plot structure
  if(length(unique(block_nesting)) == length(block_nesting)) {
    split_plot_blocking = TRUE
  } else {
    split_plot_blocking = FALSE
    stop("All blocks are not uniquely nested: design has a random block structure. ",
          "Parametric evaluation may not accurately assess the denominator degrees of freedom: ",
          "use `eval_design_mc(..., blocking = TRUE, adjust_alpha_inflation = TRUE)`",
          "for an accurate estimate of power.")
  }
  splitlayerlist = calculate_split_columns(run_matrix_processed, blockgroups, existingBlockStructure)

  if (!nointercept) {
    numberblocks = c(1, unlist(lapply(blockgroups, length)), nrow(run_matrix_processed))
  } else {
    numberblocks = c(unlist(lapply(blockgroups, length)), nrow(run_matrix_processed))
  }
  model = as.formula(paste0("~", paste(attr(terms.formula(model), "term.labels"), collapse = " + ")))
  splitlayer = splitlayerlist$splitlayer
  #`allsplit` determines if it's a split-plot or blocking structure by seeing if all the blocks are nested
  split_plot_structure = splitlayerlist$allsplit
  #And here is the level which each layer is nested under
  nested_level = c(0,splitlayerlist$nested_level + 1)
  splitterms = unlist(strsplit(as.character(model)[-1], split = " + ", fixed = TRUE))
  splitterms = gsub("\\n","",splitterms,perl=TRUE) #Remove newlines inserted when model character string too long
  splitterms = trimws(splitterms)
  interactions = is_intralayer_interaction(run_matrix_processed, model, splitlayer)

  if (!nointercept) {
    m = 1
    currentlayer = 1
    for (i in 2:(max(splitlayer) + 1)) {
      interaction_cols_layer = interactions[[currentlayer]]
      names(interaction_cols_layer) = splitterms
      layercols = which(currentlayer == splitlayer)
      #Total number of potential degrees of freedom for this layer is the number of blocks
      #minus the number of blocks from the previous layer.
      mtemp = numberblocks[i] - numberblocks[i-1]

      #Subtract out degrees of freedom for terms nested within this split-plot layer
      if(split_plot_structure) {
        if (length(layercols) > 0) {
          for (j in seq_len(length(layercols))) {
            if (is.numeric(run_matrix_processed[, layercols[j], drop = FALSE])) {
              mtemp = mtemp - 1
            } else {
              cat_degrees = length(unique(run_matrix_processed[, layercols[j], drop = FALSE ])) - 1
              mtemp = mtemp - cat_degrees
            }
          }
        }
        #Now we have to deal with interaction terms. These can either be interactions between
        #terms fully contained within a layer (and thus will account for degrees of freedom
        #subtracted within that layer) or they will be an interaction with a term nested in a
        #higher layer (for which they will also count) or with a deeper layer (for which they
        #will not count--those degrees of freedom will be accounted for when we calculate the
        #df for that layer).
        for (j in seq_len(length(interaction_cols_layer))) {
          if (interaction_cols_layer[j]) {
            interaction_cols = unlist(strsplit(splitterms[j], ":"))
            higherorder_terms = unlist(grepl("I\\((.+)\\^.+\\)", interaction_cols, perl = TRUE))
            if(any(higherorder_terms)) {
              for(term in 1:length(higherorder_terms)) {
                if(higherorder_terms[term]) {
                  interaction_cols[term] = gsub("I\\(","",interaction_cols[term],perl=TRUE)
                  interaction_cols[term] = gsub("\\^.+\\)","",interaction_cols[term],perl=TRUE)
                }
              }
            }
            #Numeric factors get 1 df automatcally, otherwise count number of levels of
            #categorical factors minus 1 for the df. Multiply these numbers together
            #e.g. two 3 level factors interacting = 1*(3-1)*(3-1) = 4 df total.
            temp_degrees = 1
            for (col_name in interaction_cols) {
              if (!is.numeric(run_matrix_processed[, col_name, drop = FALSE ])) {
                temp_degrees = temp_degrees * (length(unique(run_matrix_processed[, col_name, drop = FALSE ])) - 1)
              }
            }
            #Subtract from mtemp for this layer.
            mtemp = mtemp - temp_degrees
          }
        }
      }
      #Record current degrees of freedom and move to the next layer
      m = c(m, mtemp)
      currentlayer = currentlayer + 1
    }
  } else {
    #Subtract out degrees of freedom for terms nested within the first layer,
    #Which is slightly different in the no intercept case.

    currentlayer = 1
    layercols = which(currentlayer == splitlayer)
    mtemp = numberblocks[1]
    if(split_plot_structure) {
      if (length(layercols) > 0) {
        for (j in seq_len(length(layercols))) {
          if (is.numeric(run_matrix_processed[, layercols[j], drop = FALSE])) {
            mtemp = mtemp - 1
          } else {
            cat_degrees = length(unique(run_matrix_processed[, layercols[j], drop = FALSE ])) - 1
            mtemp = mtemp - cat_degrees
          }
        }
      }
    }
    m = mtemp
    for (i in 2:(max(splitlayer))) {
      interaction_cols_layer = interactions[[currentlayer]]

      layercols = which(currentlayer == splitlayer)
      #Total number of potential degrees of freedom for this layer is the number of blocks
      #minus the number of blocks from the previous layer.

      mtemp = numberblocks[i] - numberblocks[i - 1]
      #Subtract out degrees of freedom for terms nested within split-plot layers
      if(split_plot_structure) {
        if (length(layercols) > 0) {
          for (j in seq_len(length(layercols))) {
            if (is.numeric(run_matrix_processed[, layercols[j], drop = FALSE])) {
              mtemp = mtemp - 1
            } else {
              cat_degrees = length(unique(run_matrix_processed[, layercols[j], drop = FALSE ])) - 1
              mtemp = mtemp - cat_degrees
            }
          }
        }
      }
      #Now we have to deal with interaction terms. These can either be interactions between
      #terms fully contained within a layer (and thus will account for degrees of freedom
      #subtracted within that layer) or they will be an interaction with a term nested in a
      #higher layer (for which they will also count) or with a deeper layer (for which they
      #will not count--those degrees of freedom will be accounted for when we calculate the
      #df for that layer).
      for (j in seq_len(length(interaction_cols_layer))) {
        if (interaction_cols_layer[j]) {
          interaction_cols = unlist(strsplit(splitterms[j], ":"))
          higherorder_terms = unlist(grepl("I\\((.+)\\^.+\\)", interaction_cols, perl = TRUE))
          if(any(higherorder_terms)) {
            for(term in 1:length(higherorder_terms)) {
              if(higherorder_terms[term]) {
                interaction_cols[term] = gsub("I\\(","",interaction_cols[term],perl=TRUE)
                interaction_cols[term] = gsub("\\^.+\\)","",interaction_cols[term],perl=TRUE)
              }
            }
          }
          #Numeric factors get 1 df automatcally, otherwise count number of levels of
          #categorical factors minus 1 for the df. Multiply these numbers together
          #e.g. two 3 level factors interacting = 1*(3-1)*(3-1) = 4 df total.
          temp_degrees = 1
          for (col_name in interaction_cols) {
            if (!is.numeric(run_matrix_processed[, col_name, drop = FALSE ])) {
              temp_degrees = temp_degrees * (length(unique(run_matrix_processed[, col_name, drop = FALSE ])) - 1)
            }
          }
          #Subtract from mtemp for this layer.
          mtemp = mtemp - temp_degrees
        }
      }
      m = c(m, mtemp)
      currentlayer = currentlayer + 1
    }
  }
  degrees_of_freedom = calc_interaction_degrees(run_matrix_processed, model, contrasts, nointercept,
                                                splitlayer, m, split_plot_structure)
  if (!nointercept) {
    # From Pinheiro and Bates, pp. 91:
    # "The intercept, which is the parameter corresponding to the column of 1’s
    # in the model matrices Xi, is treated differently from all the other param-
    # eters, when it is present. As a parameter it is regarded as being estimated
    # at level 0 because it is outer to all the grouping factors. However, its de-
    # nominator degrees of freedom are calculated as if it were estimated at level
    # Q + 1. This is because the intercept is the one parameter that pools
    # information from all the observations at a level even when the corresponding
    # column in Xi doesn’t change with the level."
    degrees_of_freedom[1] = min(degrees_of_freedom[-1])
  }
  return(degrees_of_freedom)
}
