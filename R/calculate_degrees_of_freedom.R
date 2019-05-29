#'@title Calculate Degrees of Freedom
#'
#'@description Calculates the degrees of freedom adjustment for models with random effects from Penheiro and Bates, pg. 91
#'@param run_matrix_processed The design
#'@param nointercept Whether the intercept term is present
#'@return Vector of numbers indicating split plot layers
#'@keywords internal
calculate_degrees_of_freedom = function(run_matrix_processed, nointercept,model,contrasts) {
  blocklist = strsplit(rownames(run_matrix_processed), ".", fixed = TRUE)
  existingBlockStructure = do.call(rbind, blocklist)
  blockgroups = apply(existingBlockStructure, 2, blockingstructure)
  blockgroups = blockgroups[-length(blockgroups)]
  max_split_depth = length(blockgroups) + 1
  splitlayer = calculate_split_columns(run_matrix_processed,blockgroups)
  if(!nointercept) {
    numberblocks = c(1,unlist(lapply(blockgroups,length)),nrow(run_matrix_processed))
  } else {
    numberblocks = c(unlist(lapply(blockgroups,length)),nrow(run_matrix_processed))
  }
  model = as.formula(paste0("~", paste(attr(terms.formula(model), "term.labels"), collapse = " + ")))
  splitterms = unlist(strsplit(as.character(model)[-1], split = " + ", fixed = TRUE))
  interactions = is_intralayer_interaction(run_matrix_processed, model, splitlayer)
  if (!nointercept) {
    m = c(1)
    currentlayer = 1
    for(i in 2:(max(splitlayer)+1)) {
      interaction_cols_layer = interactions[[currentlayer]]
      layercols = which(currentlayer == splitlayer)
      mtemp = numberblocks[i] - numberblocks[i-1]
      if(length(layercols) > 0) {
        for(j in 1:length(layercols)) {
          if(is.numeric(run_matrix_processed[,layercols[j]])) {
            mtemp = mtemp - 1
          } else {
            cat_degrees = length(unique(run_matrix_processed[,layercols[j] ])) - 1
            mtemp = mtemp - cat_degrees
          }
        }
      }
      for(j in seq_len(length(interaction_cols_layer))) {
        if(interaction_cols_layer[j]) {
          interaction_cols = unlist(strsplit(splitterms[j],":"))
          temp_degrees = 1
          for(col_name in interaction_cols) {
            temp_degrees = temp_degrees*(length(unique(run_matrix_processed[,col_name ])) - 1)
          }
          mtemp = mtemp - temp_degrees
        }
      }
      m = c(m,mtemp)
      currentlayer = currentlayer + 1
    }
  } else {
    currentlayer = 1
    layercols = which(currentlayer == splitlayer)
    mtemp = numberblocks[1]
    if(length(layercols) > 0) {
      for(j in 1:length(layercols)) {
        if(is.numeric(run_matrix_processed[,layercols[j]])) {
          mtemp = mtemp - 1
        } else {
          cat_degrees = length(unique(run_matrix_processed[,layercols[j] ])) - 1
          mtemp = mtemp - cat_degrees
        }
      }
    }
    m = mtemp
    for(i in 2:(max(splitlayer))) {
      layercols = which(currentlayer == splitlayer)
      mtemp = numberblocks[i] - numberblocks[i-1]
      if(length(layercols) > 0) {
        for(j in 1:length(layercols)) {
          if(is.numeric(run_matrix_processed[,layercols[j]])) {
            mtemp = mtemp - 1
          } else {
            cat_degrees = length(unique(run_matrix_processed[,layercols[j] ])) - 1
            mtemp = mtemp - cat_degrees
          }
        }
      }
      m = c(m,mtemp)
      currentlayer = currentlayer + 1
    }
  }
  degrees_of_freedom = calc_interaction_degrees(run_matrix_processed, model, contrasts, splitlayer, m)
  if(!nointercept) {
    degrees_of_freedom[1] = min(degrees_of_freedom[-1])
  }
  return(degrees_of_freedom)
}
