#'@title Calculate Split Plot Columns
#'
#'@description Calculates which columns correspond to which layers of randomization restriction
#'@param run_matrix_processed The design
#'@return Vector of numbers indicating split plot layers
#'@keywords internal
calculate_split_columns = function(
  run_matrix_processed,
  blockgroups,
  blockstructure
) {
  lowest_level = length(blockgroups) + 1
  splitlayer = rep(lowest_level, ncol(run_matrix_processed))
  allsplit = TRUE
  for (i in seq_len(length(blockgroups))) {
    isblockcol = rep(TRUE, ncol(run_matrix_processed))
    temp_block_str = blockgroups[[i]]
    temp_block_str_names = names(temp_block_str)
    for (j in seq_len(length(temp_block_str))) {
      block_name = temp_block_str_names[j]
      if (temp_block_str[j] - 1 > 0) {
        block_idx = which(blockstructure[, i] == block_name)
        is_constant = lapply(
          apply(
            run_matrix_processed[block_idx, , drop = FALSE],
            2,
            unique,
            simplify = FALSE
          ),
          length
        ) ==
          1
        isblockcol = isblockcol & is_constant
      }
    }
    if (length(splitlayer[isblockcol & splitlayer == lowest_level]) < 1) {
      allsplit = FALSE
    }
    splitlayer[isblockcol & splitlayer == lowest_level] = i
  }
  #Now this also returns the level each layer is nested in (zero indicating intercept)
  return(list(splitlayer = splitlayer, allsplit = allsplit))
}
