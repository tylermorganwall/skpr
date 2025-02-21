#'@title Determine Nesting Level of Blocks
#'
#'@description Calculates if a block is fully nested within another block, and
#'what the highest level of nesting is. 1 indicates the block isn't nested within
#'another block--just within the intercept.
#'
#'@return Vector of numbers indicating the hierarchy
#'@keywords internal
calculate_block_nesting = function(blockgroups, blockstructure) {
  #`nested_level` is a label for which is the highest blocking level (excluding the lowest level)
  # the column is nested within
  nested_level = rep(1, length(blockgroups))
  for (i in seq_len(length(blockgroups))) {
    isblockcol = rep(TRUE, ncol(blockstructure))
    temp_block_str = blockgroups[[i]]
    temp_block_str_names = names(temp_block_str)
    for (j in seq_len(length(temp_block_str))) {
      block_name = temp_block_str_names[j]
      if (temp_block_str[j] - 1 > 0) {
        block_idx = which(blockstructure[, i] == block_name)
        is_constant = lapply(
          apply(blockstructure[block_idx, ], 2, unique),
          length
        ) ==
          1
        isblockcol = isblockcol & is_constant
      }
    }
    isblockcol[i] = FALSE
    block_nested_within = which(isblockcol)
    if (length(block_nested_within) != 0) {
      nested_level[i] = max(block_nested_within) + 1
    }
  }
  #Now this also returns the level each layer is nested in (zero indicating intercept)
  return(nested_level)
}
