#'@title Calculate block structure lengths
#'
#'@param existing_block_structure character matrix from rownames, split by `.`
#'
#'@return List of numbers indicating split plot layer sizes
#'@keywords internal
get_block_groups = function(existing_block_structure) {
  existing_block_structure = as.data.frame(existing_block_structure)
  block_sizes = apply(existing_block_structure,2,table)
  for(i in seq_len(length(block_sizes))) {
    block_order = order(as.integer(names(block_sizes[[i]])))
    block_sizes[[i]] = block_sizes[[i]][block_order]
  }
  return(block_sizes)
}
