#'@title Calculate Split Plot Columns
#'
#'@description Calculates which columns correspond to which layers of randomization restriction
#'@param run_matrix_processed The design
#'@return Vector of numbers indicating split plot layers
#'@keywords internal
calculate_split_columns = function(run_matrix_processed, blockgroups) {
  lowest_level = length(lapply(blockgroups,length)) + 1
  splitlayer = rep(lowest_level,ncol(run_matrix_processed))
  for(i in 1:length(blockgroups)) {
    currentrow = 1
    isblockcol = rep(TRUE,ncol(run_matrix_processed))
    temp_block_str = blockgroups[[i]]
    for(j in 1:length(temp_block_str)) {
      if(temp_block_str[j] - 1 > 0) {
        for(k in 0:(temp_block_str[j] - 2)) {
          isblockcol = isblockcol & run_matrix_processed[currentrow + k,] == run_matrix_processed[currentrow + 1 + k,]
        }
      }
      currentrow = currentrow + temp_block_str[j]
    }
    splitlayer[isblockcol & splitlayer == lowest_level] = i
  }
  return(splitlayer)
}
