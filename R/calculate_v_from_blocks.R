#'@title Calculate V from Blocks
#'
#'@description Calculates the V matrix from the block structure
#'@param nrow_design The number of runs in the design
#'@param blockgroups List indicating the size of each block for each layer
#'@param blockstructure Matrix indicating the block structure from the rownames
#'@return Variance-Covariance Matrix
#'@keywords internal
calculate_v_from_blocks = function(
  nrow_design,
  blockgroups,
  blockstructure,
  varianceratios
) {
  V = diag(nrow_design)
  get_2_permutations = function(input_vec) {
    perms = t(combn(input_vec, 2))
    identity_vals = matrix(rep(input_vec, 2), ncol = 2)
    return(rbind(rbind(perms, t(apply(perms, 1, rev))), identity_vals))
  }
  blockgroups2 = blockgroups[-length(blockgroups)]

  for (i in seq_len(length(blockgroups2))) {
    block = blockgroups2[[i]]
    block_rows = blockstructure[, i]
    for (j in seq_len(length(block))) {
      block_name = names(block)[j]
      in_block = which(block_name == block_rows)
      coords_to_add = get_2_permutations(in_block)
      for (k in seq_len(nrow(coords_to_add))) {
        V[coords_to_add[k, 1], coords_to_add[k, 2]] = V[
          coords_to_add[k, 1],
          coords_to_add[k, 2]
        ] +
          varianceratios[i]
      }
    }
  }
  return(V)
}
