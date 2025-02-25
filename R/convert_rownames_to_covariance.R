#'@title Converts dot operator to terms
#'
#'@description Converts the row names to a variance-covariance matrix (using the user-supplied variance ratio)
#'@param run_matrix_processed The design
#'@return Variance-covariance matrix V
#'@keywords internal
convert_rownames_to_covariance = function(
  run_matrix_processed,
  varianceratios,
  user_specified_varianceratio
) {
  blocklist = strsplit(rownames(run_matrix_processed), ".", fixed = TRUE)
  blockstructure = do.call(rbind, blocklist)
  blockgroups = get_block_groups(blockstructure)
  if (length(blockgroups) == 1 | is.matrix(blockgroups)) {
    stop(
      "skpr: No blocking detected. Specify block structure in row names or set blocking = FALSE"
    )
  }
  if (
    length(blockgroups) - 1 != length(varianceratios) &&
      length(varianceratios) == 1
  ) {
    if (user_specified_varianceratio) {
      warning(
        "Single varianceratio entered for multiple layers. Setting all but the run-to-run varianceratio to that level."
      )
    }
    varianceratios = c(rep(varianceratios, length(blockgroups) - 1), 1)
  }
  if (length(blockgroups) - 1 == length(varianceratios)) {
    varianceratios = c(varianceratios, 1)
  }
  if (length(blockgroups) != length(varianceratios)) {
    stop(
      "skpr: Wrong number of variance ratios specified. ",
      length(varianceratios),
      " variance ratios given c(",
      paste(varianceratios, collapse = ", "),
      "), ",
      length(blockgroups),
      " expected. Either specify value for all blocking levels or one ratio for all blocks other than then run-to-run variance."
    )
  }
  V = calculate_v_from_blocks(
    nrow(run_matrix_processed),
    blockgroups,
    blockstructure,
    varianceratios
  )
  attr(V, "tempvar") = varianceratios
  return(V)
}
