#'@title Converts dot operator to terms
#'
#'@description Converts the row names to a variance-covariance matrix (using the user-supplied variance ratio)
#'@param run_matrix_processed The design
#'@return Variance-covariance matrix V
#'@keywords internal
convert_rownames_to_covariance = function(run_matrix_processed,varianceratios) {
  blocklist = strsplit(rownames(run_matrix_processed), ".", fixed = TRUE)

  existingBlockStructure = do.call(rbind, blocklist)
  blockgroups = apply(existingBlockStructure, 2, blockingstructure)

  blockMatrixSize = nrow(run_matrix_processed)
  V = diag(blockMatrixSize)
  blockcounter = 1
  if (length(blockgroups) == 1 | is.matrix(blockgroups)) {
    stop("No blocking detected. Specify block structure in row names or set blocking = FALSE")
  }
  if (length(blockgroups) > 2 && length(varianceratios) == 1) {
    varianceratios = rep(varianceratios, length(blockgroups) - 1)
  }
  if (length(blockgroups) > 2 && length(varianceratios) != 1 && length(blockgroups) - 1 != length(varianceratios)) {
    stop("Wrong number of variance ratio specified. Either specify value for all blocking levels or one value for all blocks.")
  }
  blockgroups = blockgroups[-length(blockgroups)]
  for (block in blockgroups) {
    V[1:block[1], 1:block[1]] =  V[1:block[1], 1:block[1]] + varianceratios[blockcounter]
    placeholder = block[1]
    for (i in 2:length(block)) {
      V[(placeholder + 1):(placeholder + block[i]), (placeholder + 1):(placeholder + block[i])] =
        V[(placeholder + 1):(placeholder + block[i]), (placeholder + 1):(placeholder + block[i])] + varianceratios[blockcounter]
      placeholder = placeholder + block[i]
    }
    blockcounter = blockcounter + 1
  }
  return(V)
}
