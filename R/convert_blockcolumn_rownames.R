#'@title Convert Block Column to Rownames
#'
#'@description Detect externally generated blocking columns and convert to rownames
#'@param RunMatrix The run matrix
#'@param blocking Whether random effects should be included.
#'@return Row-name encoded blocked run matrix
#'@keywords internal
convert_blockcolumn_rownames = function(RunMatrix, blocking) {
  if (is.null(attr(RunMatrix, "splitanalyzable")) &&
      any(grepl("(Block|block)(\\s?)+[0-9]+$", colnames(RunMatrix), perl = TRUE)) ||
      any(grepl("(Whole Plots|Subplots)", colnames(RunMatrix), perl = TRUE))) {
    blockcols = grepl("(Block|block)(\\s?)+[0-9]+$", colnames(RunMatrix), perl = TRUE) | grepl("(Whole Plots|Subplots)", colnames(RunMatrix), perl = TRUE)
    if (blocking) {
      warning("Detected externally generated blocking columns: attempting to interpret blocking structure.")
      blockmatrix = RunMatrix[, blockcols, drop = FALSE]
      blockmatrix = blockmatrix[, order(unlist(lapply(lapply(blockmatrix, unique), length))), drop = FALSE]
      blockvals = lapply(blockmatrix, unique)
      rownamematrix = matrix(nrow = nrow(RunMatrix), ncol = ncol(blockmatrix) + 1)
      for (col in 1:ncol(blockmatrix)) {
        uniquevals = blockvals[[col]]
        blockcounter = 1
        for (block in uniquevals) {
          if (col == 1) {
            rownamematrix[blockmatrix[, col] == block, col] = blockcounter
            blockcounter = blockcounter + 1
          }
          if (col != 1) {
            superblock = rownamematrix[blockmatrix[, col] == block, col - 1][1]
            modop = length(unique(blockmatrix[blockmatrix[, col - 1] == superblock, col]))
            if (blockcounter %% modop == 0) {
              rownamematrix[blockmatrix[, col] == block, col] = modop
            } else {
              rownamematrix[blockmatrix[, col] == block, col] = blockcounter %% modop
            }
            blockcounter = blockcounter + 1
          }
          if (col == ncol(blockmatrix)) {
            rownamematrix[blockmatrix[, col] == block, col + 1] = 1:sum(blockmatrix[, col] == block)
          }
        }
        blockcounter = blockcounter + 1
      }
      allattr = attributes(RunMatrix)
      allattr$names = allattr$names[!blockcols]
      RunMatrix = RunMatrix[, !blockcols, drop = FALSE]
      attributes(RunMatrix) = allattr
      rownames(RunMatrix) = apply(rownamematrix, 1, paste, collapse = ".")
    } else {
      warning("Detected externally generated blocking columns but blocking not turned on: ignoring blocking structure and removing blocking columns.")
      allattr = attributes(RunMatrix)
      allattr$names = allattr$names[!blockcols]
      RunMatrix = RunMatrix[, !blockcols, drop = FALSE]
      attributes(RunMatrix) = allattr
    }
  }
  return(RunMatrix)
}
