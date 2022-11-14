#'@title Convert Block Column to Rownames
#'
#'@description Detect externally generated blocking columns and convert to rownames
#'@param RunMatrix The run matrix
#'@param blocking Whether random effects should be included.
#'@param varianceratios A vector of variance ratios for each level of restricted randomization
#'@return Row-name encoded blocked run matrix
#'@keywords internal
convert_blockcolumn_rownames = function(RunMatrix, blocking, varianceratios,
                                        verbose = FALSE) {
  if (is.null(attr(RunMatrix, "splitanalyzable")) &&
      any(grepl("(Block|block)(\\s?)+[0-9]+$", colnames(RunMatrix), perl = TRUE)) ||
      any(grepl("(Whole Plots|Whole\\.Plots|Subplots)", colnames(RunMatrix), perl = TRUE))) {
    blockcols = grepl("(Block|block)(\\s?)+[0-9]+$", colnames(RunMatrix), perl = TRUE) | grepl("(Whole Plots|Whole\\.Plots|Subplots)", colnames(RunMatrix), perl = TRUE)
    if (blocking) {
      if(verbose) {
        message("Detected externally generated blocking columns: attempting to interpret blocking structure.")
      }
      blockmatrix = RunMatrix[, blockcols, drop = FALSE]
      block_max = apply(blockmatrix,2,max)
      blockmatrix = blockmatrix[,order(block_max), drop=FALSE]
      block_order = do.call(order, lapply(blockmatrix,`[`))
      blockmatrix = blockmatrix[block_order, order(unlist(lapply(lapply(blockmatrix, unique), length))), drop = FALSE]
      blockvals = lapply(blockmatrix, unique)
      rownamematrix = matrix(nrow = nrow(RunMatrix), ncol = ncol(blockmatrix) + 1)
      for (col in 1:ncol(blockmatrix)) {
        uniquevals = blockvals[[col]]
        blockcounter = 1
        wholeblockcounter = 1
        for (block in uniquevals) {
          if (col == 1) {
            rownamematrix[blockmatrix[, col] == block, col] = blockcounter
            blockcounter = blockcounter + 1
          }
          if (col != 1) {
            superblock = rownamematrix[blockmatrix[, col] == block, col - 1][1]
            modop = length(unique(blockmatrix[blockmatrix[, col - 1] == superblock, col]))
            if (modop == 1) {
              rownamematrix[blockmatrix[, col] == block, col] = wholeblockcounter
              wholeblockcounter = wholeblockcounter + 1
            } else {
              wholeblockcounter = 1
              if (blockcounter %% modop == 0) {
                rownamematrix[blockmatrix[, col] == block, col] = modop
              } else {
                rownamematrix[blockmatrix[, col] == block, col] = blockcounter %% modop
              }
            }
            blockcounter = blockcounter + 1
          }
          if (col == ncol(blockmatrix)) {
            rownamematrix[blockmatrix[, col] == block, col + 1] = 1:sum(blockmatrix[, col] == block)
          }
        }
        blockcounter = blockcounter + 1
      }
      blockgroups = lapply(blockmatrix, table)
      names(blockgroups) = NULL
      if (length(blockgroups) != length(varianceratios) && length(varianceratios) == 1) {
        # if(user_specified_varianceratio) {
        #   warning("Single varianceratio entered for multiple layers. Setting all but the run-to-run varianceratio to that level.")
        # }
        varianceratios = c(rep(varianceratios,length(blockgroups)-1),1)
      }
      if (length(blockgroups) - 1 == length(varianceratios)) {
        varianceratios = c(varianceratios,1)
      }
      if (length(blockgroups) + 1 == length(varianceratios)) {
        varianceratios = varianceratios[-length(varianceratios)]
      }
      if (length(blockgroups) != length(varianceratios)) {
        stop("Wrong number of variance ratios specified. ", length(varianceratios),
             " variance ratios given: c(",paste(varianceratios,collapse=", "), "), ", length(blockgroups) , " expected. Either specify value for all blocking levels or one ratio for all blocks other than then run-to-run variance.")
      }
      zlist = list()
      for (i in seq_along(1:length(blockgroups))) {
        tempblocks = blockgroups[[i]]
        tempnumberblocks = length(tempblocks)
        ztemp = matrix(0, nrow=nrow(RunMatrix), ncol=tempnumberblocks)
        currentrow = 1
        for (j in 1:tempnumberblocks) {
          ztemp[currentrow:(currentrow + tempblocks[j] - 1), j] = varianceratios[i]
          currentrow = currentrow + tempblocks[j]
        }
        zlist[[i]] = ztemp
      }
      if(is.null(attr(RunMatrix, "z.matrix.list"))) {
        attr(RunMatrix, "z.matrix.list") = zlist
      } else {
        attr(RunMatrix, "z.matrix.list") = list(attr(RunMatrix, "z.matrix.list"), zlist)
      }
      allattr = attributes(RunMatrix)
      allattr$names = allattr$names[!blockcols]
      RunMatrix = RunMatrix[block_order, !blockcols, drop = FALSE]
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
  attr(RunMatrix,"tempvar") = varianceratios
  return(RunMatrix)
}
