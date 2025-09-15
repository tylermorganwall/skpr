#'@title Convert Block Column to Rownames
#'
#'@description Detect externally generated blocking columns and convert to rownames
#'@param design The run matrix
#'@param blocking Whether random effects should be included.
#'@param varianceratios A vector of variance ratios for each level of restricted randomization
#'@return Row-name encoded blocked run matrix
#'@keywords internal
convert_blockcolumn_rownames = function(
  design,
  blocking,
  varianceratios,
  verbose = FALSE
) {
  zlist = list()
  if (
    is.null(attr(design, "splitanalyzable")) &&
      any(grepl(
        "(Block|block)(\\s?)+[0-9]+$",
        colnames(design),
        perl = TRUE
      )) ||
      any(grepl(
        "(Whole Plots|Whole\\.Plots|Subplots)",
        colnames(design),
        perl = TRUE
      ))
  ) {
    blockcols = grepl(
      "(Block|block)(\\s?)+[0-9]+$",
      colnames(design),
      perl = TRUE
    ) |
      grepl(
        "(Whole Plots|Whole\\.Plots|Subplots)",
        colnames(design),
        perl = TRUE
      )
    if (blocking) {
      if (verbose) {
        message(
          "Detected externally generated blocking columns: attempting to interpret blocking structure."
        )
      }
      blockmatrix = design[, blockcols, drop = FALSE]
      block_max = apply(blockmatrix, 2, max)
      blockmatrix = blockmatrix[, order(block_max), drop = FALSE]
      block_order = do.call(order, lapply(blockmatrix, `[`))
      blockmatrix = blockmatrix[
        block_order,
        order(unlist(lapply(lapply(blockmatrix, unique), length))),
        drop = FALSE
      ]
      blockvals = lapply(blockmatrix, unique)
      rownamematrix = cbind(
        do.call("cbind", lapply(blockmatrix, as.character)),
        matrix(
          as.character(seq_len(nrow(blockmatrix))),
          ncol = 1,
          nrow = nrow(blockmatrix)
        )
      )
      blockgroups = lapply(blockmatrix, table)
      names(blockgroups) = NULL
      if (
        length(blockgroups) != length(varianceratios) &&
          length(varianceratios) == 1
      ) {
        varianceratios = c(rep(varianceratios, length(blockgroups) - 1), 1)
      }
      if (length(blockgroups) - 1 == length(varianceratios)) {
        varianceratios = c(varianceratios, 1)
      }
      if (length(blockgroups) + 1 == length(varianceratios)) {
        varianceratios = varianceratios[-length(varianceratios)]
      }
      if (length(blockgroups) != length(varianceratios)) {
        stop(
          "skpr: Wrong number of variance ratios specified. ",
          length(varianceratios),
          " variance ratios given: c(",
          paste(varianceratios, collapse = ", "),
          "), ",
          length(blockgroups),
          " expected. Either specify value for all blocking levels or one ratio for all blocks other than then run-to-run variance."
        )
      }
      for (i in seq_len(length(blockgroups))) {
        tempblocks = blockgroups[[i]]
        block_column = blockmatrix[, i, drop = TRUE]
        tempnumberblocks = length(tempblocks)
        ztemp = matrix(0, nrow = nrow(design), ncol = tempnumberblocks)
        currentrow = 1
        for (j in seq_len(tempnumberblocks)) {
          blockname = names(tempblocks)[j]
          current_block = block_column == blockname
          ztemp[current_block, j] = varianceratios[i]
          currentrow = currentrow + tempblocks[j]
        }
        zlist[[i]] = ztemp
      }
      if (!is.null(attr(design, "z.matrix.list"))) {
        zlist = c(attr(design, "z.matrix.list"), zlist)
        # attr(design, "z.matrix.list") = zlist
      } #else {
      # attr(design, "z.matrix.list") = list(
      #   attr(design, "z.matrix.list"),
      #   zlist
      # )
      # }
      allattr = attributes(design)
      allattr$names = allattr$names[!blockcols]
      design = design[block_order, !blockcols, drop = FALSE]
      attributes(design) = allattr
      rownames(design) = apply(rownamematrix, 1, paste, collapse = ".")
    } else {
      warning(
        "Detected externally generated blocking columns but blocking not turned on: ignoring blocking structure and removing blocking columns."
      )
      allattr = attributes(design)
      allattr$names = allattr$names[!blockcols]
      design = design[, !blockcols, drop = FALSE]
      attributes(design) = allattr
    }
  } else {
    if (
      any(grepl(
        "(Block|block)(\\s?)+[0-9]+$",
        colnames(design),
        perl = TRUE
      )) ||
        any(grepl(
          "(Whole Plots|Whole\\.Plots|Subplots)",
          colnames(design),
          perl = TRUE
        ))
    ) {
      added_cols = colnames(design)[
        grepl("(Block|block)(\\s?)+[0-9]+$", colnames(design), perl = TRUE) |
          grepl(
            "(Whole Plots|Whole\\.Plots|Subplots)",
            colnames(design),
            perl = TRUE
          )
      ]
      extra_cols = paste0(sprintf("`%s`", added_cols), collapse = ", ")
      blockcols = grepl(
        "(Block|block)(\\s?)+[0-9]+$",
        colnames(design),
        perl = TRUE
      ) |
        grepl(
          "(Whole Plots|Whole\\.Plots|Subplots)",
          colnames(design),
          perl = TRUE
        )
      allattr = attributes(design)
      allattr$names = allattr$names[!blockcols]
      design = design[, !blockcols, drop = FALSE]
      attributes(design) = allattr
      warning(sprintf(
        "skpr: Externally added block columns detected: extra block columns (%s) ignored.",
        extra_cols
      ))
    }
  }
  # attr(design, "tempvar") = varianceratios
  return(list(
    design_blockcolumn_converted = design,
    varianceratios = varianceratios,
    z_matrix_list = zlist
  ))
}
