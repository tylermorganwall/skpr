#'@title Remove skpr-generated blocking columns
#'
#'@description Remove skpr-generated REML blocking columns if present
#'@param RunMatrix The run matrix
#'@return Run Matrix
#'@keywords internal
remove_skpr_blockcols = function(RunMatrix) {
  if (!is.null(attr(RunMatrix, "splitanalyzable"))) {
    if (attr(RunMatrix, "splitanalyzable")) {
      allattr = attributes(RunMatrix)
      remove_cols = which(colnames(RunMatrix) %in% allattr$splitcolumns)
      if(length(remove_cols) > 0) {
        RunMatrix = RunMatrix[, -remove_cols, drop = FALSE]
        allattr$names = allattr$names[-remove_cols]
      }
      attributes(RunMatrix) = allattr
    }
  }
  return(RunMatrix)
}
