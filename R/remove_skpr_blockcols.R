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
      RunMatrix = RunMatrix[, -1:-length(allattr$splitcolumns)]
      allattr$names = allattr$names[-1:-length(allattr$splitcolumns)]
      attributes(RunMatrix) = allattr
    }
  }
  return(RunMatrix)
}
