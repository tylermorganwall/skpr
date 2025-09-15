#'@title Remove skpr-generated blocking columns
#'
#'@description Remove skpr-generated REML blocking columns if present
#'@param design The run matrix
#'@return Data frame
#'@keywords internal
remove_skpr_blockcols = function(design) {
  if (!is.null(attr(design, "splitanalyzable"))) {
    if (attr(design, "splitanalyzable")) {
      allattr = attributes(design)
      remove_cols = which(colnames(design) %in% allattr$splitcolumns)
      if (length(remove_cols) > 0) {
        design = design[, -remove_cols, drop = FALSE]
        allattr$names = allattr$names[-remove_cols]
      }
      attributes(design) = allattr
    }
  }
  return(design)
}
