#'@title check_for_suggest_packages
#'
#'@description checks for suggests
#'
#'@keywords internal
#'@return none
check_for_suggest_packages = function(packages) {
  stopifnot(inherits(packages,"character") && length(packages) > 0)
  packages_not_found = list()
  cntr = 1
  for(i in seq_len(length(packages))) {
    if(length(find.package(packages[i],quiet=TRUE)) == 0) {
      packages_not_found[[cntr]] = packages[i]
      cntr = cntr + 1
    }
  }
  if(cntr > 1) {
    packages_not_found = paste(sprintf(r"("%s")",unlist(packages_not_found)), collapse = ", ")
    stop(sprintf(r"{The following packages are required for this functionality but were not found: c(%s)}", packages_not_found))
  }
}
