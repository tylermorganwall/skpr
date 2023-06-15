#' Run Documentation
#'
#' @return bool
#'
#' @keywords internal
run_documentation = function() {
  return(identical(Sys.getenv("IN_PKGDOWN"), "true"))
}
