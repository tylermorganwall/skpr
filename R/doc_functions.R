#'@title Determines if rendering in knitr
#'
#'@return boolean
#'@keywords internal
is_rendering_in_knitr = function() {
  isTRUE(getOption('knitr.in.progress'))
}
