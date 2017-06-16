#'@title skprGUI
#'
#'@description skprGUI
#'
#'@return skprGUI
#'@export
skprGUI = function() {
  appDir = system.file("shiny-examples", "skprGUI", package = "skpr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
