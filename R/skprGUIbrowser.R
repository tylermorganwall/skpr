#'@title skprGUI
#'
#'@description skprGUI
#'
#'@return skprGUI
#'@import shiny
#'@export
skprGUIbrowser = function() {
  appDir = system.file("shiny", "skprGUI", package = "skpr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
