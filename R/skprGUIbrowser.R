#'@title skprGUIbrowser
#'
#'@description skprGUI provides a graphical user interface to skpr, in an external browser.
#'
#'@import shiny
#'@export
#'@examples
#'#Type `skprGUIbrowser()` to begin
#'
# nocov start

skprGUIbrowser = function() {
  appDir = system.file("shiny", "skprGUI", package = "skpr")
  if (appDir == "") {
    stop("skpr: Could not find example directory. Try re-installing `skpr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
# nocov end
