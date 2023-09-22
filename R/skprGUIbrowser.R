#'@title skprGUIbrowser
#'
#'@description skprGUI provides a graphical user interface to skpr, in an external browser.
#'
#'@export
#'@examples
#'#Type `skprGUIbrowser()` to begin
#'
# nocov start
skprGUIbrowser = function() {
  check_for_suggest_packages(c("shiny","shinythemes","shinyjs","gt","rintrojs"))

  appDir = system.file("shiny", "skprGUI", package = "skpr")
  if (appDir == "") {
    stop("skpr: Could not find example directory. Try re-installing `skpr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
# nocov end
