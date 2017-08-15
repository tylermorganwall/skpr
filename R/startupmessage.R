.onAttach <- function(...) {
  if (!interactive()) {return()}

  startupmessage <- c(
    "Developed at the Institute for Defense Analyses (IDA). Type skprGUI() and click tutorial for an introduction.",
    "Questions? Contact: tylermw@gmail.com, or visit http://github.com/tylermorganwall/skpr"
  )

  packageStartupMessage(paste(strwrap(startupmessage), collapse = "\n"))
}
