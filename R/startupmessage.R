.onAttach <- function(...) {
  if (!interactive()) {return()}

  startupmessage <- c(
    "Developed at the Institute for Defense Analyses (IDA). Questions? Contact: tylermw@gmail.com",
    "See http://github.com/tylermorganwall/skpr for updates and more information."
  )

  packageStartupMessage(paste(strwrap(startupmessage), collapse = "\n"))
}
