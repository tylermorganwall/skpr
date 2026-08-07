# Load application support files into testing environment
if (requireNamespace("shinytest2", quietly = TRUE)) {
  shinytest2::load_app_env()
}
