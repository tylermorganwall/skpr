#' Set up progressr handler
#'
#' @keywords internal
set_up_progressr_handler = function(msg_string, type_string) {
  oplan = future::plan()
  original_future_call = deparse(attr(oplan,"call", exact = TRUE), width.cutoff = 500L)
  if(original_future_call != "NULL") {
    if(skpr_system_setup_env$has_multicore_support) {
      message_string = 'plan("multicore", workers = number_of_cores-1)'
    } else {
      message_string = 'plan("multisession", workers = number_of_cores-1)'
    }
    message(sprintf("Using user-defined {future} plan() `%s` instead of {skpr}'s default multicore plan (for this computer) of `%s`", original_future_call, message_string))
  } else {
    numbercores =  getOption("cores", default = getOption("Ncpus", default = max(c(1, future::availableCores()-1))))
    if(skpr_system_setup_env$has_multicore_support) {
      future::plan("multicore", workers = numbercores, .call = NULL)
    } else {
      future::plan("multisession", workers = numbercores, .call = NULL)
    }
  }
  progressr::handlers(global = TRUE)
  progressr::handlers(list(
    progressr::handler_progress(
      format   = sprintf("  %s (%d workers) [:bar] (:current/:total, :tick_rate %s/s) ETA::eta", msg_string, future::nbrOfWorkers(), type_string),
      width    = 100,
      complete = "=",
      interval = 0.2
    )
  ))
}
