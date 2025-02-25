register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}


.onLoad <- function(...) {
  register_s3_method("skpr", "print", "skpr_eval_output")
  register_s3_method("skpr", "print", "skpr_power_curve_output")
}

detect_multicore_support = function() {
  ns = getNamespace("parallel")
  supportedByOS = exists(
    "mcparallel",
    mode = "function",
    envir = ns,
    inherits = FALSE
  )
  return(supportedByOS)
}

skpr_system_setup_env = new.env(parent = emptyenv())

assign(
  "has_multicore_support",
  detect_multicore_support(),
  envir = skpr_system_setup_env
)
