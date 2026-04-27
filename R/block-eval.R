#' @param expr Quoted expression to evaluate in the context of `data`
#' @param env Environment in which to evaluate `expr`
#' @rdname block_server
#' @export
block_eval <- function(x, expr, env, ...) {
  UseMethod("block_eval")
}

#' @export
block_eval.block <- function(x, expr, env, ...) {
  eval(expr, env)
}

#' @rdname block_server
#' @export
eval_env <- function(data) {
  if (isTRUE(blockr_option("attach_default_packages", FALSE))) {
    parent <- default_eval_parent()
  } else {
    parent <- baseenv()
  }
  list2env(data, parent = parent)
}

eval_env_default_pkgs <- c(
  "stats", "graphics", "grDevices", "utils", "datasets", "methods"
)

default_eval_parent <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    parent <- baseenv()
    for (pkg in rev(eval_env_default_pkgs)) {
      parent <- pkg_export_env(pkg, parent)
    }
    cached <<- parent
    cached
  }
})

pkg_export_env <- function(pkg, parent) {
  ns <- loadNamespace(pkg)
  e <- new.env(parent = parent)
  exports <- getNamespaceExports(ns)
  importIntoEnv(e, exports, ns, exports)
  lazydata <- getNamespaceInfo(ns, "lazydata")
  for (nm in ls(lazydata, all.names = TRUE)) {
    do.call(delayedAssign,
            list(nm, as.symbol(nm), eval.env = lazydata, assign.env = e))
  }
  lockEnvironment(e, bindings = TRUE)
  e
}

#' @param session Shiny session object
#' @rdname block_server
#' @export
block_eval_trigger <- function(x, session = get_session()) {
  UseMethod("block_eval_trigger", x)
}

#' @export
block_eval_trigger.block <- function(x, session = get_session()) {
  NULL
}
