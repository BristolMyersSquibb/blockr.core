#' Block evaluation
#'
#' The generic `block_eval()` evaluates a block expression, interpolated with
#' the block's user inputs, in the context of its data inputs, which an
#' environment created by `eval_env()` holds. A block class can implement a
#' method to evaluate its expression otherwise, as the `plot_block` method does
#' to record the plot the expression draws.
#'
#' @param x Block object
#' @param expr Quoted expression to evaluate in the context of `data`
#' @param env Environment in which to evaluate `expr`
#' @param ... Generic consistency
#'
#' @return The block result for `block_eval()` and an environment for
#' `eval_env()`.
#'
#' @export
block_eval <- function(x, expr, env, ...) {
  UseMethod("block_eval")
}

#' @export
block_eval.block <- function(x, expr, env, ...) {
  eval(expr, env)
}

#' @param data Input data (a named list)
#' @rdname block_eval
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
