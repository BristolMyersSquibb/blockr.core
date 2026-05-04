#' @import vctrs shiny
#' @keywords internal
#' @importFrom cli qty
#' @importFrom grDevices col2rgb
#' @importFrom graphics plot
#' @importFrom methods is
NULL

# `datasets` has no function exports (only lazy data), so @importFrom
# can't anchor it; reference `iris` here to silence the unused-Imports NOTE.
ignore_unused_imports <- function() datasets::iris

#' @param env An environment that is resolved to a package name
#' @rdname set_names
#' @export
pkg_name <- function(env = parent.frame()) {
  utils::packageName(env)
}

#' @param pkg A string.valued package name or an environment passed to
#' [pkg_name()]
#' @rdname set_names
#' @export
pkg_version <- function(pkg = parent.frame()) {

  if (is.environment(pkg)) {
    pkg <- pkg_name(pkg)
  }

  utils::packageVersion(pkg)
}

#' @rdname set_names
#' @export
pkg_file <- function(..., pkg = parent.frame()) {
  system.file(..., package = pkg_name(pkg))
}

#' @rdname set_names
#' @export
pkg_avail <- function(...) {
  pkgs <- c(...)
  log_trace("checking availability of package{?s} {pkgs}")
  all(lgl_ply(pkgs, requireNamespace, quietly = TRUE))
}

#' Build an OTel-friendly observer label
#'
#' Returns `name` prefixed with the calling package's name in angle brackets,
#' so that observer labels emitted into OpenTelemetry traces are unambiguous
#' across `blockr.core`, `blockr.dock`, and downstream extension packages.
#' For example, calling `otel_lbl("add_block")` from inside `blockr.dock`
#' returns `"<blockr.dock>add_block"`.
#'
#' Intended for use as the `label` argument of [shiny::observeEvent()],
#' [shiny::observe()], [shiny::reactive()], and [shiny::reactiveVal()].
#' The package name is resolved at call time via [pkg_name()] applied to
#' `env`, which defaults to the immediate caller's environment.
#'
#' @param name Short, snake_case identifier describing the observer.
#' @param env An environment used to resolve the package name; defaults to
#'   the caller's frame.
#'
#' @return A string of the form `"<pkg>name"`.
#'
#' @examples
#' # Inside a package, equivalent to label = "<your.pkg>my_obs"
#' otel_lbl("my_obs")
#'
#' @rdname otel_lbl
#' @export
otel_lbl <- function(name, env = parent.frame()) {

  if (!is_string(name) || is.na(name) || !nzchar(name)) {
    blockr_abort(
      "`name` must be a non-empty string.",
      class = "otel_lbl_name_invalid"
    )
  }

  paste0("<", pkg_name(env), ">", name)
}
