#' @import vctrs
#' @keywords internal
#' @importFrom cli qty
#' @importFrom grDevices col2rgb
#' @importFrom graphics plot
#' @importFrom methods is
NULL

# `datasets` has no function exports (only lazy data), so @importFrom
# can't anchor it, and `glue` is called only by the code the glue block
# builds; reference both here to silence the unused-Imports NOTE.
ignore_unused_imports <- function() list(datasets::iris, glue::glue)

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

  if (is.environment(pkg)) {
    pkg <- pkg_name(pkg)
  }

  # Through the package's own `system.file()` where its namespace is loaded,
  # which for a package that `pkgload::load_all()` loaded is pkgload's, which
  # looks under the package's `inst/`, while base R's looks in its source
  # directory, where no installed file is
  fun <- if (is_string(pkg) && isNamespaceLoaded(pkg)) {
    get("system.file", envir = asNamespace(pkg), mode = "function")
  } else {
    system.file
  }

  fun(..., package = pkg)
}

#' @rdname set_names
#' @export
pkg_avail <- function(...) {
  pkgs <- c(...)
  log_trace("checking availability of package{?s} {pkgs}")
  all(lgl_ply(pkgs, requireNamespace, quietly = TRUE))
}
