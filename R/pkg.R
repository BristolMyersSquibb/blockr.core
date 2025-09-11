#' @import vctrs shiny
#' @keywords internal
NULL

pkg_name <- function(env = parent.frame()) {
  utils::packageName(env)
}

pkg_version <- function(pkg = pkg_name()) {
  utils::packageVersion(pkg)
}

pkg_file <- function(...) {
  system.file(..., package = pkg_name())
}

is_pkg_avail <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}
