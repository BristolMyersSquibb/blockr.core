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
  package_system_file(..., package = pkg_name(pkg))
}

# The package of each method of `generic` that R finds from core's namespace,
# by the method's class: the package whose namespace defines the method's
# function, or `NA` for a method that an app script defines, from no package.
method_packages <- function(generic) {

  ns <- asNamespace("blockr.core")
  methods <- as.character(utils::.S3methods(generic, envir = ns))
  classes <- sub(paste0("^", generic, "[.]"), "", methods)

  set_names(
    chr_ply(
      classes,
      function(class) {
        fun <- utils::getS3method(generic, class, optional = TRUE, envir = ns)
        pkg <- if (is.function(fun)) utils::packageName(environment(fun))
        coal(pkg, NA_character_, fail_all = FALSE)
      }
    ),
    classes
  )
}

# The classes of the methods of `generic` that R finds from core's namespace,
# those that packages register and those that an app script defines.
# Where `packages` is given, only those whose method a package among them
# defines, or an app script (see `method_packages()`).
offered_method_classes <- function(generic, packages = NULL) {

  pkgs <- method_packages(generic)

  if (is.null(packages)) {
    return(names(pkgs))
  }

  names(pkgs)[is.na(pkgs) | pkgs %in% packages]
}

# What `system.file()` gives for the files of a package, through the package's
# own `system.file()` where its namespace is loaded.
# For a package that `pkgload::load_all()` loaded, that is pkgload's, which
# looks under the package's `inst/`, while base R's looks in its source
# directory, where no installed file is.
package_system_file <- function(..., package) {

  fun <- if (is_string(package) && isNamespaceLoaded(package)) {
    get("system.file", envir = asNamespace(package), mode = "function")
  } else {
    system.file
  }

  fun(..., package = package)
}

#' @rdname set_names
#' @export
pkg_avail <- function(...) {
  pkgs <- c(...)
  log_trace("checking availability of package{?s} {pkgs}")
  all(lgl_ply(pkgs, requireNamespace, quietly = TRUE))
}
