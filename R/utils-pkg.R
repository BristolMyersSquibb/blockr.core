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

# A reference to what a package defines, such as a block class, a plugin
# class, a renderer or a module that a package offers others.
# It holds the package, or `NULL` for what an app script defines, and the id
# within that package.
# R writes it to the client as `{ "package": ..., "id": ... }`, with `null`
# for no package, and reads it back as the same list.
# A package of `NA` counts as none, as a row of `available_blocks()` gives it.
pkg_ref <- function(package, id) {

  if (length(package) == 1L && is.na(package)) {
    package <- NULL
  }

  list(package = package, id = id)
}

# Whether `x` is a reference, a list of exactly a package and an id, in either
# order, as R decodes one that the client sends.
# The package is a non-empty string or `NULL`, and the id a non-empty string.
is_pkg_ref <- function(x) {
  is.list(x) && !is.object(x) && length(x) == 2L &&
    setequal(names(x), c("package", "id")) &&
    (is.null(x[["package"]]) || is_nonempty_string(x[["package"]])) &&
    is_nonempty_string(x[["id"]])
}

# Whether `x` and `y` name one thing: whether they hold the same package and
# the same id, as a reference does, and as an entry that holds `package` and
# `id` does too.
same_pkg_ref <- function(x, y) {
  identical(x[["package"]], y[["package"]]) && identical(x[["id"]], y[["id"]])
}

# How a message names what the reference `x` names, as `head_block of
# blockr.core`, or by its id alone for what an app script defines.
# Anything else, such as a string where a reference belongs, it names as it
# prints.
pkg_ref_label <- function(x) {

  if (!is.list(x) || !is_string(x[["id"]])) {
    return(paste(format(x), collapse = ", "))
  }

  if (is.null(x[["package"]])) {
    return(x[["id"]])
  }

  paste(x[["id"]], "of", x[["package"]])
}

# How a message names what each row of the data frame `x` names, with its
# package in `package` and its id in `id`, as `pkg_ref_label()` names one.
pkg_row_labels <- function(x) {

  ids <- as.character(x[["id"]])
  packages <- as.character(x[["package"]])

  ifelse(is.na(packages), ids, paste(ids, "of", packages))
}

# The entry of the list `x` that the reference `ref` names, where each entry
# holds a `package` and an `id`, or `NULL` where none does.
find_pkg_ref <- function(x, ref) {

  for (entry in x) {
    if (same_pkg_ref(entry, ref)) {
      return(entry)
    }
  }

  NULL
}

# For each row of the data frame `x`, with a `package` and an `id`, the index
# of the first entry of the list `table` that names the same, where each entry
# holds a `package` and an `id`, or `NA` where none does.
match_rows <- function(x, table) {
  int_ply(
    seq_len(nrow(x)),
    function(i) {
      ref <- pkg_ref(x[["package"]][i], x[["id"]][i])
      hit <- which(lgl_ply(table, same_pkg_ref, ref))
      if (length(hit)) hit[1L] else NA_integer_
    }
  )
}

# The entries of `x`, a list by package of lists by id, as one list, in the
# order of their packages and, within each package, in their order there.
flat_by_package <- function(x) {
  unlst(lapply(unname(x), unname))
}

# Whether `x` is named as an R package is: letters, digits and dots, starting
# with a letter and ending with no dot, at least two characters long.
is_package_name <- function(x) {
  is_string(x) && !is.na(x) &&
    grepl("^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$", x)
}
