declared_startup_modules <- new.env()

declared_modules <- new.env()

#' Modules a package declares
#'
#' Besides its block classes and its front-ends (see [register_front_end()]),
#' a package declares in R, from its `.onLoad()`, the modules of its client
#' that R serves: its startup module, at most one, with
#' `register_startup_module()`, for what has to run before anything mounts,
#' such as registering the board options it ships, and the modules it offers
#' other packages, each with `register_module()` under a name that gives it
#' the ID `{pkg}-{name}`, as a registry id is, by which `host.module()` and
#' `CoreApi.module()` reach it in the client. Each is made by [js_module()],
#' and the stylesheet and JSON Schema beside a module need no declaration.
#' Declaring a package's startup module, or a module under the same ID, again
#' replaces it, as when a package is loaded again.
#'
#' R orders the startup modules by the `Depends` and `Imports` of each
#' package's DESCRIPTION, so that a package's comes after those of the
#' packages it depends on, and the client initializes them one at a time in
#' that order. A package relies on the order only for the packages it imports
#' in R, and between any other two packages it is unspecified.
#'
#' @param module The module, made by [js_module()]
#' @param name The name part of the module's ID
#' @param package The package that declares the module
#'
#' @examples
#' \dontrun{
#' # In the `.onLoad()` of a package such as blockr.dplyr
#' register_startup_module(js_module("js/startup.js"))
#' register_module("filter", js_module("js/filter.js"))
#' }
#'
#' @return Both `register_startup_module()` and `register_module()` return the
#' module's entry invisibly, a list holding the declaring `package` and the
#' `module`, and for `register_module()` the module's `id` and `name` too.
#'
#' @export
register_startup_module <- function(module,
                                    package = pkg_name(parent.frame())) {

  check_declaring_package(package, "startup module", "startup_module_invalid")

  if (!is_js_module(module)) {
    blockr_abort(
      "The startup module of package {package} is expected to be made by ",
      "`js_module()`.",
      class = "startup_module_invalid"
    )
  }

  entry <- list(package = package, module = module)

  assign(package, entry, envir = declared_startup_modules)

  invisible(entry)
}

#' @rdname register_startup_module
#' @export
register_module <- function(name, module, package = pkg_name(parent.frame())) {

  id <- declared_id(name, package, "module", "module_invalid")

  if (!is_js_module(module)) {
    blockr_abort(
      "Module {id} is expected to be made by `js_module()`.",
      class = "module_invalid"
    )
  }

  entry <- list(id = id, name = name, package = package, module = module)

  assign(id, entry, envir = declared_modules)

  invisible(entry)
}

startup_modules <- function() {

  pkgs <- ls(envir = declared_startup_modules)

  if (!length(pkgs)) {
    return(list())
  }

  deps <- lapply(pkgs, function(pkg) intersect(pkg_dependencies(pkg), pkgs))

  order <- topo_sort(
    as_adjacency_matrix(
      from = unlst(deps),
      to = rep(pkgs, lengths(deps)),
      nodes = pkgs
    )
  )

  lapply(order, get, envir = declared_startup_modules, inherits = FALSE)
}

list_modules <- function() {
  ls(envir = declared_modules)
}

get_module <- function(id) {
  get_declared(id, declared_modules, "module", "module_missing")
}

pkg_dependencies <- function(pkg) {

  desc <- utils::packageDescription(pkg, fields = c("Depends", "Imports"))
  deps <- unlst(strsplit(Filter(Negate(is.na), unlst(desc)), ","))
  deps <- trimws(sub("\\([^)]*\\)", "", deps))

  setdiff(deps[nzchar(deps)], "R")
}
