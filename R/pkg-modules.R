declared_startup_modules <- new.env()

declared_startup_modules$entries <- list()

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
#' whose package is the one that declares it, so that a module made outside a
#' package is declared by none, and the stylesheet and JSON Schema beside a
#' module need no declaration.
#' Declaring a package's startup module, or a module under the same ID, again
#' replaces it, as when a package is loaded again.
#'
#' R keeps the startup modules in the order in which packages declare them,
#' which is the order in which their namespaces load, since R runs the
#' `.onLoad()` of the packages a namespace imports before its own, so that a
#' package's comes after those of the packages its NAMESPACE imports, and the
#' client initializes them one at a time in that order. A package relies on
#' the order only for the packages its NAMESPACE imports, since R loads a
#' package that its DESCRIPTION alone names only once something asks for it,
#' and declaring a startup module again keeps its place.
#'
#' @param module The module, made by [js_module()]
#' @param name The name part of the module's ID
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
register_startup_module <- function(module) {

  if (!is_js_module(module)) {
    blockr_abort(
      "A startup module is expected to be made by `js_module()`.",
      class = "startup_module_invalid"
    )
  }

  package <- check_declaring_package(
    module[["package"]],
    "startup module",
    "startup_module_invalid"
  )

  entry <- list(package = package, module = module)

  declared_startup_modules$entries[[package]] <- entry

  invisible(entry)
}

#' @rdname register_startup_module
#' @export
register_module <- function(name, module) {

  if (!is_js_module(module)) {
    blockr_abort(
      "A module a package offers is expected to be made by `js_module()`.",
      class = "module_invalid"
    )
  }

  id <- declared_id(name, module[["package"]], "module", "module_invalid")

  entry <- list(
    id = id,
    name = name,
    package = module[["package"]],
    module = module
  )

  assign(id, entry, envir = declared_modules)

  invisible(entry)
}

startup_modules <- function() {
  unname(declared_startup_modules$entries)
}

list_modules <- function() {
  ls(envir = declared_modules)
}

get_module <- function(id) {
  get_declared(id, declared_modules, "module", "module_missing")
}
