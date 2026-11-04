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
#' Next to its startup module, as `startup.schema.json` next to `startup.js`, a
#' package puts one JSON Schema, laid out like the board, for the parts of the
#' board that the package defines: the front-end's state under
#' `front_end.state` and the values of its board options under `options`, by
#' their IDs. R shapes the board that an app script builds by it, as it does by
#' core's schema of the board (see [blockr_ser()]), so that a value of length
#' one stays an array where the schema has one. R reads nothing else out of
#' it. Core puts the schema of its own board options next to its runtime in the
#' same way.
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
#' R tells the client where it serves each module that a package declares, its
#' front-ends and renderers included, in its reply to `hello` and to every
#' `open`, with the versions of the packages they come from. It serves a module,
#' and the stylesheet beside it, under the package's name, at the module's path
#' in the package, with a hash of the file's content in the URL, so that no
#' browser runs a changed file from its cache. It serves only the files it
#' names, and nothing else of the package.
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

# The JSON Schemas next to core's runtime and next to the startup module of each
# package that R has loaded, by which R writes the board an app script built.
# Each is one JSON Schema, laid out like the board, for the parts of the board
# that its package defines, such as the values of its board options under
# `options`.
startup_schemas <- function() {

  modules <- c(
    list(list(package = "blockr.core", module = core_runtime())),
    startup_modules()
  )

  res <- list()

  for (entry in modules) {

    file <- module_file(entry[["module"]], ".schema.json")

    if (nzchar(file)) {
      res <- c(res, list(read_startup_schema(file, entry[["package"]])))
    }
  }

  res
}

core_runtime <- function() {
  js_module("js/blockr-core.js", package = "blockr.core")
}

read_startup_schema <- function(file, pkg) {

  res <- decode_schema(file)

  if (!is.list(res) || (length(res) && is.null(names(res)))) {
    blockr_abort(
      "The schema that package {pkg} puts next to its startup module, in ",
      "{file}, is expected to be a JSON object.",
      class = "startup_schema_invalid"
    )
  }

  res
}
