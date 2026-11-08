declared_startup_modules <- new.env()

declared_startup_modules$entries <- list()

declared_modules <- new.env()

#' Modules a package declares
#'
#' Besides its block classes (see [register_block()]), a package declares in
#' R, from its `.onLoad()`, the modules of its client that R serves: its
#' startup module, at most one, with
#' `register_startup_module()`, for what has to run before anything mounts,
#' such as registering its plugin classes and the board options it ships, and
#' the modules it offers other packages, each with
#' `register_module()` under a name that gives it the ID `{pkg}-{name}`, as a
#' registry id is, by which `host.module()` and `CoreApi.module()` reach it in
#' the client.
#' Each is made by [js_module()], whose package is the one that
#' declares it, so that a module made outside a package is declared by none,
#' and the stylesheet and JSON Schema beside a module need no declaration.
#' Declaring a package's startup module, or a module under the same ID, again
#' replaces it, as when a package is loaded again.
#'
#' The module that a plugin class names, such as the UI that every frontend
#' places for each of its plugins, is such a module, which the class names by
#' its ID in JavaScript (see [new_plugin()]).
#' Core declares its basic frontend the same way, as `blockr.core-basic`,
#' which draws the board.
#'
#' Next to its startup module, as `startup.schema.json` next to `startup.js`, a
#' package puts the JSON Schemas of what it defines of a board: the state of
#' the plugins of each plugin class that it registers, under `plugins` by the
#' class's ID, and the value of each board option that it registers, under
#' `options` by the board option's ID, as blockr.dag might:
#'
#' ```
#' {
#'   "plugins": {
#'     "blockr.dag-dag_plugin": {
#'       "type": "object",
#'       "properties": { "positions": { "type": "object" } }
#'     }
#'   },
#'   "options": { "grid_gap": { "type": "integer", "minimum": 0 } }
#' }
#' ```
#'
#' So R finds the schema of a plugin's state by the plugin's class, whatever ID
#' the plugin has on a board.
#' R shapes the board that an app script builds by them, as it does by
#' core's schema of the board (see [blockr_ser()]), so that a value of length
#' one stays an array where a schema has one. R reads nothing else out of
#' them. Core puts the schemas of its own board options next to its runtime in
#' the same way.
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
#' R tells the client where it serves each module that a package declares, those
#' of plugin classes included, in its reply to `hello`, with the versions of the
#' packages they come from, and where it serves a renderer's module in each
#' output that the renderer draws (see [new_renderer()]). It serves a module,
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
#'
#' # In the `.onLoad()` of blockr.dock, the module of its frontend
#' register_module("dock", js_module("js/dock.js"))
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

# The ID `{pkg}-{name}` of what a package declares, as a registry id is,
# such as a module or a renderer, whose name has to be a non-empty string.
declared_id <- function(name, package, what, class) {

  check_declaring_package(package, what, class)

  if (!is_nonempty_string(name)) {
    blockr_abort(
      "The name of a {what} of package {package} is expected to be a ",
      "non-empty string.",
      class = class
    )
  }

  paste0(package, "-", name)
}

get_declared <- function(id, env, what, class) {

  res <- get0(id, envir = env, inherits = FALSE)

  if (is.null(res)) {
    blockr_abort("No {what} is declared as {id}.", class = class)
  }

  res
}

check_declaring_package <- function(package, what, class) {

  if (!is_nonempty_string(package)) {
    blockr_abort(
      "A {what} is declared by a package, from its `.onLoad()`.",
      class = class
    )
  }

  invisible(package)
}

# The JSON Schemas next to core's runtime and next to the startup module of each
# package that R has loaded, by which R writes the board an app script built:
# for each package, the schemas of the state of its plugin classes under
# `plugins`, by the class's ID, and of the values of its board options under
# `options`, by the board option's ID.
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

  is_object <- function(x) {
    is.list(x) && (!length(x) || not_null(names(x)))
  }

  parts <- c("plugins", "options")

  if (!is_object(res) || !all(names(res) %in% parts) ||
        !all(lgl_ply(res, is_object)) ||
        !all(lgl_ply(unlst(unname(res)), is_object))) {
    blockr_abort(
      "The schemas that package {pkg} puts next to its startup module, in ",
      "{file}, are expected to be a JSON object that holds JSON Schemas ",
      "under {parts}, the state of each plugin class by the class's ID and ",
      "the value of each board option by its ID.",
      class = "startup_schema_invalid"
    )
  }

  res
}
