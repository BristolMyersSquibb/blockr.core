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
#' places for the class's plugin, is such a module, which the class names by
#' its ID in JavaScript (see [new_plugin()]).
#' Core declares its basic frontend the same way, as `blockr.core-basic`,
#' which draws the board.
#'
#' Next to its startup module, a package puts a JSON Schema for each plugin
#' class and each board option that it registers, in a file of its own, as a
#' block class's UI module ships the schema of the block's state next to it.
#' The schema of the state of the plugins of the class `{pkg}-{id}` goes in
#' `{id}.schema.json`, named by the id of the class, and that of the value of
#' the board option `{id}` in `{id}_option.schema.json`, named by the class in
#' R of the board option, on which [board_option_version()] dispatches.
#' So blockr.dag might put the schema of the class `blockr.dag-dag_plugin` in
#' `dag_plugin.schema.json`, and that of a board option `grid_gap` in
#' `grid_gap_option.schema.json`.
#' R finds the schema of a plugin's state next to the startup module of the
#' package that its class's ID names, and the schema of a board option's value
#' next to that of any package.
#' R shapes the board that an app script builds by them, as it does by core's
#' schema of the board (see [blockr_ser()]), so that a value of length one stays
#' an array where a schema has one.
#' R reads nothing else out of them.
#' Core puts the schemas of its own board options next to its runtime in the
#' same way, as `n_rows_option.schema.json`.
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
#' R tells the client where it serves each module that a package the app
#' uses declares (see [serve()]), those of plugin classes included, in its
#' reply to `hello`, with the versions of the packages they come from, and
#' where it serves a renderer's module in each output that the renderer draws
#' (see [new_renderer()]).
#' It serves a module,
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

# Core's runtime and the startup module of each package that R has loaded, in
# R's order, next to which core puts the JSON Schemas of its board options and
# a package those of its plugin classes and board options, by which R writes
# the board an app script built.
schema_modules <- function() {
  c(
    list(list(package = "blockr.core", module = core_runtime())),
    startup_modules()
  )
}

core_runtime <- function() {
  js_module("js/blockr-core.js", package = "blockr.core")
}

# The JSON Schema of the state of the plugins of the class with the ID `id`,
# `{pkg}-{id}`, in `{id}.schema.json` next to the startup module of the package
# `{pkg}`, or `NULL` where there is none.
plugin_class_schema <- function(id) {

  for (entry in schema_modules()) {
    if (identical(entry[["package"]], id_package(id))) {
      return(schema_beside(entry[["module"]], id_class(id)))
    }
  }

  NULL
}

# The JSON Schema of the value of the board option with the ID `id`, in
# `{id}_option.schema.json` next to core's runtime or a startup module, the
# first that R finds in R's order, or `NULL` where there is none.
# R looks next to each, since a board option's ID names no package.
board_option_schema <- function(id) {

  for (entry in schema_modules()) {

    schema <- schema_beside(entry[["module"]], paste0(id, "_option"))

    if (not_null(schema)) {
      return(schema)
    }
  }

  NULL
}

# The JSON Schema in `{class}.schema.json` next to a module, decoded, or `NULL`
# where there is none.
schema_beside <- function(module, class) {

  dir <- dirname(module_path(module))
  name <- paste0(class, ".schema.json")

  file <- package_file(
    if (identical(dir, ".")) name else file.path(dir, name),
    module[["package"]]
  )

  if (nzchar(file)) decode_schema(file)
}
