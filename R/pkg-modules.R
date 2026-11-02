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
#' Beside its startup module, as `startup.schema.json` beside `startup.js`, a
#' package ships the JSON Schemas of the keys of the front-end's state and of
#' the board options that the module registers. R writes their values by them
#' for the board that an app script builds (see [blockr_ser()]). The file holds
#' a JSON object. Under `keys`, it holds an array of the keys, each with its
#' `path` in the front-end's state, such as `["views"]`, its `kind`,
#' `"collection"` or `"value"`, and its `schema`, an entry's or the value's.
#' Under `options`, it holds the schema of each board option's value, by the
#' board option's ID. Core ships the schemas of its own board options beside
#' its runtime in the same way.
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

# The JSON Schemas that core ships beside its runtime and each package that R
# has loaded beside its startup module, by which R writes the keys of the
# front-end's state and the values of board options: in `keys`, each key with
# its path, its kind and its schema, and in `options`, each board option's
# schema by its ID.
startup_schemas <- function() {

  modules <- c(
    list(list(package = "blockr.core", module = core_runtime())),
    startup_modules()
  )

  keys <- list()
  options <- list()
  key_pkgs <- character()
  option_pkgs <- character()

  for (entry in modules) {

    pkg <- entry[["package"]]
    file <- module_file(entry[["module"]], ".schema.json")

    if (!nzchar(file)) {
      next
    }

    schemas <- read_startup_schemas(file, pkg)

    for (key in schemas[["keys"]]) {

      path <- paste(key[["path"]], collapse = "/")
      other <- key_pkgs[path]

      if (!is.na(other)) {
        blockr_abort(
          "Packages {other} and {pkg} both ship a schema for the key {path} ",
          "of the front-end's state.",
          class = "startup_schema_clash"
        )
      }

      key_pkgs[path] <- pkg
      keys <- c(keys, list(key))
    }

    for (id in names(schemas[["options"]])) {

      other <- option_pkgs[id]

      if (!is.na(other)) {
        blockr_abort(
          "Packages {other} and {pkg} both ship a schema for board option ",
          "{id}.",
          class = "startup_schema_clash"
        )
      }

      option_pkgs[id] <- pkg
      options[[id]] <- schemas[["options"]][[id]]
    }
  }

  list(keys = keys, options = options)
}

core_runtime <- function() {
  js_module("js/blockr-core.js", package = "blockr.core")
}

read_startup_schemas <- function(file, pkg) {

  res <- decode_schema(file)

  invalid <- function(what) {
    blockr_abort(
      "The schemas that package {pkg} ships in {file} are expected to hold ",
      "{what}.",
      class = "startup_schema_invalid"
    )
  }

  if (!is.list(res) || (length(res) && is.null(names(res)))) {
    invalid("an object")
  }

  keys <- coal(res[["keys"]], list(), fail_all = FALSE)

  if (!is.list(keys) || not_null(names(keys))) {
    invalid("an array of keys in `keys`")
  }

  for (i in seq_along(keys)) {

    key <- keys[[i]]

    if (!is.list(key) || !is_names_array(key[["path"]]) ||
          !is_string(key[["kind"]]) ||
          !key[["kind"]] %in% c("collection", "value") ||
          !is.list(key[["schema"]])) {
      invalid(
        paste(
          "for each key a `path` of names, a `kind`, \"collection\" or",
          "\"value\", and a `schema`"
        )
      )
    }

    keys[[i]][["path"]] <- as.character(unclass(key[["path"]]))
  }

  options <- coal(res[["options"]], list(), fail_all = FALSE)

  if (!is.list(options) || (length(options) && is.null(names(options))) ||
        !all(lgl_ply(options, is.list))) {
    invalid("a schema for each board option by its ID in `options`")
  }

  list(keys = keys, options = options)
}

# Whether a value that `decode_schema()` read is an array of names in JSON,
# which it marks with `I()` where the array holds one.
is_names_array <- function(x) {
  is.character(x) && length(x) && all(nzchar(x)) && !anyNA(x) &&
    (inherits(x, "AsIs") || length(x) > 1L)
}
