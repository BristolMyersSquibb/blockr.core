#' What a package lists for core
#'
#' A package lists what it offers core in one object that it exports,
#' `._BLOCKR_MODULES_`: its block classes, its plugin classes, its board
#' options and the modules that it offers other packages' UIs.
#' R finds the packages that export it, the available packages, without
#' loading any: the installed packages whose `Meta/nsInfo.rds` lists the object
#' among their exports, and the loaded packages that export it, among them
#' those that `pkgload::load_all()` loaded, which sit in no library.
#' It checks no package's dependencies, since only a blockr package exports the
#' object.
#' R reads an object only from a loaded namespace, so it loads each available
#' package as it reads the package's object, as [available_blocks()] and
#' [available_plugins()] do as an app starts.
#'
#' The object is a list, or an environment that the package's `.onLoad()`
#' fills, of up to four parts, each a list:
#'
#' * Its block classes, in `blocks`: each block constructor by name, with the
#'   registry metadata of the class that it makes, as the block registration
#'   roclet writes them into the package's `inst/registry/blocks.yml`, from
#'   which `read_blocks_yml()` reads them (see [block_registration_roclet()]).
#' * Its plugin classes, in `plugins`: each plugin constructor by name, with
#'   the `id` of the class of the plugins that it makes, the class's `name`,
#'   the human-readable form of its id, and the class's `module` (see
#'   [new_plugin()]).
#' * Its board options, in `options`: each board option by its ID, with its
#'   `module` (see [new_board_option()]).
#' * The modules that it offers other packages' UIs, in `modules`: each by a
#'   name that gives it the ID `{pkg}-{name}`, as a registry id is, by which
#'   `host.module()` and `CoreApi.module()` reach it in the client.
#'
#' A module is given by its path in the package's `inst/`, as [js_module()]
#' takes it, so that `"js/dag.js"` is the file that the package installs as
#' `js/dag.js`.
#' A plugin class's module has the class's `PluginClass` without its id as its
#' default export, and a board option's module the board option's `Option`
#' without its ID, each beside the major of core's JS API that it was built
#' against.
#' The client registers each under its ID, a plugin class's as `{pkg}-{id}`.
#' The JSON Schema of the state of the plugins of a plugin class, or of the
#' value of a board option, sits beside its module, as `dag.schema.json` beside
#' `dag.js`, as a block class's UI module ships the schema of the block's state
#' beside it.
#' R writes the state of the plugins and the values of the board options of the
#' board that an app script builds by it, so that a value of length one stays
#' an array where a schema has one (see [blockr_ser()]).
#' It reads nothing else out of them.
#'
#' A package may define the object in its code, as a list, or export an empty
#' environment and fill it from its `.onLoad()`, as core does.
#' Core reads its block classes there from its `inst/registry/blocks.yml`, with
#' `read_blocks_yml()`, as in this sketch of a package with a block class and a
#' plugin class:
#'
#' ```r
#' ._BLOCKR_MODULES_ <- new.env(parent = emptyenv())
#'
#' .onLoad <- function(libname, pkgname) {
#'   ._BLOCKR_MODULES_$blocks <- blockr.core::read_blocks_yml(pkgname)
#'   ._BLOCKR_MODULES_$plugins <- list(
#'     new_dag_plugin = list(
#'       id = "dag_plugin",
#'       name = "DAG",
#'       module = "js/dag.js"
#'     )
#'   )
#' }
#' ```
#'
#' R checks no part against the constructors that it names, since a stale part
#' is a defect of the package's build, while a part that R cannot read stops
#' what reads it, with a classed error that names the package and each fault.
#'
#' R tells the client, in its reply to `hello`, where it serves the module of
#' the class of each plugin that the app runs, of each board option that the
#' packages that the app uses list and of each module that a loaded package
#' offers others, with the versions of the packages they come from.
#' It names where it serves a renderer's module in each output that the
#' renderer draws (see [new_renderer()]).
#' It serves a module, and the stylesheet beside it, under the package's name,
#' at the module's path in the package, with a hash of the file's content in
#' the URL, so that no browser runs a changed file from its cache.
#' It serves only the files it names, and nothing else of the package.
#'
#' @param pkg The name of the package
#'
#' @examples
#' names(read_blocks_yml("blockr.core"))
#'
#' @return The function `read_blocks_yml()` returns the block classes that the
#' package lists in its `inst/registry/blocks.yml`, a list by constructor,
#' which is empty where the package ships no such file.
#'
#' @export
read_blocks_yml <- function(pkg) {

  file <- package_file("registry/blocks.yml", pkg)

  if (!nzchar(file)) {
    return(list())
  }

  entries <- tryCatch(
    yaml::read_yaml(file),
    error = function(e) {
      blockr_abort(
        "The `inst/registry/blocks.yml` of package {pkg} holds no YAML: ",
        "{conditionMessage(e)}",
        class = c(
          "package_blocks_yaml_invalid",
          "package_blocks_invalid",
          "package_listing_invalid"
        )
      )
    }
  )

  coal(entries, list(), fail_all = FALSE)
}

#' @rdname read_blocks_yml
#' @format NULL
#' @usage NULL
#' @export
._BLOCKR_MODULES_ <- new.env(parent = emptyenv()) # nolint: object_name_linter.

listing_name <- "._BLOCKR_MODULES_"

listing_parts <- c("blocks", "plugins", "options", "modules")

# The available packages, by name, in order: the installed packages whose
# `Meta/nsInfo.rds` lists `._BLOCKR_MODULES_` among their exports, which R
# reads without loading any package, and the loaded packages that export it,
# among them those that `pkgload::load_all()` loaded, which sit in no library.
available_packages <- function() {

  installed <- unlst(lapply(.libPaths(), listing_exporters))
  loaded <- Filter(exports_listing, loadedNamespaces())

  sort(unique(c(installed, loaded)))
}

# The packages of the library `lib` whose `Meta/nsInfo.rds`, which
# `loadNamespace()` reads, lists `._BLOCKR_MODULES_` among their exports.
listing_exporters <- function(lib) {

  pkgs <- list.files(lib)
  files <- file.path(lib, pkgs, "Meta", "nsInfo.rds")
  keep <- file.exists(files)

  pkgs[keep][lgl_ply(files[keep], records_listing_export)]
}

records_listing_export <- function(file) {
  exports <- tryCatch(readRDS(file)[["exports"]], error = function(e) NULL)
  listing_name %in% exports
}

exports_listing <- function(pkg) {
  listing_name %in% getNamespaceExports(pkg)
}

# The loaded packages that export `._BLOCKR_MODULES_`, by name, in order.
loaded_listing_packages <- function() {
  sort(Filter(exports_listing, loadedNamespaces()))
}

# What package `pkg` lists in its `._BLOCKR_MODULES_`, which R reads once it
# has loaded the package: the object's four parts by name, each a list, which
# is empty where the object leaves it out. R ignores any other part, which a
# later version of core may read.
# A package that exports no such object lists nothing, as one that registers
# its block classes in the block registry of the process as it loads.
package_listing <- function(pkg) {

  loadNamespace(pkg)

  obj <- listing_object(pkg)

  if (is.environment(obj)) {
    obj <- as.list(obj, all.names = TRUE)
  }

  obj <- coal(obj, list(), fail_all = FALSE)

  if (!is.list(obj) || is.object(obj) || !has_unique_names(obj)) {
    blockr_abort(
      "Package {pkg} exports `._BLOCKR_MODULES_` as neither a list nor an ",
      "environment of its parts by name.",
      class = "package_listing_invalid"
    )
  }

  lapply(
    set_names(nm = listing_parts),
    function(part) coal(obj[[part]], list(), fail_all = FALSE)
  )
}

# The `._BLOCKR_MODULES_` that the loaded package `pkg` exports, or `NULL`
# where it exports none.
listing_object <- function(pkg) {
  if (exports_listing(pkg)) getExportedValue(pkg, listing_name)
}

# The modules that package `pkg` offers other packages' UIs, by ID,
# `{pkg}-{name}`, each as `js_module()` makes it.
listed_modules <- function(pkg, listing = package_listing(pkg)) {

  entries <- listing[["modules"]]

  faults <- if (!is_named_list(entries)) {
    "They are no list of module paths by name."
  } else {
    unlst(
      Map(
        function(path, name) {
          if (!is_nonempty_string(path)) {
            paste0("Its module ", name, " is no path, a non-empty string.")
          }
        },
        entries,
        names(entries)
      )
    )
  }

  check_listing_part(
    faults,
    pkg,
    "the modules that it offers others",
    "modules"
  )

  if (!length(entries)) {
    return(list())
  }

  set_names(
    lapply(entries, js_module, package = pkg),
    paste0(pkg, "-", names(entries))
  )
}

# The modules that the loaded packages offer other packages' UIs, by ID, as
# R names them in its reply to `hello`.
offered_modules <- function(pkgs = loaded_listing_packages()) {
  joined_by_id(lapply(pkgs, listed_modules))
}

# The board options that package `pkg` lists, by ID, each with its package and
# its module, as `js_module()` makes it.
listed_options <- function(pkg, listing = package_listing(pkg)) {

  entries <- listing[["options"]]

  faults <- if (!is_named_list(entries)) {
    "They are no list of board options by ID, each with its `module`."
  } else {
    unlst(Map(listing_entry_faults, entries, names(entries), list("module")))
  }

  check_listing_part(faults, pkg, "board options", "options")

  lapply(
    entries,
    function(x) {
      list(package = pkg, module = js_module(x[["module"]], package = pkg))
    }
  )
}

# The board options that the packages `pkgs` list, by ID. Two packages that
# list one ID stop the app, naming both, since the client defines one board
# option for each ID.
packages_options <- function(pkgs) {

  listed <- lapply(set_names(nm = pkgs), listed_options)

  ids <- unlst(lapply(listed, names))
  twice <- unique(ids[duplicated(ids)])

  if (length(twice)) {

    owners <- pkgs[lgl_ply(listed, function(x) twice[1L] %in% names(x))]

    blockr_abort(
      "{qty(twice)}The board option{?s} {twice} {?is/are} listed by more than ",
      "one package, {twice[1L]} by ", paste_enum(owners, quotes = ""),
      ", where the client defines one board option for each ID.",
      class = "package_options_conflict"
    )
  }

  joined_by_id(listed)
}

# The board options of the packages that an app uses, by ID, each with where R
# serves its module and the version of its shape, as R names them in its reply
# to `hello`. Those packages are core, the packages of the block classes in the
# app's block registry, `registry`, and the packages of the classes of the
# app's plugins, `plugins`, by the IDs of their classes, as `app_plugins()`
# gives them.
# R asks each board option's package for its version through its method of
# `board_option_version()`.
app_options <- function(registry = block_registry, plugins = list()) {

  entries <- registry_entries(registry)

  pkgs <- unique(
    c(
      "blockr.core",
      unlst(lst_xtr(entries, "package")),
      id_package(names(plugins))
    )
  )

  options <- packages_options(intersect(pkgs, loaded_listing_packages()))

  Map(
    function(x, id) c(x, list(version = option_version(id))),
    options,
    names(options)
  )
}

# The board options that the available packages list, by ID.
available_options <- function() {
  packages_options(available_packages())
}

# The lists by ID that `x` holds, joined into one list by ID.
joined_by_id <- function(x) {
  coal(unlst(unname(x), use_names = TRUE), list(), fail_all = FALSE)
}

# Stops on the faults that R found in the part `part` of the listing of package
# `pkg`, which lists `what` there, each fault a sentence.
check_listing_part <- function(faults, pkg, what, part) {

  if (!length(faults)) {
    return(invisible())
  }

  faults <- paste(faults, collapse = " ")

  blockr_abort(
    "Package {pkg} lists {what} in its `._BLOCKR_MODULES_` that R cannot ",
    "read. {faults}",
    class = c(paste0("package_", part, "_invalid"), "package_listing_invalid")
  )
}

# The faults of an entry of a part of a package's listing, under the key `key`,
# each as a sentence: an entry that is no named list, that lacks one of
# `fields` or gives one as anything but a non-empty string, where a field given
# as `NULL` counts as left out.
listing_entry_faults <- function(entry, key, fields) {

  fault <- function(...) paste0("Its entry for ", key, " ", ..., ".")

  if (!is_named_list(entry)) {
    return(fault("is no named list"))
  }

  lacks <- fields[lgl_ply(fields, function(x) is.null(entry[[x]]))]
  given <- setdiff(fields, lacks)
  strings <- given[!lgl_ply(entry[given], is_nonempty_string)]

  c(
    if (length(lacks)) {
      fault("lacks ", paste_enum(lacks))
    },
    if (length(strings)) {
      fault(
        "gives ", paste_enum(strings), " as ",
        if (length(strings) > 1L) "values other than" else "a value other than",
        " a non-empty string"
      )
    }
  )
}

# Whether `x` is a list by name, each name a non-empty string of its own, as
# YAML gives a mapping, where an empty list counts as one.
is_named_list <- function(x) {
  is.list(x) && !is.object(x) && has_unique_names(x)
}

core_runtime <- function() {
  js_module("js/blockr-core.js", package = "blockr.core")
}

# The JSON Schema of the state of the plugins of the class with the ID `id`,
# `{pkg}-{id}`, beside the module that the class's package lists for it, or
# `NULL` where none lists the class or its module ships none.
plugin_class_schema <- function(id, classes = available_plugin_classes()) {
  module_schema(classes[[id]][["module"]])
}

# The JSON Schema of the value of the board option with the ID `id`, beside the
# module that the package that lists the board option gives it, or `NULL`
# where no available package lists it or its module ships none.
board_option_schema <- function(id, options = available_options()) {
  module_schema(options[[id]][["module"]])
}

# The JSON Schema that a module ships beside it, as `dag.schema.json` beside
# `dag.js`, decoded, or `NULL` for no module or where it ships none.
module_schema <- function(module) {

  if (is.null(module)) {
    return(NULL)
  }

  file <- module_file(module, ".schema.json")

  if (nzchar(file)) decode_schema(file)
}
