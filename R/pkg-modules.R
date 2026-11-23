#' What a package lists for core
#'
#' A package lists what it offers core in one object that it exports,
#' `._BLOCKR_MODULES_`: its block classes, its plugin classes, its board
#' options, the modules that it offers other packages' UIs and its output
#' families.
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
#' fills, of up to five parts, each a list or, for the output families, a
#' character vector:
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
#'   Such a module may draw the menus of board actions, as core's own menu,
#'   `blockr.core-board_action_menu`, does.
#'   The client draws them with the module that the blockr option
#'   `board_action_menu` names by its ID, which is core's menu unless an app
#'   names another, as in
#'   `options(blockr.board_action_menu = "mykit-board_action_menu")`.
#'   Its default export has the type `MenuModule` of `@blockr/core`, and
#'   `menuModuleTests()` of `@blockr/core/stubs` tests it.
#' * Its output families, in `families`: the name of each output family that
#'   it offers, which is the family's class, and under which its namespace
#'   binds the family (see [new_output_family()]).
#'   An open family hands its results to every family that the available
#'   packages list there.
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
#' the class of each plugin that the app runs, of each board option that an
#' available package lists and of each module that an available package offers
#' others, as [serve()] read them as it built the app, with the versions of the
#' packages they come from.
#' It names where it serves a renderer's module in each output that the
#' renderer draws (see [new_renderer()]).
#' It serves a module, and the stylesheet beside it, under the package's name,
#' at the module's path in the package, with a hash of the file's content in
#' the URL, so that no browser runs a changed file from its cache.
#' It serves the other files in the module's directory as well, each at its
#' path, so that a module or its stylesheet can import a file beside it.
#' Such a file has no hash in its URL, so the package names it with a hash of
#' its content, as blockr.core names the bundle that holds React
#' `vendor-<hash>.js`.
#' It serves nothing else of the package.
#'
#' R reads the output families that the available packages list as [serve()]
#' builds the app, as it reads their board options.
#' It gets each family from the namespace of the package that lists it, by
#' the name that the package lists.
#' It names their board options and renderers in the entry of each block class
#' whose family is open, in the block registry (see [new_output_family()]).
#' Two packages that list a family of one class stop the app, naming both,
#' since R tells families apart by their class.
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

listing_parts <- c("blocks", "plugins", "options", "modules", "families")

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

# What package `pkg` lists in its `._BLOCKR_MODULES_`, which R gets from the
# package's namespace by the package's name, and so loads the package: the
# object's five parts by name, each an empty list where the object leaves it
# out. R ignores any other part, which a later version of core may read.
# A package that exports no such object lists nothing, as one that registers
# its block classes in the block registry of the process as it loads.
package_listing <- function(pkg) {

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

# The `._BLOCKR_MODULES_` that package `pkg` exports, or `NULL` where it
# exports none. Asking for the package's exports loads the package.
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

# The modules that the available packages offer other packages' UIs, by ID,
# which an app holds as `serve()` reads them, and R names in its reply to
# `hello`.
offered_modules <- function(pkgs = available_packages()) {
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

  joined_by_id(
    lapply(set_names(nm = pkgs), listed_options),
    function(twice, owners) {
      blockr_abort(
        "{qty(twice)}The board option{?s} {twice} {?is/are} listed by more ",
        "than one package, {twice[1L]} by ", paste_enum(owners, quotes = ""),
        ", where the client defines one board option for each ID.",
        class = "package_options_conflict"
      )
    }
  )
}

# The board options of an app: those that the available packages list, by ID,
# each with its package, where R serves its module, the version of its shape
# and its requests, which the app holds as `serve()` reads them.
# R names all but the requests in its reply to `hello`, and answers a request
# of the option with the function that the app holds for it.
# R reads them from every available package, as it does the modules that
# packages offer each other, so that a reply names a board option that a
# block's request reads, of a package whose block classes and plugins the app
# offers none of.
# R asks each board option's package for its version and its requests through
# its methods of `board_option_version()` and `board_option_requests()`, which
# the package registered as R read its listing, and so loaded it.
app_options <- function(pkgs = available_packages()) {

  options <- packages_options(pkgs)

  Map(
    function(x, id) {
      requests <- board_option_requests(new_board_option(id))
      c(x, list(version = option_version(id), requests = requests))
    },
    options,
    names(options)
  )
}

# The board options that the available packages list, by ID.
available_options <- function() {
  packages_options(available_packages())
}

# The output families that package `pkg` lists in the `families` of its
# `._BLOCKR_MODULES_`, by class, each as the package's namespace binds it
# under the name that the package lists, which has to be the family's class.
listed_families <- function(pkg, listing = package_listing(pkg)) {

  entries <- listing[["families"]]

  shaped <- is.character(entries) ||
    (is.list(entries) && !is.object(entries))

  if (!shaped || !all(lgl_ply(entries, is_nonempty_string))) {
    check_listing_part(
      "They are no names of output families, each a non-empty string.",
      pkg,
      "output families",
      "families"
    )
  }

  classes <- unique(as.character(unlst(entries)))

  if (!length(classes)) {
    return(list())
  }

  found <- lapply(
    set_names(nm = classes),
    get0,
    envir = asNamespace(pkg),
    inherits = FALSE
  )

  bound <- lgl_ply(
    classes,
    function(x) {
      is_output_family(found[[x]]) && identical(class(found[[x]])[1L], x)
    }
  )

  faults <- if (!all(bound)) {
    paste0(
      "It binds no output family of class ", classes[!bound],
      " under that name."
    )
  }

  check_listing_part(faults, pkg, "output families", "families")

  found
}

# The output families that the available packages list, by class, which an app
# holds as `serve()` reads them, and to which an open family hands its results.
# Two packages that list a family of one class stop the app, naming both,
# since R tells families apart by their class.
app_families <- function(pkgs = available_packages()) {

  joined_by_id(
    lapply(set_names(nm = pkgs), listed_families),
    function(twice, owners) {
      blockr_abort(
        "{qty(twice)}The output famil{?y/ies} {twice} {?is/are} listed by ",
        "more than one package, {twice[1L]} by ",
        paste_enum(owners, quotes = ""),
        ", where R tells families apart by their class.",
        class = "package_families_conflict"
      )
    }
  )
}

# The lists by ID that `x` holds, joined into one list by ID.
# Where `conflict` is given, two lists that hold one ID stop the join: R calls
# `conflict()` with the IDs that more than one list holds and the names, in
# `x`, of the lists that hold the first of them, such as their packages.
joined_by_id <- function(x, conflict = NULL) {

  if (not_null(conflict)) {

    ids <- unlst(lapply(x, names))
    twice <- unique(ids[duplicated(ids)])

    if (length(twice)) {
      conflict(twice, names(x)[lgl_ply(x, function(y) twice[1L] %in% names(y))])
    }
  }

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
