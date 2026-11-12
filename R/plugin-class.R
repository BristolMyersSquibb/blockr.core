#' Plugins
#'
#' A plugin extends a board as a whole, such as a frontend that draws the page,
#' the DAG or a document of `blockr.md`, and is core's one way to extend a board
#' besides block classes.
#' An app runs at most one plugin of each plugin class, and the app says which
#' plugins run, not the board: [serve()] takes them in `plugins`, as rows that
#' give each plugin's class by its package and its id.
#' So a plugin is identified by the id of its class, and named by its class.
#'
#' A plugin object holds the state that a plugin gives the board that an app
#' script builds, as a block holds its state, and [new_board()] takes such
#' objects in `plugins` for their state alone.
#' A package's constructor `new_{name}_plugin()` returns one, made by
#' `new_plugin()`, of class `c("{name}_plugin", ..., "plugin")`.
#' The constructor's args are the plugin's state, such as the positions of the
#' DAG, which it passes to `new_plugin()` as a list by name, followed by the id
#' of the plugin's class, as `new_plugin(list(positions = positions),
#' "dag_plugin")`.
#' The class vector, `class`, defaults to the id, and serves S3 dispatch alone.
#'
#' The id of a plugin's class in full is `{pkg}-{id}`, as
#' `blockr.dag-dag_plugin`, where the package is the one that the constructor
#' lives in, since that package defines the class in JavaScript, in a module of
#' its own, whose default export the client registers under that id.
#' So an app script defines no plugin class, and `new_plugin()` raises an error
#' where the constructor that calls it lives outside a package.
#' A package lists each of its plugin classes in its `._BLOCKR_MODULES_`, by
#' its constructor, with its id, its name, the human-readable form of its id,
#' such as "Document" for `md_plugin`, which titles the plugin's panel, and its
#' module (see [read_blocks_yml()]).
#' The function `available_plugins()` gives the plugin classes that the
#' available packages list, as a data frame with a row for each class, with
#' its package in `package` and its id in `id`, which identify the class.
#' The rest of a class, such as its name, stays with its package's listing.
#'
#' Core defines two plugin classes beside `plugin`, since every page relies on
#' what each does: `frontend_plugin`, the class of a plugin that draws the page,
#' and `storage_plugin`, the class of a plugin through which every frontend
#' saves boards and opens them.
#' A package's class extends one of them or `plugin` itself, as
#' `c("session_plugin", "storage_plugin", "plugin")`.
#' Core's own plugins do so too, and none of them keeps any state.
#' Its basic frontend, which `new_basic_frontend_plugin()` makes, is of class
#' `c("basic_frontend_plugin", "frontend_plugin", "plugin")`.
#' Its storage plugin, which `new_file_storage_plugin()` makes, is of class
#' `c("file_storage_plugin", "storage_plugin", "plugin")`, and saves a board as
#' a download and opens an upload.
#' Its Edit board plugin, which `new_edit_board_plugin()` makes, is of class
#' `c("edit_board_plugin", "plugin")`.
#' It adds, renames and removes blocks, and adds, changes and removes links and
#' stacks.
#' Every frontend places it as it places any plugin with a UI.
#' An app runs it unless its rows leave it out, as it runs every plugin class
#' that `available_plugins()` gives.
#' So the user can add the first block to the board that `serve(new_board())`
#' draws.
#'
#' The version of a plugin class is the version of the shape of its plugins'
#' state, a whole number from 1, which the class's method of `plugin_version()`
#' gives, and which is 1 for a class without one.
#' A package raises it as it changes that shape, so that the client migrates
#' the state of a plugin that a JSON board holds at an older version.
#' R calls the method on the plugin that the class's constructor makes, called
#' without args, and sends the version of the class of each of the app's
#' plugins in its reply to `hello`.
#' A plugin class that needs R gives its requests through its method of
#' [plugin_requests()], which R calls on such a plugin too.
#'
#' @param state The plugin's state, the args of its constructor, as a list by
#'   name
#' @param id The id of the plugin's class without its package, named as
#'   `{name}_plugin`, as `"dag_plugin"`
#' @param class The plugin's class vector without `"plugin"`, its own class
#'   first, as `c("dock_plugin", "frontend_plugin")`, which defaults to `id`
#' @param x Plugin
#' @param ... Generic consistency
#'
#' @examples
#' \dontrun{
#' # In a package such as blockr.dag, whose plugin class is
#' # blockr.dag-dag_plugin
#' new_dag_plugin <- function(positions = list()) {
#'   new_plugin(list(positions = positions), "dag_plugin")
#' }
#'
#' plugin_version.dag_plugin <- function(x, ...) 2L
#' }
#'
#' available_plugins()
#'
#' plugin_class_id(new_basic_frontend_plugin())
#'
#' @return The constructors `new_plugin()`, `new_basic_frontend_plugin()`,
#' `new_file_storage_plugin()` and `new_edit_board_plugin()` return a `plugin`
#' object, and `is_plugin()` a scalar logical.
#' Of the getters, `plugin_class_id()` returns the id of the plugin's class in
#' full, a string, and `plugin_state()` the state, a list by name.
#' The generic `plugin_version()` returns a whole number, and
#' `available_plugins()` a data frame with a row for each plugin class.
#'
#' @export
new_plugin <- function(state = list(), id, class = id) {

  package <- pkg_name(parent.frame())

  if (is.null(package)) {
    blockr_abort(
      "A plugin is made by the constructor of the package that defines its ",
      "class, while this constructor lives outside a package.",
      class = "plugin_package_missing"
    )
  }

  if (missing(id) || !is_plugin_id(id)) {
    blockr_abort(
      "A plugin's class has an id of its own, a string named as ",
      "`{{name}}_plugin`, which is none of {core_plugin_classes()}.",
      class = "plugin_id_invalid"
    )
  }

  class <- check_plugin_class(class)

  if (!is.list(state) || is.object(state) ||
        (length(state) && !all(nzchar(coal(names(state), ""))))) {
    blockr_abort(
      "The state of a plugin of class {id} is expected to be a list of its ",
      "constructor's args by name.",
      class = "plugin_state_invalid"
    )
  }

  structure(
    set_names(state, coal(names(state), character(), fail_all = FALSE)),
    class_id = paste0(package, "-", id),
    class = unique(c(class, "plugin"))
  )
}

# Whether `id` is the id of a plugin class without its package, named as
# `{name}_plugin`, and none of core's classes that others extend.
is_plugin_id <- function(id) {
  is_string(id) && !is.na(id) && grepl(".+_plugin$", id) &&
    !id %in% core_plugin_classes()
}

# A plugin's class vector without "plugin", whose first class is the plugin's
# own, as `{name}_plugin`, which is none of core's.
check_plugin_class <- function(class) {

  own <- if (is.character(class) && length(class)) class[1L] else ""

  if (!is_plugin_id(own) || anyNA(class)) {
    blockr_abort(
      "A plugin's class vector is expected to start with a class of its own, ",
      "named as `{{name}}_plugin`, and none of {core_plugin_classes()}.",
      class = "plugin_class_invalid"
    )
  }

  setdiff(class, "plugin")
}

core_plugin_classes <- function() {
  c("plugin", "frontend_plugin", "storage_plugin")
}

#' @rdname new_plugin
#' @export
new_basic_frontend_plugin <- function() {
  new_plugin(
    list(),
    "basic_frontend_plugin",
    c("basic_frontend_plugin", "frontend_plugin")
  )
}

#' @rdname new_plugin
#' @export
new_file_storage_plugin <- function() {
  new_plugin(
    list(),
    "file_storage_plugin",
    c("file_storage_plugin", "storage_plugin")
  )
}

#' @rdname new_plugin
#' @export
new_edit_board_plugin <- function() {
  new_plugin(list(), "edit_board_plugin")
}

#' @rdname new_plugin
#' @export
is_plugin <- function(x) {
  inherits(x, "plugin")
}

# A getter checks only that it reads a plugin.
check_plugin <- function(x) {

  if (!is_plugin(x)) {
    blockr_abort(
      "Expecting a plugin to inherit from \"plugin\".",
      class = "plugin_inheritance_invalid"
    )
  }

  invisible(x)
}

#' @rdname new_plugin
#' @export
plugin_class_id <- function(x) {
  check_plugin(x)
  attr(x, "class_id", exact = TRUE)
}

#' @rdname new_plugin
#' @export
plugin_state <- function(x) {
  check_plugin(x)
  bare_list(x)
}

#' @rdname new_plugin
#' @export
plugin_version <- function(x, ...) {
  UseMethod("plugin_version")
}

#' @rdname new_plugin
#' @export
plugin_version.plugin <- function(x, ...) {
  1L
}

# Whether a plugin is a frontend or a storage plugin, of which an app runs one
# and one at most.
is_frontend_plugin <- function(x) {
  inherits(x, "frontend_plugin")
}

is_storage_plugin <- function(x) {
  inherits(x, "storage_plugin")
}

# The package that each ID names, as `blockr.dag` for `blockr.dag-dag_plugin`,
# the part before the first `-`. An ID without a `-` names no package, as that
# of a block class that an app script defines, and gives `NA`.
id_package <- function(ids) {

  res <- sub("-.*$", "", ids)
  res[!grepl("-", ids, fixed = TRUE)] <- NA_character_

  res
}

# The class that each ID names, as `dag_plugin` for `blockr.dag-dag_plugin`,
# the part after the first `-`.
id_class <- function(ids) {
  sub("^[^-]*-", "", ids)
}

#' @rdname new_plugin
#' @export
available_plugins <- function() {

  classes <- available_plugin_classes()

  data.frame(
    package = chr_xtr(classes, "package"),
    id = chr_xtr(classes, "id")
  )
}

# The plugin classes that the available packages list, by the full ID of each
# class, `{pkg}-{id}`, each with its package, its id, its name, its
# constructor and its module.
available_plugin_classes <- function() {
  joined_by_id(lapply(available_packages(), listed_plugin_classes))
}

# The plugin classes that package `pkg` lists in the `plugins` of its
# `._BLOCKR_MODULES_`, by the full ID of each class, `{pkg}-{id}`, each with
# its package, its id, its name, its constructor, which R looks up by its name
# in the package's namespace, and its module, as `js_module()` makes it.
listed_plugin_classes <- function(pkg, listing = package_listing(pkg)) {

  entries <- listing[["plugins"]]

  faults <- if (!is_named_list(entries)) {
    paste(
      "They are no list of plugin constructors by name, each with the `id`,",
      "`name` and `module` of its class."
    )
  } else {
    plugin_listing_faults(entries)
  }

  check_listing_part(faults, pkg, "plugin classes", "plugins")

  if (!length(entries)) {
    return(list())
  }

  classes <- Map(
    function(entry, ctor) {
      list(
        package = pkg,
        id = entry[["id"]],
        name = entry[["name"]],
        constructor = get(ctor, envir = asNamespace(pkg), mode = "function"),
        module = js_module(entry[["module"]], package = pkg)
      )
    },
    entries,
    names(entries)
  )

  set_names(classes, paste0(pkg, "-", chr_xtr(entries, "id")))
}

# The faults of the plugin classes that a package lists, each as a sentence:
# an entry that gives no `id`, `name` and `module` as strings, an id that is
# not named as `{name}_plugin`, and two constructors that list one id.
plugin_listing_faults <- function(entries) {

  fields <- c("id", "name", "module")

  faults <- unlst(
    Map(listing_entry_faults, entries, names(entries), list(fields))
  )

  if (length(faults)) {
    return(faults)
  }

  ids <- chr_xtr(entries, "id")
  named <- lgl_ply(ids, is_plugin_id)

  twice <- unique(ids[duplicated(ids)])

  c(
    if (!all(named)) {
      paste0(
        "Its entry for ", names(entries)[!named], " gives the id ",
        ids[!named], ", which is not named as `{name}_plugin`, or is one of ",
        "core's classes that others extend."
      )
    },
    if (length(twice)) {
      paste0(
        "Its constructors ", paste_enum(names(entries)[ids %in% twice]),
        " list one id, ", twice[1L], "."
      )
    }
  )
}

# The plugin that the constructor of the plugin class with the ID `id`,
# `{pkg}-{id}`, makes when called without args, on which R calls the class's
# methods of `plugin_version()` and `plugin_requests()`, since the class is
# known by its ID alone.
# The class is one that an available package lists.
plugin_of_class <- function(id, available = available_plugin_classes()) {

  cls <- available[[id]]

  if (is.null(cls)) {
    blockr_abort(
      "No available package lists the plugin class {id} (see ",
      "`available_plugins()`).",
      class = "plugin_class_unknown"
    )
  }

  cls[["constructor"]]()
}

# The version of the plugin class with the ID `id`: what the method of
# `plugin_version()` for the class returns, called on the plugin that the
# class's constructor makes.
plugin_class_version <- function(id, available = available_plugin_classes()) {
  checked_plugin_version(plugin_of_class(id, available))
}

# The version that the class of the plugin `x` gives, through its method of
# `plugin_version()`, a whole number from 1.
checked_plugin_version <- function(x) {

  version <- plugin_version(x)

  if (!is_count(version, allow_zero = FALSE)) {
    blockr_abort(
      "The method of `plugin_version()` for plugin class ",
      "{plugin_class_id(x)} returns no whole number of 1 or more.",
      class = "plugin_version_invalid"
    )
  }

  version
}

# The requests of the plugin class with the ID `id`, a list of functions by
# name: what the method of `plugin_requests()` for the class returns, called on
# the plugin that the class's constructor makes.
plugin_class_requests <- function(id, available = available_plugin_classes()) {

  reqs <- plugin_requests(plugin_of_class(id, available))

  if (!is.list(reqs) || is.object(reqs) || !all(lgl_ply(reqs, is.function)) ||
        !has_unique_names(reqs)) {
    blockr_abort(
      "The method of `plugin_requests()` for plugin class {id} returns no ",
      "list of functions by name.",
      class = "plugin_requests_invalid"
    )
  }

  reqs
}

# The app's plugins, which run on every board that the app opens, from the
# rows that `serve()` takes in `plugins`, by the ID of each plugin's class, in
# the order of the rows: each with its class's name, version and module.
# R reads the rows and refuses them where they name a class that no available
# package lists, or a class twice. It tells a frontend and a storage plugin by
# the R class of the plugin that the class's constructor makes.
# Core's basic frontend runs only where no other row is a frontend, and core's
# storage plugin only where no other row is a storage plugin, and R refuses
# rows that then hold no frontend, two frontends or two storage plugins.
app_plugins <- function(rows, available = available_plugin_classes()) {

  rows <- plugin_rows(rows, available)
  ids <- paste(rows[["package"]], rows[["id"]], sep = "-")

  twice <- unique(ids[duplicated(ids)])

  if (length(twice)) {
    blockr_abort(
      "The app runs one plugin of each class, while its rows name the ",
      "{qty(twice)}plugin class{?es} {twice} more than once.",
      class = "app_plugins_invalid"
    )
  }

  plugins <- lapply(set_names(available[ids], ids), function(cls) {
    cls[["constructor"]]()
  })

  roles <- list(
    list(
      test = is_frontend_plugin,
      core = "blockr.core-basic_frontend_plugin",
      what = "frontend"
    ),
    list(
      test = is_storage_plugin,
      core = "blockr.core-file_storage_plugin",
      what = "storage plugin"
    )
  )

  held <- list()

  for (role in roles) {

    of_role <- names(plugins)[lgl_ply(plugins, role$test)]

    if (length(setdiff(of_role, role$core))) {
      plugins <- plugins[names(plugins) != role$core]
      of_role <- setdiff(of_role, role$core)
    }

    held[[role$what]] <- of_role
  }

  if (!length(held[["frontend"]])) {
    blockr_abort(
      "The app runs no frontend, since its rows name none, where an app runs ",
      "one, such as core's basic frontend, blockr.core-basic_frontend_plugin.",
      class = "app_plugins_invalid"
    )
  }

  for (role in roles) {

    of_role <- held[[role$what]]

    if (length(of_role) > 1L) {
      blockr_abort(
        "The app runs one {role$what} at most, while its rows name the ",
        "{role$what}s {of_role}.",
        class = "app_plugins_invalid"
      )
    }
  }

  lapply(
    set_names(nm = names(plugins)),
    function(id) {
      list(
        name = available[[id]][["name"]],
        version = checked_plugin_version(plugins[[id]]),
        module = available[[id]][["module"]]
      )
    }
  )
}

# The plugin classes that an app runs, as `serve()` takes them in `plugins`:
# the rows of a data frame, with the class's package in `package` and its id
# in `id`, in their order (see `check_class_rows()`).
# Each names a class that an available package lists, and a row that names
# none stops the app.
plugin_rows <- function(rows, available) {

  listed <- data.frame(
    package = chr_xtr(available, "package"),
    id = chr_xtr(available, "id")
  )

  rows <- check_class_rows(
    rows,
    "plugins that an app runs",
    "available_plugins()",
    "app_plugins_invalid"
  )

  check_plugin_packages(rows[["package"]], listed)

  key <- function(x) paste(x[["package"]], x[["id"]], sep = "\r")
  unknown <- !key(rows) %in% key(listed)
  rows_unknown <- paste(rows[["id"]], "of", rows[["package"]])[unknown]

  if (length(rows_unknown)) {
    blockr_abort(
      "The app runs {qty(rows_unknown)}the plugin class{?es} {rows_unknown}, ",
      "which {?its/their} package{?s} {?does/do} not list.",
      class = "app_plugins_unknown"
    )
  }

  rows
}

check_plugin_packages <- function(packages, listed) {

  missing <- setdiff(packages, listed[["package"]])

  if (length(missing)) {
    blockr_abort(
      "{qty(missing)}The package{?s} {missing}, whose plugins the app runs, ",
      "{?is/are} no available package that lists plugin classes (see ",
      "`available_plugins()`).",
      class = "app_plugins_unknown"
    )
  }

  invisible()
}

#' @export
format.plugin <- function(x, ...) {

  state <- plugin_state(x)

  c(
    paste0("<", class(x)[1L], "> ", plugin_class_id(x)),
    if (length(state)) {
      paste0(names(state), ": ", chr_ply(state, format_head))
    }
  )
}

#' @export
print.plugin <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
str_value.plugin <- function(x, ...) {
  paste0("<", class(x)[1L], "> ", plugin_class_id(x))
}

#' @export
str.plugin <- function(object, ...) {
  cat_str_value(object, ...)
}
