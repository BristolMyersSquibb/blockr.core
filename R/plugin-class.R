#' Plugins
#'
#' A plugin extends a board as a whole, such as a frontend that draws the page,
#' the DAG or a document of `blockr.md`, and is core's one way to extend a board
#' besides block classes.
#' An app runs at most one plugin of each plugin class, and the app says which
#' plugins run, not the board: [serve()] takes them in `plugins`, as rows that
#' give each plugin's class by its package and its id.
#' So a plugin is identified by its class's package and id, and named by its
#' class.
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
#' A plugin's class is identified by its package and its id, each a part of
#' its own, as `list(package = "blockr.dag", id = "dag_plugin")`, which
#' `plugin_class_id()` returns.
#' The package is the one that the constructor lives in, since that package
#' defines the class in JavaScript, in a module of its own, whose default
#' export the client registers under that package and id.
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
#' A plugin class names the board options that its plugin reads through its
#' method of `plugin_options()`, by their constructors, as a request names
#' those that it reads (see [new_block_request()]).
#' The method returns a constructor, such as `new_dark_mode_option`, or a list
#' of them, and core's method for `plugin` names none.
#' R calls it on the same plugin, and sends the IDs of the board options in
#' its reply to `hello`.
#' The client puts them on the board before it starts the plugin or draws its
#' UI, as core's basic frontend gets `dark_mode`.
#' A plugin class that needs R gives its requests through its method of
#' [plugin_requests()], which R calls on such a plugin too.
#'
#' The app's frontend may take arguments of [serve()], as the dock takes the
#' items of its bar in `navbar`, through its class's method of
#' `frontend_args()`.
#' As `serve()` is called, R calls the method on the plugin that the
#' frontend's constructor makes, with every argument in the `...` of
#' `serve()`.
#' The method takes the arguments that the frontend knows and refuses the
#' rest, naming the frontend.
#' It returns what the frontend reads of them, as a list by name.
#' R writes that list as a JSON object, and sends it in its reply to `hello`,
#' so that no board or file sets it.
#' Core then calls the `render()` of the frontend's module with it, in `args`.
#' A method cannot hand the arguments that it does not know on to core's
#' method with `NextMethod()`, since R would hand on those that it took as
#' well, so each method refuses them itself.
#' Core's method for `frontend_plugin` takes no argument and returns an empty
#' list.
#' So core's basic frontend refuses every argument, and so does a frontend
#' whose class has no method of its own.
#'
#' The app's frontend gives the values of the theme variables that it wants
#' its own, such as the color of its cards, through its class's method of
#' `frontend_theme()`.
#' The method returns the frontend's theme, as [blockr.ui::new_blockr_theme()]
#' makes it, with the frontend's values for light mode and for dark mode.
#' Core's method for `frontend_plugin` returns a theme with no values.
#' So a frontend that keeps every default of blockr.ui, as core's basic
#' frontend does, has no method of its own.
#' A frontend's tests compare [blockr.ui::unset_variables()] of its theme with
#' the defaults that it keeps on purpose, so that a theme variable that a later
#' blockr.ui adds fails them until the frontend sets it or keeps its default.
#'
#' A plugin class may complete the state of its plugins on the board that a
#' script builds, through its method of `complete_plugin()`.
#' Once the board is valid, [new_board()] calls the method on each of the
#' board's plugins.
#' The method gets the board in `board`.
#' In `aliases`, it gets the names that the script gives the board's plugins,
#' each mapped to the package and the id of its plugin's class, as
#' `list(dag = list(package = "blockr.dag", id = "dag_plugin"))`.
#' It returns the plugin with its state completed, such as with each such name
#' turned into the package and the id of a class, and the board holds what it
#' returns.
#' It may refuse a state that names what the board lacks, such as a block, so
#' that a script's mistake fails where the script builds its board.
#' Each method gets the board as [validate_board()] passed it, before any
#' plugin completed its state, so the order of the plugins does not matter.
#' Core's method for `plugin` returns the plugin as it is.
#' The board holds each plugin under the package and the id of its class, so
#' `new_board()` refuses a result that is not a plugin of the same class.
#' R reads nothing of a JSON board but its format, so only `new_board()` calls
#' `complete_plugin()`, and the names exist only there.
#'
#' @param state The plugin's state, the args of its constructor, as a list by
#'   name
#' @param id The id of the plugin's class without its package, named as
#'   `{name}_plugin`, as `"dag_plugin"`
#' @param class The plugin's class vector without `"plugin"`, its own class
#'   first, as `c("dock_plugin", "frontend_plugin")`, which defaults to `id`
#' @param x Plugin
#' @param board For `complete_plugin()`, the board that holds the plugin, as
#'   [validate_board()] passed it
#' @param aliases For `complete_plugin()`, the names that the script gives the
#'   board's plugins, as a list by name of the classes of the plugins, each a
#'   list of its `package` and its `id`
#' @param ... For `frontend_args()`, the arguments that [serve()] hands the
#'   app's frontend, and generic consistency otherwise
#'
#' @examples
#' \dontrun{
#' # In a package such as blockr.dag, whose plugin class is dag_plugin
#' new_dag_plugin <- function(positions = list()) {
#'   new_plugin(list(positions = positions), "dag_plugin")
#' }
#'
#' plugin_version.dag_plugin <- function(x, ...) 2L
#'
#' # In a package whose frontend, of the class grid_plugin, reads the board
#' # option compact, which the package defines, and core's dark_mode
#' plugin_options.grid_plugin <- function(x, ...) {
#'   list(new_compact_option, blockr.core::new_dark_mode_option)
#' }
#'
#' # In a package whose frontend, of the class grid_plugin, shows a title that
#' # the app script gives, as in serve(board, title = "Trials")
#' frontend_args.grid_plugin <- function(x, ..., title = "Grid") {
#'   if (...length()) {
#'     stop("The Grid frontend takes no argument of `serve()` but `title`.")
#'   }
#'   list(title = title)
#' }
#'
#' # In a package whose frontend, of the class grid_plugin, draws its cards in
#' # a color of its own, in each mode
#' frontend_theme.grid_plugin <- function(x, ...) {
#'   blockr.ui::new_blockr_theme(
#'     light = list("--bk-color-card" = "#fafafa"),
#'     dark = list("--bk-color-card" = "#262626")
#'   )
#' }
#'
#' # In a package whose plugin class, notes_plugin, keeps the plugin that its
#' # notes are about, which a script may give by a name, as in
#' # list(dag = new_dag_plugin(), new_notes_plugin(about = "dag"))
#' complete_plugin.notes_plugin <- function(x, board, aliases, ...) {
#'   if (is.character(x[["about"]]) && x[["about"]] %in% names(aliases)) {
#'     x[["about"]] <- aliases[[x[["about"]]]]
#'   }
#'   x
#' }
#' }
#'
#' available_plugins()
#'
#' plugin_class_id(new_basic_frontend_plugin())
#'
#' frontend_args(new_basic_frontend_plugin())
#'
#' frontend_theme(new_basic_frontend_plugin())
#'
#' @return The constructors `new_plugin()`, `new_basic_frontend_plugin()`,
#' `new_file_storage_plugin()` and `new_edit_board_plugin()` return a `plugin`
#' object, and `is_plugin()` a scalar logical.
#' Of the getters, `plugin_class_id()` returns the class of the plugin, a list
#' of its `package` and its `id`, each a string, and `plugin_state()` the
#' state, a list by name.
#' The generic `plugin_version()` returns a whole number, `plugin_options()`
#' a board option constructor, a list of them or `NULL`, and
#' `available_plugins()` a data frame with a row for each plugin class.
#' The generic `frontend_args()` returns what the frontend reads of the
#' arguments, a list by name, `frontend_theme()` the frontend's theme, an
#' object of class `blockr_theme`, and `complete_plugin()` the plugin with its
#' state completed.
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
    class_id = pkg_id(package, id),
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

#' @rdname new_plugin
#' @export
plugin_options <- function(x, ...) {
  UseMethod("plugin_options")
}

#' @rdname new_plugin
#' @export
plugin_options.plugin <- function(x, ...) {
  NULL
}

#' @rdname new_plugin
#' @export
plugin_options.basic_frontend_plugin <- function(x, ...) {
  new_dark_mode_option
}

#' @rdname new_plugin
#' @export
frontend_args <- function(x, ...) {
  UseMethod("frontend_args")
}

#' @rdname new_plugin
#' @export
frontend_args.frontend_plugin <- function(x, ...) {

  # R reads the arguments unevaluated, so that one that calls a function of a
  # package that the app has not loaded, as `navbar = default_navbar()`
  # without blockr.dock, fails with this error too.
  nms <- coal(...names(), character(...length()), fail_all = FALSE)

  if (length(nms)) {
    blockr_abort(
      "The app's frontend, {pkg_id_label(plugin_class_id(x))}, takes no ",
      "argument of `serve()`, and was given {arg_labels(nms)}.",
      class = "frontend_args_invalid"
    )
  }

  list()
}

#' @rdname new_plugin
#' @export
frontend_theme <- function(x, ...) {
  UseMethod("frontend_theme")
}

#' @rdname new_plugin
#' @export
frontend_theme.frontend_plugin <- function(x, ...) {
  blockr.ui::new_blockr_theme()
}

#' @rdname new_plugin
#' @export
complete_plugin <- function(x, board, aliases, ...) {
  UseMethod("complete_plugin")
}

#' @rdname new_plugin
#' @export
complete_plugin.plugin <- function(x, board, aliases, ...) {
  x
}

# Whether a plugin is a frontend or a storage plugin, of which an app runs one
# and one at most.
is_frontend_plugin <- function(x) {
  inherits(x, "frontend_plugin")
}

is_storage_plugin <- function(x) {
  inherits(x, "storage_plugin")
}

#' @rdname new_plugin
#' @export
available_plugins <- function() {

  classes <- flat_by_package(available_plugin_classes())

  data.frame(
    package = chr_xtr(classes, "package"),
    id = chr_xtr(classes, "id")
  )
}

# The plugin classes that the available packages list, by package, each
# package's by the id of each class, each with its package, its id, its name,
# its constructor and its module.
# A package that lists none has no entry.
available_plugin_classes <- function() {

  pkgs <- available_packages()

  Filter(length, lapply(set_names(nm = pkgs), listed_plugin_classes))
}

# The plugin class that the id `id` names among those of `available`,
# as `available_plugin_classes()` gives them, or `NULL` where none is.
plugin_class_entry <- function(id, available = available_plugin_classes()) {

  if (!is_pkg_id(id) || is.null(id[["package"]])) {
    return(NULL)
  }

  available[[id[["package"]]]][[id[["id"]]]]
}

# The plugin classes that package `pkg` lists in the `plugins` of its
# `._BLOCKR_MODULES_`, by the id of each class, each with its package, its id,
# its name, its constructor, which R looks up by its name in the package's
# namespace, and its module, as `js_module()` makes it.
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

  set_names(classes, chr_xtr(entries, "id"))
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

# The plugin that the constructor of the plugin class that the id `id` names
# makes when called without args, on which R calls the class's methods,
# such as that of `frontend_args()`, since the class is known by its package
# and its id alone.
# The class is one that an available package lists.
plugin_of_class <- function(id, available = available_plugin_classes()) {

  cls <- plugin_class_entry(id, available)

  if (is.null(cls)) {
    blockr_abort(
      "No available package lists the plugin class {pkg_id_label(id)} ",
      "(see `available_plugins()`).",
      class = "plugin_class_unknown"
    )
  }

  cls[["constructor"]]()
}

# The version of the plugin class that the id `id` names: what the
# method of `plugin_version()` for the class returns, called on the plugin
# that the class's constructor makes.
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
      "{pkg_id_label(plugin_class_id(x))} returns no whole number of 1 or ",
      "more.",
      class = "plugin_version_invalid"
    )
  }

  version
}

# The requests that the class of the plugin `x` gives, through its method of
# `plugin_requests()`, a list of functions by name.
checked_plugin_requests <- function(x) {

  reqs <- plugin_requests(x)

  if (!is.list(reqs) || is.object(reqs) || !all(lgl_ply(reqs, is.function)) ||
        !has_unique_names(reqs)) {
    blockr_abort(
      "The method of `plugin_requests()` for plugin class ",
      "{pkg_id_label(plugin_class_id(x))} returns no list of functions by ",
      "name.",
      class = "plugin_requests_invalid"
    )
  }

  reqs
}

# The IDs of the board options that the class of the plugin `x` names through
# its method of `plugin_options()`, by their constructors.
checked_plugin_options <- function(x) {
  option_ctor_ids(
    as_option_ctors(
      plugin_options(x),
      paste0(
        "The method of `plugin_options()` for plugin class ",
        "{pkg_id_label(plugin_class_id(x))} returns no board option ",
        "constructor, nor a list of them."
      ),
      "plugin_options_invalid"
    )
  )
}

# The app's plugins, which run on every board that the app opens, from the
# rows that `serve()` takes in `plugins`, as a list in the order of the rows:
# each with the package and the id of its class, the class's name and version,
# the IDs of the board options that its plugin reads, its module and its
# requests, which R answers the plugin's requests with.
# R gets the version, the board options and the requests from the class's
# methods of `plugin_version()`, `plugin_options()` and `plugin_requests()`,
# called on the plugin that the class's constructor makes, and stops the app
# where the version is no whole number from 1, the board options are given by
# no constructors or the requests are no list of functions by name.
# R reads the rows and refuses them where they name a class that no available
# package lists, or a class twice. It tells a frontend and a storage plugin by
# the R class of the plugin that the class's constructor makes.
# Core's basic frontend runs only where no other row is a frontend, and core's
# storage plugin only where no other row is a storage plugin, and R refuses
# rows that then hold no frontend, two frontends or two storage plugins.
app_plugins <- function(rows, available = available_plugin_classes()) {

  rows <- plugin_rows(rows, available)
  ids <- unname(Map(pkg_id, rows[["package"]], rows[["id"]]))

  twice <- unique(chr_ply(ids[duplicated(ids)], pkg_id_label))

  if (length(twice)) {
    blockr_abort(
      "The app runs one plugin of each class, while its rows name the ",
      "{qty(twice)}plugin class{?es} {twice} more than once.",
      class = "app_plugins_invalid"
    )
  }

  classes <- lapply(ids, plugin_class_entry, available)
  plugins <- lapply(classes, function(cls) cls[["constructor"]]())

  roles <- list(
    list(
      test = is_frontend_plugin,
      core = pkg_id("blockr.core", "basic_frontend_plugin"),
      what = "frontend"
    ),
    list(
      test = is_storage_plugin,
      core = pkg_id("blockr.core", "file_storage_plugin"),
      what = "storage plugin"
    )
  )

  keep <- rep(TRUE, length(ids))
  held <- list()

  for (role in roles) {

    of_role <- which(keep & lgl_ply(plugins, role$test))
    others <- of_role[!lgl_ply(ids[of_role], same_pkg_id, role$core)]

    if (length(others)) {
      keep[setdiff(of_role, others)] <- FALSE
      of_role <- others
    }

    held[[role$what]] <- of_role
  }

  if (!length(held[["frontend"]])) {
    blockr_abort(
      "The app runs no frontend, since its rows name none, where an app runs ",
      "one, such as core's basic frontend, basic_frontend_plugin of ",
      "blockr.core.",
      class = "app_plugins_invalid"
    )
  }

  for (role in roles) {

    of_role <- chr_ply(ids[held[[role$what]]], pkg_id_label)

    if (length(of_role) > 1L) {
      blockr_abort(
        "The app runs one {role$what} at most, while its rows name the ",
        "{role$what}s {of_role}.",
        class = "app_plugins_invalid"
      )
    }
  }

  unname(
    Map(
      function(cls, plugin) {
        list(
          package = cls[["package"]],
          id = cls[["id"]],
          name = cls[["name"]],
          version = checked_plugin_version(plugin),
          options = checked_plugin_options(plugin),
          module = cls[["module"]],
          requests = checked_plugin_requests(plugin)
        )
      },
      classes[keep],
      plugins[keep]
    )
  )
}

# The plugin classes that an app runs, as `serve()` takes them in `plugins`:
# the rows of a data frame, with the class's package in `package` and its id
# in `id`, in their order (see `check_class_rows()`).
# Each names a class that an available package lists, and a row that names
# none stops the app.
plugin_rows <- function(rows, available) {

  rows <- check_class_rows(
    rows,
    "plugins that an app runs",
    "available_plugins()",
    "app_plugins_invalid"
  )

  check_plugin_packages(rows[["package"]], names(available))

  unknown <- is.na(match_rows(rows, flat_by_package(available)))
  rows_unknown <- pkg_row_labels(rows)[unknown]

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

  missing <- setdiff(packages, listed)

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
  c(
    paste0("<", class(x)[1L], "> ", pkg_id_label(plugin_class_id(x))),
    format_state(plugin_state(x))
  )
}

#' @export
print.plugin <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
str_value.plugin <- function(x, ...) {
  paste0("<", class(x)[1L], "> ", pkg_id_label(plugin_class_id(x)))
}

#' @export
str.plugin <- function(object, ...) {
  cat_str_value(object, ...)
}
