#' Plugins
#'
#' A plugin extends a board as a whole, such as a frontend that draws the page,
#' the DAG or a document of `blockr.md`, and is core's one way to extend a board
#' besides block classes.
#' A board holds its plugins (see [new_board()]), each with its state, which
#' the board keeps as it keeps a block's state.
#' A package's constructor `new_{name}_plugin()` returns a plugin made by
#' `new_plugin()`, of class `c("{name}_plugin", ..., "plugin")`.
#' The constructor's args are the plugin's state, such as the positions of the
#' DAG, which it passes to `new_plugin()` as a list, and its `plugin_name`
#' gives the plugin's name, as `block_name` gives a block's.
#'
#' The id of a plugin's class is `{pkg}-{name}_plugin`, as
#' `blockr.dag-dag_plugin`, where the package is the one that the constructor
#' lives in, since that package defines the class in JavaScript, where its
#' startup module registers the class under that id (see
#' [register_startup_module()]).
#' So an app script defines no plugin class, and `new_plugin()` raises an error
#' where the constructor that calls it lives outside a package.
#'
#' Core defines two plugin classes beside `plugin`, since every page relies on
#' what each does: `frontend_plugin`, the class of a plugin that draws the page,
#' and `storage_plugin`, the class of a plugin through which every frontend
#' saves boards and opens them.
#' A package's class extends one of them or `plugin` itself, as
#' `c("session_plugin", "storage_plugin", "plugin")`, and a board holds at most
#' one frontend and one storage plugin.
#'
#' The version of a plugin class is the version of the shape of its plugins'
#' state, a whole number from 1, which the class's method of `plugin_version()`
#' gives, and which is 1 for a class without one.
#' A package raises it as it changes that shape, so that the client migrates
#' the state of a plugin that a JSON board holds at an older version.
#' R calls the method on an object of class `c("{name}_plugin", "plugin")`,
#' which it makes from the class's id, and sends the version of each class that
#' a method gives one in its reply to `hello`.
#' A plugin class that needs R gives its requests through its method of
#' [plugin_requests()], which R calls on such an object too.
#'
#' @param state The plugin's state, the args of its constructor, as a list by
#'   name
#' @param plugin_name The plugin's name, which titles its panel, made from the
#'   name of its class where it is `NULL`, as `"Dag"` for `dag_plugin`
#' @param class The plugin's class vector without `"plugin"`, its own class
#'   first, as `"dag_plugin"` or `c("dock_plugin", "frontend_plugin")`
#' @param x Plugin
#' @param ... Generic consistency
#'
#' @examples
#' \dontrun{
#' # In a package such as blockr.dag, whose plugin class is
#' # blockr.dag-dag_plugin
#' new_dag_plugin <- function(positions = list(), plugin_name = "DAG") {
#'   new_plugin(list(positions = positions), plugin_name, "dag_plugin")
#' }
#'
#' plugin_version.dag_plugin <- function(x, ...) 2L
#' }
#'
#' @return The constructor `new_plugin()` returns a `plugin` object, and
#' `is_plugin()` a scalar logical. Of the getters, `plugin_name()` returns a
#' string, `plugin_class_id()` the id of the plugin's class, a string, and
#' `plugin_state()` the state, a list by name. The generic `plugin_version()`
#' returns a whole number.
#'
#' @export
new_plugin <- function(state = list(), plugin_name = NULL,
                       class = character()) {

  package <- pkg_name(parent.frame())

  if (is.null(package)) {
    blockr_abort(
      "A plugin is made by the constructor of the package that defines its ",
      "class, while this constructor lives outside a package.",
      class = "plugin_package_missing"
    )
  }

  class <- check_plugin_class(class)

  if (!is.list(state) || is.object(state) ||
        (length(state) && !all(nzchar(coal(names(state), ""))))) {
    blockr_abort(
      "The state of a plugin of class {class[1L]} is expected to be a list ",
      "of its constructor's args by name.",
      class = "plugin_state_invalid"
    )
  }

  plugin_name <- coal(
    plugin_name,
    default_plugin_name(class[1L]),
    fail_all = FALSE
  )

  if (!is_string(plugin_name) || is.na(plugin_name)) {
    blockr_abort(
      "Expecting a string-valued plugin name.",
      class = "plugin_name_invalid"
    )
  }

  structure(
    set_names(state, coal(names(state), character(), fail_all = FALSE)),
    plugin_name = plugin_name,
    plugin_package = package,
    class = unique(c(class, "plugin"))
  )
}

# A plugin's class vector without "plugin", whose first class is the plugin's
# own, as `{name}_plugin`, which is none of core's.
check_plugin_class <- function(class) {

  own <- if (is.character(class) && length(class)) class[1L] else ""

  if (!grepl(".+_plugin$", own) || own %in% core_plugin_classes() ||
        anyNA(class)) {
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
plugin_name <- function(x) {
  check_plugin(x)
  attr(x, "plugin_name", exact = TRUE)
}

#' @rdname new_plugin
#' @export
plugin_class_id <- function(x) {
  check_plugin(x)
  paste0(attr(x, "plugin_package", exact = TRUE), "-", class(x)[1L])
}

#' @rdname new_plugin
#' @export
plugin_state <- function(x) {
  check_plugin(x)
  bare_list(x)
}

#' @rdname new_plugin
#' @export
default_plugin_name <- function(class) {
  default_name(class, "_plugin")
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

# Whether a plugin is a frontend or a storage plugin, of which a board holds
# one at most each.
is_frontend_plugin <- function(x) {
  inherits(x, "frontend_plugin")
}

is_storage_plugin <- function(x) {
  inherits(x, "storage_plugin")
}

# The id a plugin takes where the list of a board's plugins gives it no name:
# the name of its class without `_plugin`, as `dag` for `dag_plugin`.
default_plugin_id <- function(x) {
  sub("_plugin$", "", class(x)[1L])
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

# The version of the plugin class with the ID `id`, `{pkg}-{name}_plugin`: what
# the method of `plugin_version()` for `{name}_plugin` returns, called on an
# object of class `c("{name}_plugin", "plugin")`, as R calls it for a class
# that it knows by its ID alone.
plugin_class_version <- function(id) {

  version <- plugin_version(plugin_of_class(id))

  if (!is_count(version, allow_zero = FALSE)) {
    blockr_abort(
      "The method of `plugin_version()` for plugin class {id} returns no ",
      "whole number of 1 or more.",
      class = "plugin_version_invalid"
    )
  }

  version
}

# The requests of the plugin class with the ID `id`, `{pkg}-{name}_plugin`, a
# list of functions by name: what the method of `plugin_requests()` for
# `{name}_plugin` returns, called on an object of class
# `c("{name}_plugin", "plugin")`.
plugin_class_requests <- function(id) {

  reqs <- plugin_requests(plugin_of_class(id))

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

# The object on which R calls the methods of a plugin class that it knows by
# its ID alone, `{pkg}-{name}_plugin`: one of class
# `c("{name}_plugin", "plugin")`, as `c("dag_plugin", "plugin")` for
# `blockr.dag-dag_plugin`.
plugin_of_class <- function(id) {
  structure(list(), class = c(id_class(id), "plugin"))
}

# The version of each plugin class that a method of `plugin_version()` gives
# one, by the class's ID, as R sends them in its reply to `hello`. The class's
# package is the one whose namespace holds the method, and a method that an app
# script defines, from no namespace, gives no class's version.
# Where `packages` is given, a class from another package gives none either.
plugin_versions <- function(packages = NULL) {

  pkgs <- method_packages("plugin_version")
  classes <- names(pkgs)

  keep <- grepl("_plugin$", classes) &
    !classes %in% core_plugin_classes() &
    !is.na(pkgs)

  if (not_null(packages)) {
    keep <- keep & pkgs %in% packages
  }

  ids <- paste(pkgs[keep], classes[keep], sep = "-")

  as_json_object(set_names(lapply(ids, plugin_class_version), ids))
}

#' @export
format.plugin <- function(x, ...) {

  state <- plugin_state(x)

  c(
    paste0("<", class(x)[1L], "> \"", plugin_name(x), "\""),
    paste0("Class: ", plugin_class_id(x)),
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
  paste0("<", class(x)[1L], "> \"", plugin_name(x), "\"")
}

#' @export
str.plugin <- function(object, ...) {
  cat_str_value(object, ...)
}
