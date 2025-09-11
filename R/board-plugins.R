#' Board plugin
#'
#' A core mechanism for extending or customizing UX aspects of the board module
#' is a "plugin" architecture. All plugins inherit from `plugin` and a sub-class
#' is assigned to each specific plugin. The "manage blocks" plugin for example
#' has a class vector `c("manage_blocks", "plugin")`. Sets of plugins are
#' handled via a wrapper class `plugins`. Each plugin needs a server component,
#' in most cases accompanied by a UI component and is optionally bundled with a
#' validator function.
#'
#' @param server,ui Server/UI for the plugin module
#' @param validator Validator function that validates server return values
#' @param class Plugin subclass
#'
#' @examples
#' plg <- board_plugins()
#'
#' is_plugins(plg)
#' names(plg)
#'
#' plg[1:3]
#'
#' is_plugin(plg[["preserve_board"]])
#'
#' @return Constructors `new_plugin()`/`plugins()` return `plugin` and
#' `plugins` objects, respectively, as do `as_plugin()`/`as_plugins()` and
#' validators `validate_plugin()`/`validate_plugins()`, which are typically
#' called for their side effects of throwing errors in case of validation
#' failure. Inheritance checkers `is_plugin()`/`is_plugins()` return scalar
#' logicals and finally, the convenience function `board_plugins()` returns a
#' `plugins` object with all known plugins (or a selected subset thereof).
#'
#' @export
new_plugin <- function(server, ui = NULL, validator = function(x, ...) x,
                       class = character()) {

  res <- structure(
    list(server = server, ui = ui),
    validator = validator,
    class = c(class, "plugin")
  )

  validate_plugin(res)
}

#' @param x Plugin object
#' @rdname new_plugin
#' @export
is_plugin <- function(x) {
  inherits(x, "plugin") &&
    sum(inherits(x, known_plugins(), which = TRUE) != 0L) == 1L
}

known_plugins <- function() {
  c("preserve_board", "manage_blocks", "manage_links", "manage_stacks",
    "notify_user", "generate_code", "edit_block", "edit_stack")
}

#' @rdname new_plugin
#' @export
as_plugin <- function(x) {
  UseMethod("as_plugin")
}

#' @export
as_plugin.plugin <- function(x) x

#' @export
as_plugin.list <- function(x) do.call(new_plugin, x)

#' @export
as_plugin.plugins <- function(x) {

  if (length(x) != 1L) {
    abort(
      "Cannot cast length != 1 \"plugins\" to \"plugin\".",
      class = "plugins_as_plugin_invalid"
    )
  }

  .subset2(x, 1L)
}

#' @export
as.list.plugin <- function(x, ...) {
  list(
    server = plugin_server(x),
    ui = plugin_ui(x),
    validator = plugin_validator(x),
    class = setdiff(class(x), "plugin")
  )
}

plugin_server <- function(x) x[["server"]]

plugin_ui <- function(x) x[["ui"]]

plugin_validator <- function(x) attr(x, "validator")

plugin_id <- function(x) class(x)[1L]

validate_plugin <- function(x) {

  if (!is_plugin(x)) {
    abort(
      "Expecting a plugin to inherit from `plugin` and any one of ",
      "{known_plugins()}.",
      class = "plugin_inheritance_invalid"
    )
  }

  if (!is.list(x)) {
    abort(
      "Expecting a plugin to behave list-like.",
      class = "plugin_type_invalid"
    )
  }

  if (!setequal(names(x), c("server", "ui"))) {
    abort(
      "Expecting a plugin to contain components \"server\" and \"ui\".",
      class = "plugin_components_invalid"
    )
  }

  srv <- plugin_server(x)

  if (!is.null(srv) && !is.function(srv)) {
    abort(
      "Expecting a plugin server to be a function.",
      class = "plugin_server_invalid"
    )
  }

  ui <- plugin_ui(x)

  if (!is.null(ui) && !is.function(ui)) {
    abort(
      "Expecting a plugin ui to be `NULL` or a function.",
      class = "plugin_ui_invalid"
    )
  }

  if (!is.function(plugin_validator(x))) {
    abort(
      "Expecting a plugin validator to be a function.",
      class = "plugin_validator_invalid"
    )
  }

  x
}

#' @export
format.plugin <- function(x, ...) {

  out <- ""

  for (cl in rev(class(x))) {
    out <- paste0("<", cl, out, ">")
  }

  out
}

#' @export
print.plugin <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

harmonize_list_of_plugins <- function(x) {
  if (is_plugin(x)) {
    list(x)
  } else if (is_plugins(x)) {
    as.list(x)
  } else if (is.list(x) && all(lgl_ply(x, is_plugin))) {
    x
  } else {
    list(as_plugin(x))
  }
}

#' @export
c.plugin <- function(...) {

  res <- unlist(
    lapply(list(...), harmonize_list_of_plugins),
    recursive = FALSE
  )

  as_plugins(res)
}

get_plugin <- function(plugin, plugins = NULL, component = NULL) {

  if (is.null(plugins)) {

    if (is.null(plugin)) {
      return(NULL)
    }

    res <- validate_plugin(plugin)

  } else {

    stopifnot(is_string(plugin), is_plugins(plugins))

    if (!plugin %in% chr_ply(plugins, plugin_id)) {
      return(NULL)
    }

    res <- plugins[[plugin]]
  }

  if (is.null(component)) {
    return(res)
  }

  switch(
    match.arg(component, c("server", "ui")),
    ui = plugin_ui(res),
    server = function(server_args, validator_args = NULL) {
      do.call(
        plugin_validator(res),
        c(
          list(do.call(plugin_server(res), server_args)),
          validator_args
        )
      )
    }
  )
}

get_plugin_ui <- function(plugin, plugins = NULL) {
  get_plugin(plugin, plugins, component = "ui")
}

get_plugin_server <- function(plugin, plugins = NULL) {
  get_plugin(plugin, plugins, component = "server")
}

call_plugin_server <- function(plugin, server_args, plugins = NULL,
                               validator_args = NULL) {

  server <- get_plugin_server(plugin, plugins)

  if (is.null(server)) {
    return(NULL)
  }

  if (is_plugin(plugin)) {
    id <- class(plugin)[1L]
  } else {
    id <- plugin
  }

  server(c(list(id), server_args), validator_args)
}

call_plugin_ui <- function(plugin, ns, ..., plugins = NULL) {

  ui <- get_plugin_ui(plugin, plugins)

  if (is.null(ui)) {
    return(tagList())
  }

  if (is_plugin(plugin)) {
    id <- class(plugin)[1L]
  } else {
    id <- plugin
  }

  ui(ns(id), ...)
}

#' @param which (Optional) character vectors of plugins to include
#' @rdname new_plugin
#' @export
board_plugins <- function(which = NULL) {

  plugins <- plugins(
    preserve_board(),
    manage_blocks(),
    manage_links(),
    manage_stacks(),
    notify_user(),
    generate_code(),
    edit_block(),
    edit_stack()
  )

  if (is.null(which)) {
    return(plugins)
  }

  plugins[which]
}

validate_callbacks <- function(x) {

  if (!is.list(x)) {
    abort(
      "Expecting a list of callbacks.",
      class = "invalid_callbacks"
    )
  }

  for (f in x) {
    if (!is.function(f)) {
      abort(
        "Expecting callbacks to be passed as functions.",
        class = "invalid_callbacks"
      )
    }
  }

  if (!is.null(names(x)) && length(unique(names(x))) != length(x)) {
    abort(
      "Callbacks must either be unnamed or named with unique names.",
      class = "invalid_callbacks"
    )
  }

  invisible(x)
}

#' @param ... Plugin objects
#' @rdname new_plugin
#' @export
plugins <- function(...) {
  validate_plugins(
    new_vctr(unname(list(...)), class = c("plugins", "list"))
  )
}

#' @rdname new_plugin
#' @export
is_plugins <- function(x) {
  inherits(x, "plugins")
}

#' @rdname new_plugin
#' @export
as_plugins <- function(x) {
  UseMethod("as_plugins")
}

#' @export
as_plugins.plugins <- function(x) {
  validate_plugins(x)
}

#' @export
as_plugins.list <- function(x) {
  do.call(plugins, x)
}

#' @export
as_plugins.plugin <- function(x) {
  as_plugins(list(x))
}

#' @export
as.list.plugins <- function(x, ...) {
  vec_data(x)
}

#' @export
anyDuplicated.plugins <- function(x, incomparables = FALSE, ...) {
  anyDuplicated(chr_ply(x, plugin_id), incomparables = incomparables, ...)
}

#' @rdname new_plugin
#' @export
validate_plugins <- function(x) {

  if (!is_plugins(x)) {
    abort(
      "Expecting a board plugins object to inherit from \"plugins\".",
      class = "plugins_inheritance_invalid"
    )
  }

  if (!is.list(x)) {
    abort(
      "Expecting a board plugins object behave list-like.",
      class = "plugins_type_invalid"
    )
  }

  if (!all(lgl_ply(x, is_plugin))) {
    abort(
      "Expecting a board plugins to contain a set of plugins.",
      class = "plugins_contains_invalid"
    )
  }

  if (anyDuplicated(x) != 0L) {
    abort(
      "Duplicate board plugins are not allowed.",
      class = "plugins_duplicate_invalid"
    )
  }

  x
}

#' @export
format.plugins <- function(x, ...) {
  c(
    paste0("<", class(x)[1L], "[", length(x), "]>"),
    chr_mply(paste0, names(x), ": ", chr_ply(x, format, ...))
  )
}

#' @export
print.plugins <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
names.plugins <- function(x) {
  chr_ply(x, plugin_id)
}

#' @export
`names<-.plugins` <- function(x, value) {

  reset <- is.null(value) ||
    identical(value, chr_ply(x, plugin_id)) ||
    all(is.na(value) | value == "")

  if (isTRUE(reset)) {
    attr(x, "names") <- NULL
    return(x)
  }

  abort("Cannot modify plugin names.", class = "plugin_names_assign_invalid")
}

#' @export
board_ui.plugin <- function(id, x, ...) {
  call_plugin_ui(x, NS(id), ...)
}

#' @export
board_ui.plugins <- function(id, x, ...) {
  do.call(
    tagList,
    lapply(x, function(y, id, ...) board_ui(id, y, ...), id, ...)
  )
}

#' @export
vec_restore.plugins <- function(x, to, ...) {
  validate_plugins(NextMethod())
}

#' @export
c.plugins <- function(...) {

  res <- unlist(
    lapply(list(...), harmonize_list_of_plugins),
    recursive = FALSE
  )

  as_plugins(res)
}

#' @export
`[.plugins` <- function(x, i, ...) {

  loc <- vec_as_location(
    i,
    length(x),
    chr_ply(x, plugin_id),
    missing = "remove"
  )

  vec_slice(x, loc)
}

#' @export
`[<-.plugins` <- function(x, i, ..., value) {
  abort("Cannot assign into `plugins`.", class = "plugins_assignment_invalid")
}

#' @export
`[[.plugins` <- function(x, i, ...) {

  loc <- vec_as_location2(
    i,
    length(x),
    chr_ply(x, plugin_id),
    missing = "propagate"
  )

  as_plugin(vec_slice(x, loc))
}

#' @export
`[[<-.plugins` <- function(x, i, ..., value) {
  abort("Cannot assign into `plugins`.", class = "plugins_assignment_invalid")
}
