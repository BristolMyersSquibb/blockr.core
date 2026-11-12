#' Serialization
#'
#' R writes the board that an app script builds as a JSON board once: the
#' opening board, which it sends the client.
#' A JSON board is the format in which the client saves every board, so R
#' writes the opening board as the client would save it.
#' Both write each value of a board by the same JSON Schemas, so that no
#' shape depends on R's JSON writer. So `blockr_ser()` returns the board as R
#' values that yyjsonr, jsonlite and Shiny's encoder all write as the same
#' JSON. A value that a schema has as an array is marked with [I()], so that a
#' vector of length one stays an array, and an object is a named list, an empty
#' one included. Nothing in R reads a board back, since the client opens every
#' board itself.
#'
#' @section Writing a board:
#' A JSON board holds the board under `board`, with two things beside it:
#' `format`, the version of the format, and `packages`, the version of each
#' package that the board's content comes from, by package.
#' Those are the packages that the IDs of the plugins' classes and of the
#' blocks' classes name, each the part of the ID before its first `-`.
#' The ID of a block class that an app script defines names no package.
#' The board holds its `id` and `name`, its blocks, links and stacks, each by
#' ID, in their order, and the state of its plugins, by the IDs of their
#' classes.
#' In `versions`, it holds the version of each plugin class whose plugin's
#' state it holds, by the class's ID (see [plugin_version()]), and each block
#' holds the version of its class's state in `version` (see
#' [new_block_class()]), so that the client migrates by the versions of the
#' shapes that a JSON board holds.
#' It holds each board option that the script gives a value, by ID, as its
#' `value` with the `version` of the value's shape (see
#' [board_option_version()]), and no other.
#'
#' A plugin is written as its state alone, under the ID of its class (see
#' [new_plugin()]), and a plugin whose constructor gives it no state is not
#' written at all, so that no board names a plugin that keeps nothing, such as
#' core's basic frontend.
#' R checks no plugin's state, since the plugin's class is defined in the
#' client, which checks the state against the class's schema as it opens the
#' board.
#'
#' A stack is written with its name, its blocks and its color, which is `null`
#' for a stack without one.
#'
#' A block is written by the registry ID that its class gives (see
#' [register_block()]), with its state, its name and its lock, `locked`, which
#' a block whose lock is `NA` holds as `null`.
#' R reads the ID, the version of the block's state and the schema of the
#' state from the block's class.
#' It reads nothing from the block registry, so writing a board registers no
#' class (see [serve()]).
#'
#' Core's objects are written by the JSON Schema of a JSON board, which core
#' generates from its TypeScript types, so that a stack's blocks stay an array
#' and an empty collection an object.
#' A block's state is written by the schema that its class's UI ships (see
#' [register_block()]), and bare where it ships none.
#' The state of each plugin and the value of each board option are written by
#' the schema that the package that lists the plugin's class or the board
#' option ships beside the module that it lists for it, as `dag.schema.json`
#' beside `dag.js` (see [read_blocks_yml()]).
#' Each is written bare where no package gives one.
#'
#' A board that the format cannot hold fails as it is written, and so as the
#' script runs. Every value has to be `NULL`, a vector of logicals, numbers or
#' strings, or a list of such values, with no attribute but names. So a
#' function, an environment, a call or a value of a class, such as a factor or
#' a data frame, is an error. A named vector is written as an object.
#'
#' @param x Object to serialize
#' @param ... Generic consistency
#'
#' @examples
#' new_identity_block <- block_constructor(
#'   new_block_class(
#'     "identity_block",
#'     inputs = "data",
#'     expr = function(note = "") bbquote(.(data)),
#'     ui = js_module("identity-block.js")
#'   )
#' )
#'
#' board <- new_board(
#'   blocks = c(a = new_identity_block(), b = new_identity_block("Second")),
#'   links = links(from = "a", to = "b"),
#'   stacks = stacks(s = new_stack("a", "Start")),
#'   options = new_show_conditions_option("error"),
#'   name = "My board"
#' )
#'
#' str(blockr_ser(board))
#'
#' @return For a board, `blockr_ser()` returns the board as a JSON board, as a
#' list that R's JSON writers write as its JSON.
#' Its methods for the parts of a board return what the board holds in their
#' place, before the schemas shape it.
#'
#' @export
blockr_ser <- function(x, ...) {
  UseMethod("blockr_ser")
}

#' @rdname blockr_ser
#' @export
blockr_ser.board <- function(x, ...) {

  plugins <- Filter(function(p) length(plugin_state(p)) > 0L, board_plugins(x))
  blocks <- board_blocks(x)

  board <- list(
    id = board_id(x),
    versions = plugin_versions_of(plugins),
    name = board_name(x),
    blocks = blockr_ser(blocks),
    links = blockr_ser(board_links(x)),
    stacks = blockr_ser(board_stacks(x)),
    options = blockr_ser(board_options(x)),
    plugins = ser_collection(plugins)
  )

  shape_json_board(
    list(
      format = board_json_format(),
      packages = board_packages(board),
      board = board
    ),
    lapply(as.list(blocks), block_class)
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.blocks <- function(x, ...) {
  ser_collection(as.list(x))
}

#' @rdname blockr_ser
#' @export
blockr_ser.block <- function(x, ...) {

  cls <- block_class(x)

  list(
    registry_id = class_registry_id(cls),
    version = cls[["version"]],
    state = plain_value(
      block_state(x),
      paste0("The state of block \"", block_name(x), "\"")
    ),
    name = block_name(x),
    locked = block_locked(x)
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.links <- function(x, ...) {
  ser_collection(as.list(x))
}

#' @rdname blockr_ser
#' @export
blockr_ser.link <- function(x, ...) {
  list(from = x[["from"]], to = x[["to"]], input = x[["input"]])
}

#' @rdname blockr_ser
#' @export
blockr_ser.stacks <- function(x, ...) {
  ser_collection(as.list(x))
}

#' @rdname blockr_ser
#' @export
blockr_ser.stack <- function(x, ...) {
  list(
    name = stack_name(x),
    blocks = stack_blocks(x),
    color = coal(stack_color(x), NA_character_)
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.board_options <- function(x, ...) {

  opts <- as.list(x)
  opts <- opts[lgl_ply(opts, has_board_option_value)]

  set_names(lapply(opts, blockr_ser), names(opts))
}

#' @rdname blockr_ser
#' @export
blockr_ser.board_option <- function(x, ...) {
  plain_value(
    board_option_value(x),
    paste0("The value of board option ", board_option_id(x))
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.plugin <- function(x, ...) {
  as_json_object(
    plain_value(
      plugin_state(x),
      paste0("The state of the plugin of class ", plugin_class_id(x))
    )
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.default <- function(x, ...) {
  x
}

# A collection as a JSON board holds it, an object of its entries by ID.
ser_collection <- function(x) {
  as_json_object(lapply(x, blockr_ser))
}

# The packages that a board's content comes from, with their versions, as a
# JSON board records them: the packages that the IDs of the classes of the
# plugins whose state it holds and of the blocks' classes name. The ID of a
# block class that an app script defines names none.
board_packages <- function(board) {

  packages <- id_package(
    c(
      names(board[["plugins"]]),
      chr_xtr(board[["blocks"]], "registry_id")
    )
  )

  package_versions(packages[!is.na(packages)])
}

# The version of the class of each plugin, by the class's ID, in the order of
# the plugins, as a JSON board records them: what the class's method of
# `plugin_version()` gives for the plugin.
plugin_versions_of <- function(plugins) {
  as_json_object(lapply(plugins, checked_plugin_version))
}

# A value as a board's JSON holds it: `NULL`, a vector of logicals, numbers or
# strings, or a list of such values, with no attribute but names, and a named
# vector as a named list, so that it is written as an object. A value that a
# schema marked as an array with `I()` stays marked.
plain_value <- function(x, what) {

  if (is.null(x)) {
    return(NULL)
  }

  kind <- unplain_kind(x)

  if (not_null(kind)) {
    blockr_abort(
      "{what} holds {kind}, which the JSON of a board cannot hold.",
      class = "board_value_invalid"
    )
  }

  marked <- inherits(x, "AsIs")

  if (is.list(x)) {
    res <- lapply(x, plain_value, what)
    return(if (marked) I(res) else res)
  }

  if (!is.null(names(x)) && !marked) {
    return(as.list(x))
  }

  x
}

# What keeps a value from being plain, as an error names it, or `NULL` for a
# plain value.
unplain_kind <- function(x) {

  cls <- setdiff(oldClass(x), "AsIs")
  type <- typeof(x)

  if (length(cls)) {
    return(paste("a value of class", cls[1L]))
  }

  if (type %in% c("closure", "builtin", "special")) {
    return("a function")
  }

  if (!type %in% c("logical", "integer", "double", "character", "list")) {
    return(paste("a value of type", type))
  }

  extra <- setdiff(names(attributes(x)), c("names", "class"))

  if (length(extra)) {
    return(paste("a value with the attribute", extra[1L]))
  }

  NULL
}

# Shapes a JSON board by the schemas that give its values' shapes: the state of
# each plugin and the value of each board option by the schemas beside the
# modules that packages list for the plugin's class and the board option, core's
# objects by the schema of a JSON board, once each board option's value is
# recorded with its version, and each block's state by the schema that its
# class's UI ships. The block classes are given in `classes`, by block ID.
# R reads the listings of the available packages only for a board that holds a
# plugin or a board option.
shape_json_board <- function(x, classes) {

  options <- x[["board"]][["options"]]

  if (length(options)) {
    x[["board"]][["options"]] <- shape_option_values(
      options,
      available_options()
    )
  }

  plugins <- x[["board"]][["plugins"]]

  if (length(plugins)) {
    plugins <- Map(
      shape_plugin_state,
      plugins,
      names(plugins),
      list(available_plugin_classes())
    )
  }

  x[["board"]][["plugins"]] <- as_json_object(plugins)

  x[["board"]][["options"]] <- versioned_options(x[["board"]][["options"]])

  x <- shape_by_schema(x, json_board_schema())

  blocks <- x[["board"]][["blocks"]]

  x[["board"]][["blocks"]] <- as_json_object(
    Map(shape_block_state, blocks, classes[names(blocks)])
  )

  x
}

# The values of board options, by ID, as a JSON board holds them, each with
# the version of its shape.
versioned_options <- function(x) {
  as_json_object(
    Map(
      function(value, id) list(value = value, version = option_version(id)),
      x,
      names(x)
    )
  )
}

# The JSON Schema of a JSON board, which the build generates from `JsonBoard`
# in srcts/types/board.ts.
json_board_schema <- function() {
  decode_schema(
    system.file("board", "json-board.schema.json", package = "blockr.core")
  )
}

shape_block_state <- function(x, cls) {
  x["state"] <- list(shape_by_schema(x[["state"]], class_schema(cls)))
  x
}

# The values of board options by ID, each shaped by the schema of its board
# option's value, if any, among the board options listed in `options`.
shape_option_values <- function(x, options = available_options()) {

  for (id in names(x)) {

    schema <- board_option_schema(id, options)

    if (not_null(schema)) {
      x[id] <- list(shape_by_schema(x[[id]], schema))
    }
  }

  x
}

# A plugin's state as a JSON board holds it, under the ID of the plugin's
# class, `id`, shaped by the schema of the class's state, if any, among the
# plugin classes listed in `classes`.
shape_plugin_state <- function(x, id, classes = available_plugin_classes()) {

  schema <- plugin_class_schema(id, classes)

  if (is.null(schema)) {
    return(x)
  }

  shape_by_schema(x, schema)
}
