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
#' Those are the packages of the plugins' classes, each the package that the
#' ID of the class names, and the packages of the blocks' classes.
#' The board holds its `id` and `name`, and its blocks, links, stacks and
#' plugins, each by ID, in their order.
#' In `versions`, it holds the version of each plugin class whose plugins it
#' holds, by the class's ID (see [plugin_version()]), and each block holds the
#' version of its class's state in `version` (see [new_block_class()]), so that
#' the client migrates by the versions of the shapes that a JSON board holds.
#' It holds each board option that the script gives a value, by ID, as its
#' `value` with the `version` of the value's shape (see
#' [board_option_version()]), and no other.
#'
#' A plugin is written by the ID of its class, with its name and its state
#' (see [new_plugin()]).
#' R checks no plugin's state, since the plugin's class is defined in the
#' client, which checks the state against the class's schema as it opens the
#' board.
#'
#' A stack is written with its name, its blocks and its color, which is `null`
#' for a stack without one.
#'
#' A block is written by the registry ID of its class, with its state and its
#' name.
#' Where the block registry holds no entry for its class, writing the block
#' registers the class, as a class from outside a package, under its name.
#' So a class defined in an app script is registered before any session
#' evaluates a block of it.
#'
#' Core's objects are written by the JSON Schema of a JSON board, which core
#' generates from its TypeScript types, so that a stack's blocks stay an array
#' and an empty collection an object.
#' A block's state is written by the schema that its class's UI ships (see
#' [register_block()]), and bare where it ships none.
#' The state of each plugin and the value of each board option are written by
#' the schema that a package puts next to its startup module in a file of its
#' own, named by the class in R of the plugin or of the board option, as
#' `dag_plugin.schema.json` or `n_rows_option.schema.json`, and core next to
#' its runtime for its own board options (see [register_startup_module()]).
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
#' unregister_blocks("identity_block")
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

  plugins <- board_plugins(x)

  board <- list(
    id = board_id(x),
    versions = plugin_versions_of(plugins),
    name = board_name(x),
    blocks = blockr_ser(board_blocks(x)),
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
    )
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

  entry <- class_registry_entry(block_class(x))

  list(
    registry_id = entry[["id"]],
    version = entry[["block_class"]][["version"]],
    state = plain_value(
      block_state(x),
      paste0("The state of block \"", block_name(x), "\"")
    ),
    name = block_name(x)
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
  list(
    class = plugin_class_id(x),
    name = plugin_name(x),
    state = as_json_object(
      plain_value(
        plugin_state(x),
        paste0("The state of plugin \"", plugin_name(x), "\"")
      )
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
# JSON board records them: the package of each plugin's class, which the ID of
# the class names, and the package of each block's class.
board_packages <- function(board) {

  plugins <- lapply(
    board[["plugins"]],
    function(plg) id_package(plg[["class"]])
  )

  blocks <- lapply(
    board[["blocks"]],
    function(blk) get_registry_entry(blk[["registry_id"]])[["package"]]
  )

  package_versions(unlst(c(plugins, blocks)))
}

# The version of each class of the plugins, by the class's ID, in the order in
# which the plugins first name it, as a JSON board records them.
plugin_versions_of <- function(plugins) {

  ids <- unique(chr_ply(plugins, plugin_class_id))

  as_json_object(set_names(lapply(ids, plugin_class_version), ids))
}

# The block registry's entry for a block class, which registers the class
# where no entry holds it, as a class from outside a package, under its name.
class_registry_entry <- function(cls) {

  id <- registry_id_from_block(cls)

  if (length(id)) {
    return(get_registry_entry(id[1L]))
  }

  register_block(block_constructor(cls), default_block_name(cls[["class"]]), "")
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

# What keeps a value, or any value that it holds, from being plain, as an error
# names it, or `NULL` for a value that is plain all through, `NULL` included.
nested_unplain_kind <- function(x) {

  if (is.null(x)) {
    return(NULL)
  }

  kind <- unplain_kind(x)

  if (not_null(kind) || !is.list(x)) {
    return(kind)
  }

  for (el in x) {

    kind <- nested_unplain_kind(el)

    if (not_null(kind)) {
      return(kind)
    }
  }

  NULL
}

# Shapes a JSON board by the schemas that give its values' shapes: the state of
# each plugin and the value of each board option by the schemas next to the
# startup modules of packages, each in a file named by the class in R of the
# plugin or of the board option, core's objects by the schema of a JSON board,
# once each board option's value is recorded with its version, and each block's
# state by the schema that its class's UI ships.
shape_json_board <- function(x) {

  x[["board"]][["options"]] <- shape_option_values(x[["board"]][["options"]])

  x[["board"]][["plugins"]] <- as_json_object(
    lapply(x[["board"]][["plugins"]], shape_plugin_state)
  )

  x[["board"]][["options"]] <- versioned_options(x[["board"]][["options"]])

  x <- shape_by_schema(x, json_board_schema())

  x[["board"]][["blocks"]] <- as_json_object(
    lapply(x[["board"]][["blocks"]], shape_block_state)
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

shape_block_state <- function(x) {
  entry <- get_registry_entry(x[["registry_id"]])
  x["state"] <- list(shape_by_schema(x[["state"]], entry[["schema"]]))
  x
}

# The values of board options by ID, each shaped by the schema of its board
# option's value, if any.
shape_option_values <- function(x) {

  for (id in names(x)) {

    schema <- board_option_schema(id)

    if (not_null(schema)) {
      x[id] <- list(shape_by_schema(x[[id]], schema))
    }
  }

  x
}

# A plugin as a JSON board holds it, with its state shaped by the schema of its
# class's state, if any.
shape_plugin_state <- function(x) {

  schema <- plugin_class_schema(x[["class"]])

  if (not_null(schema)) {
    x["state"] <- list(shape_by_schema(x[["state"]], schema))
  }

  x
}
