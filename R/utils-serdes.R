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
#' Those are the packages of the board's classes, each the package whose
#' namespace holds the class's method of [board_front_end()], blockr.core's for
#' `board` among them, and the packages of the blocks' classes.
#' The board holds its `id` and `name`, and its blocks, links and stacks, each
#' by ID, in their order.
#' In `versions`, it holds the version of each class in its class vector that a
#' loaded package names a front-end for, by class (see [board_version()]), and
#' each block holds the version of its class's state in `version` (see
#' [new_block_class()]), so that the client migrates by the versions of the
#' shapes that a JSON board holds.
#' It holds each board option that the script gives a value, by ID, as its
#' `value` with the `version` of the value's shape (see
#' [board_option_version()]), and no other.
#'
#' Its `class` is the board's class vector, most specific class first, as
#' `c("dock_board", "board")`.
#' The client keeps it and saves it unchanged.
#' The properties that the board's class adds sit beside core's, each under its
#' name.
#' They are the components that the board holds besides its blocks, links,
#' stacks and board options, such as those that a board class's constructor
#' passes to [new_board()].
#' Each is written by `blockr_ser()`, so that a package writes the objects of
#' its own classes there through a method.
#' R checks no property and picks no front-end, since board classes are defined
#' in the client.
#' The client drops each property that the class it builds the board as does
#' not add.
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
#' The parts of the board that a package defines, such as the properties of its
#' board classes or the values of its board options, are written by the schema
#' that the package puts next to its startup module, as core puts the schema of
#' its board options next to its runtime (see [register_startup_module()]).
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

  board <- c(
    list(
      id = board_id(x),
      class = class(x),
      versions = board_versions(class(x)),
      name = board_name(x),
      blocks = blockr_ser(board_blocks(x)),
      links = blockr_ser(board_links(x)),
      stacks = blockr_ser(board_stacks(x)),
      options = blockr_ser(board_options(x))
    ),
    board_properties(x)
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
  c(
    list(name = stack_name(x), blocks = stack_blocks(x)),
    if (not_null(stack_color(x))) list(color = stack_color(x))
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
blockr_ser.default <- function(x, ...) {
  x
}

# A collection as a JSON board holds it, an object of its entries by ID.
ser_collection <- function(x) {
  as_json_object(lapply(x, blockr_ser))
}

board_components <- function() {
  c("blocks", "links", "stacks", "options")
}

# The packages that a board's content comes from, with their versions, as a
# JSON board records them: the package of each class in the board's class
# vector that names a front-end, blockr.core's for `board` among them, and the
# package of each block's class.
board_packages <- function(board) {

  classes <- lapply(board[["class"]], class_package)

  blocks <- lapply(
    board[["blocks"]],
    function(blk) get_registry_entry(blk[["registry_id"]])[["package"]]
  )

  package_versions(unlst(c(classes, blocks)))
}

# The properties that the board's class adds: the components that a board holds
# besides core's, by name, each written by `blockr_ser()`.
board_properties <- function(x) {

  keys <- setdiff(names(x), board_components())

  if (!all(nzchar(keys))) {
    blockr_abort(
      "The components that a board holds besides its blocks, links, stacks ",
      "and board options are the properties of its class, by name, so each ",
      "needs a name.",
      class = "board_properties_invalid"
    )
  }

  set_names(
    lapply(
      keys,
      function(key) {
        plain_value(
          blockr_ser(x[[key]]),
          paste0("The board's property ", key)
        )
      }
    ),
    keys
  )
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

# Shapes a JSON board by the schemas that give its values' shapes: the parts
# of the board that packages define, such as the properties of their board
# classes and the values of their board options, by the schemas next to their
# startup modules, which are laid out like the board, core's objects by the
# schema of a JSON board, once each board option's value is recorded with its
# version, and each block's state by the schema that its class's UI ships.
shape_json_board <- function(x) {

  for (schema in startup_schemas()) {
    x[["board"]] <- shape_by_schema(x[["board"]], schema)
  }

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
