#' Serialization
#'
#' R writes the board that an app script builds as JSON once: the opening
#' board, which it sends the client. The client writes every board that is
#' saved. Both write each value of a board by the same JSON Schemas, so that no
#' shape depends on R's JSON writer. So `blockr_ser()` returns the board as R
#' values that yyjsonr, jsonlite and Shiny's encoder all write as the same
#' JSON. A value that a schema has as an array is marked with [I()], so that a
#' vector of length one stays an array, and an object is a named list, an empty
#' one included. The function `blockr_deser()` reads a file of today's stack
#' (below).
#'
#' @section Writing a board:
#' The board is written as R sends it. It holds its own `id` and `name` only
#' where it has them. It holds its blocks, links and stacks as arrays, in their
#' order, each entry with its `id` where the script gave one, since R mints no
#' IDs: the client mints each missing one as it opens the board. It holds the
#' value of each board option that the script gives, by ID, and no other.
#'
#' Its `front_end` names the front-end that the board is made for, which R
#' picks by the board's class (see [board_front_end()]), and holds the
#' front-end's state. That state is made of the components that the board
#' holds besides its blocks, links, stacks and board options, such as those
#' that a front-end's constructor adds. Each is written by `blockr_ser()`, so
#' that a package writes the objects of its own classes there through a
#' method.
#'
#' A block is written by the registry ID of its class, with its state, its name
#' and core's attribute `visible`. Where the block registry holds no entry for
#' its class, writing the block registers the class, as a class from outside a
#' package, under its name. So a class defined in an app script is registered
#' before any session evaluates a block of it. A block made by [new_block()]
#' names no block class and cannot be written.
#'
#' Core's objects are written by the JSON Schema of the board that core
#' generates from its TypeScript types, so that a stack's blocks stay an array
#' and an empty collection an object. A block's state is written by the schema
#' that its class's UI ships (see [register_block()]), and bare where it ships
#' none. The parts of the board that a package defines, such as the front-end's
#' state or the values of its board options, are written by the schema that the
#' package puts next to its startup module, as core puts the schema of its board
#' options next to its runtime (see [register_startup_module()]).
#'
#' A board that the format cannot hold fails as it is written, and so as the
#' script runs. Every value has to be `NULL`, a vector of logicals, numbers or
#' strings, or a list of such values, with no attribute but names. So a
#' function, an environment, a call or a value of a class, such as a factor or
#' a data frame, is an error. A named vector is written as an object.
#'
#' @param x Object to (de)serialize
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
#' @return For a board, `blockr_ser()` returns the board as R sends it, as a
#' list that R's JSON writers write as the board's JSON. Its methods for the
#' parts of a board return what the board holds in their place, before the
#' board's schemas shape it. The function `blockr_deser()` returns the board,
#' block, link, stack or board option that it recreates from a file of today's
#' stack.
#'
#' @export
blockr_ser <- function(x, ...) {
  UseMethod("blockr_ser")
}

#' @rdname blockr_ser
#' @export
blockr_ser.board <- function(x, ...) {

  res <- c(
    if (not_null(board_id(x))) list(id = board_id(x)),
    list(
      front_end = list(
        id = board_front_end(x),
        state = front_end_state(x),
        attrs = list()
      )
    ),
    if (not_null(board_name(x))) list(name = board_name(x)),
    list(
      blocks = blockr_ser(board_blocks(x)),
      links = blockr_ser(board_links(x)),
      stacks = blockr_ser(board_stacks(x)),
      options = blockr_ser(board_options(x))
    )
  )

  shape_board(res)
}

#' @rdname blockr_ser
#' @export
blockr_ser.blocks <- function(x, ...) {
  ser_collection(as.list(x), ids_or_empty(names(x), length(x)))
}

#' @rdname blockr_ser
#' @export
blockr_ser.block <- function(x, ...) {

  if (is_legacy_block(x)) {
    blockr_abort(
      "Block \"{block_name(x)}\" of class {class(x)[1L]} is made by ",
      "`new_block()` and names no block class, so it cannot be written.",
      class = "block_ser_legacy"
    )
  }

  entry <- class_registry_entry(block_class(x))

  list(
    registry_id = entry[["id"]],
    state = plain_value(
      block_state(x),
      paste0("The state of block \"", block_name(x), "\"")
    ),
    name = block_name(x),
    attrs = block_attrs(x)
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.links <- function(x, ...) {
  lnks <- as.list(x)
  ser_collection(lnks, ids_or_empty(names(lnks), length(lnks)))
}

#' @rdname blockr_ser
#' @export
blockr_ser.link <- function(x, ...) {
  list(from = x[["from"]], to = x[["to"]], input = x[["input"]])
}

#' @rdname blockr_ser
#' @export
blockr_ser.stacks <- function(x, ...) {
  ser_collection(as.list(x), ids_or_empty(names(x), length(x)))
}

#' @rdname blockr_ser
#' @export
blockr_ser.stack <- function(x, ...) {
  list(name = stack_name(x), blocks = stack_blocks(x))
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

ser_collection <- function(x, ids) {
  map(
    function(obj, id) c(if (nzchar(id)) list(id = id), blockr_ser(obj)),
    x,
    ids
  )
}

board_components <- function() {
  c("blocks", "links", "stacks", "options")
}

# The front-end's state: the components that a board holds besides core's, by
# key, each written by `blockr_ser()`.
front_end_state <- function(x) {

  keys <- setdiff(names(x), board_components())

  if (!all(nzchar(keys))) {
    blockr_abort(
      "The components that a board holds besides its blocks, links, stacks ",
      "and board options are the front-end's state, by key, so each needs a ",
      "name.",
      class = "board_front_end_state_invalid"
    )
  }

  set_names(
    lapply(
      keys,
      function(key) {
        plain_value(
          blockr_ser(x[[key]]),
          paste0("The front-end's state ", key)
        )
      }
    ),
    keys
  )
}

# Core's attribute `visible`, which says which of a block's sections show,
# written as the array of sections that its schema in the client has.
block_attrs <- function(x) {

  visible <- attr(x, "visible", exact = TRUE)

  if (is.null(visible)) {
    return(list())
  }

  list(visible = I(unname(visible)))
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

# Shapes the board as R sends it by the schemas that give its values' shapes:
# core's objects by the board's schema, the parts that packages define, such as
# the front-end's state and the values of board options, by the schemas next to
# their startup modules, and each block's state by the schema that its class's
# UI ships.
shape_board <- function(x) {

  for (schema in c(list(board_schema()), startup_schemas())) {
    x <- shape_by_schema(x, schema)
  }

  x[["blocks"]] <- lapply(x[["blocks"]], shape_block_state)

  x
}

# The JSON Schema of the board as R sends it, which the build generates from
# `OpeningBoard` in srcts/types/board.ts.
board_schema <- function() {
  decode_schema(
    system.file("board", "opening-board.schema.json", package = "blockr.core")
  )
}

shape_block_state <- function(x) {
  entry <- get_registry_entry(x[["registry_id"]])
  x["state"] <- list(shape_by_schema(x[["state"]], entry[["schema"]]))
  x
}

#' @section Reading today's files:
#' A file of today's stack, which `blockr_ser()` wrote in blockr.core 0.1.4 and
#' before, holds every object as its class vector in `object`, its
#' constructor's args in `payload` and its constructor in `constructor`. The
#' function `blockr_deser()` recreates the objects from the R values read from
#' such a file, as the conversion of one into a saved file does. The class
#' vector selects the `blockr_deser()` method, but the recreated object only has
#' to keep the kind of object it names: the last class with a `blockr_deser()`
#' method of its own, such as `block` for any block. Classes in front of it may
#' differ from the ones written, so a package can rename or drop classes of its
#' blocks without breaking saved boards. An object recreated as another kind
#' raises an error of class `block_deser_class_error`. The function
#' `blockr_deser()` forwards `...` to the method it dispatches to. That lets
#' callers, and the methods that read nested objects, thread context down to
#' inner methods.
#'
#' A board is read with its ID and name, which the file holds next to its
#' payload, either of them `NULL` where the board has none. For a board written
#' by blockr.core 0.1.4, `blockr_deser()` reads the name off its `board_name`
#' board option instead. A board option is recreated from its value alone,
#' without the category and label saved with it, which its definition in the
#' client gives.
#'
#' @param on_error How to handle a block that cannot be deserialized -- its
#' constructor (or the package providing it) is unavailable, its payload cannot
#' be reconstructed, or the re-created object is not a block. `"abort"` (the
#' default) propagates the error, while `"drop"` omits the offending block
#' (emitting a warning) and continues; board deserialization then prunes any
#' links and stacks that reference a dropped block, so a board referencing a
#' since-retired or no-longer-installed block type still loads. Board
#' deserialization resolves `on_error` once and threads it to the contained
#' blocks. The default is sourced from `blockr_option("deser_on_error",
#' "abort")`, letting a deployment opt into dropping via
#' `options(blockr.deser_on_error = "drop")` or the `BLOCKR_DESER_ON_ERROR`
#' environment variable.
#' @rdname blockr_ser
#' @export
blockr_deser <- function(x, ..., on_error = NULL) {
  UseMethod("blockr_deser")
}

#' @rdname blockr_ser
#' @export
blockr_deser.list <- function(x, ...) {

  stopifnot("object" %in% names(x))

  cls <- x[["object"]]

  res <- blockr_deser(
    structure(list(), class = cls),
    data = x,
    ...
  )

  if (identical(class(res), cls)) {
    return(res)
  }

  kind <- deser_kind(cls)

  if (!inherits(res, kind)) {
    blockr_abort(
      "Could not deserialize object: expected an object of class {kind}, ",
      "but received {qty(class(res))}class{?es} {class(res)}.",
      class = "block_deser_class_error"
    )
  }

  res
}

# The kind a saved class vector names is its last class with a `blockr_deser()`
# method of its own, such as `block`, leaving out the `list` that vctrs-based
# objects end in. Only the kind has to survive a rebuild: the classes in front
# of it are the producing package's to rename or drop.
deser_kind <- function(cls) {

  has_method <- lgl_ply(
    cls,
    function(x) {
      x != "list" &&
        not_null(utils::getS3method("blockr_deser", x, optional = TRUE))
    }
  )

  cls <- cls[has_method]

  cls[length(cls)]
}

#' @param data List valued data (converted from JSON)
#' @rdname blockr_ser
#' @export
blockr_deser.block <- function(x, data, ...) {

  stopifnot(
    all(c("constructor", "payload") %in% names(data))
  )

  ctor <- blockr_deser(data[["constructor"]])

  args <- c(
    data[["payload"]],
    list(
      ctor = coal(ctor_name(ctor), ctor_fun(ctor)),
      ctor_pkg = ctor_pkg(ctor)
    )
  )

  do.call(ctor_fun(ctor), args)
}

#' @rdname blockr_ser
#' @export
blockr_deser.blocks <- function(x, data, ..., on_error = NULL) {

  on_error <- resolve_deser_on_error(on_error)

  payload <- data[["payload"]]

  if (identical(on_error, "abort")) {
    return(as_blocks(lapply(payload, blockr_deser)))
  }

  res <- map(deser_or_drop, payload, names(payload))
  names(res) <- names(payload)

  as_blocks(res[!lgl_ply(res, is.null)])
}

deser_or_drop <- function(x, id) {
  tryCatch(
    blockr_deser(x),
    error = function(e) {
      log_warn(
        "Dropping block ", id, " during deserialization: ",
        conditionMessage(e),
        use_glue = FALSE
      )
      NULL
    }
  )
}

resolve_deser_on_error <- function(on_error) {
  match.arg(
    coal(on_error, blockr_option("deser_on_error", "abort")),
    c("drop", "abort")
  )
}

#' @rdname blockr_ser
#' @export
blockr_deser.board <- function(x, data, ..., on_error = NULL) {

  stopifnot(
    all(c("constructor", "payload") %in% names(data))
  )

  on_error <- resolve_deser_on_error(on_error)

  ctor <- blockr_deser(data[["constructor"]])

  payload <- data[["payload"]]
  name <- data[["name"]]

  # A board written by blockr.core 0.1.4 holds its name as the value of its
  # `board_name` board option, which no longer exists
  opts <- payload[["options"]][["payload"]]
  is_name <- lgl_ply(
    opts,
    function(opt) "board_name_option" %in% opt[["object"]]
  )

  if (any(is_name)) {
    name <- coal(
      name,
      opts[[which(is_name)[1L]]][["payload"]][["value"]],
      fail_all = FALSE
    )
    payload[["options"]][["payload"]] <- opts[!is_name]
  }

  parts <- lapply(payload, blockr_deser, ..., on_error = on_error)

  if (identical(on_error, "drop")) {
    parts <- drop_block_refs(parts, payload[["blocks"]])
  }

  args <- c(parts, list(id = data[["id"]], name = name))

  do.call(ctor_fun(ctor), args)
}

drop_block_refs <- function(parts, blocks) {

  dropped <- setdiff(names(blocks[["payload"]]), names(parts[["blocks"]]))

  if (!length(dropped)) {
    return(parts)
  }

  lnk <- parts[["links"]]
  stk <- as.list(parts[["stacks"]])

  before <- length(lnk) + sum(lengths(stk))

  parts[["links"]] <- lnk[!links_incident(lnk, dropped)]

  stk <- lapply(stk, setdiff, dropped)
  stk <- stk[lengths(stk) > 0L]
  parts[["stacks"]] <- as_stacks(stk)

  if (length(parts[["links"]]) + sum(lengths(stk)) < before) {
    log_warn("Pruned links and stacks referencing dropped blocks.")
  }

  parts
}

#' @rdname blockr_ser
#' @export
blockr_deser.link <- function(x, data, ...) {

  stopifnot(
    all(c("constructor", "payload") %in% names(data))
  )

  do.call(
    blockr_deser(data[["constructor"]]),
    data[["payload"]]
  )
}

#' @rdname blockr_ser
#' @export
blockr_deser.links <- function(x, data, ...) {
  as_links(
    lapply(data[["payload"]], blockr_deser)
  )
}

#' @rdname blockr_ser
#' @export
blockr_deser.stack <- function(x, data, ...) {

  stopifnot(
    all(c("constructor", "payload") %in% names(data))
  )

  do.call(
    blockr_deser(data[["constructor"]]),
    data[["payload"]]
  )
}

#' @rdname blockr_ser
#' @export
blockr_deser.stacks <- function(x, data, ...) {
  as_stacks(
    lapply(data[["payload"]], blockr_deser)
  )
}

#' @rdname blockr_ser
#' @export
blockr_deser.board_options <- function(x, data, ...) {
  as_board_options(
    lapply(data[["payload"]], blockr_deser)
  )
}

#' @rdname blockr_ser
#' @export
blockr_deser.blockr_ctor <- function(x, data, ...) {

  stopifnot(
    all(c("constructor", "package") %in% names(data))
  )

  pkg <- data[["package"]]
  ctr <- data[["constructor"]]

  if (is.null(pkg)) {
    new_blockr_ctor(unserialize(ctr))
  } else if (pkg_avail(pkg)) {
    new_blockr_ctor(NULL, ctr, pkg)
  } else {
    blockr_abort(
      "Cannot deserialize object that depends on not installed ",
      "package {pkg}",
      class = "blockr_deser_missing_pkg"
    )
  }
}

#' @rdname blockr_ser
#' @export
blockr_deser.board_option <- function(x, data, ...) {

  stopifnot(
    all(c("constructor", "payload") %in% names(data))
  )

  ctor <- blockr_deser(data[["constructor"]])

  # The category and label that today's files hold, the board option's
  # definition in the client gives
  args <- data[["payload"]]
  args <- args[setdiff(names(args), c("category", "label"))]

  do.call(ctor_fun(ctor), args)
}
