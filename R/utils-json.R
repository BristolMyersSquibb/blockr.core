#' Serialization utilities
#'
#' Object serialization is available via `to_json()`, while de-serialization
#' is available as `from_json()`. Blocks are serialized by writing out
#' information on the constructor used to create the object, combining this
#' with block state information, which constitutes values such that when passed
#' to the constructor the original object can be re-created.
#'
#' Helper functions `blockr_ser()` and `blockr_deser()` are implemented as
#' generics and perform most of the heavy lifting for (de-)serialization:
#' representing objects as easy-to-serialize (nested) lists containing mostly
#' strings and no objects which are hard/impossible to truthfully re-create in
#' new sessions (such as environments).
#'
#' @param x Object to (de)serialize
#' @param ... Generic consistency
#'
#' @examples
#' blk <- new_dataset_block("iris")
#'
#' blockr_ser(blk)
#' to_json(blk)
#'
#' all.equal(blk, blockr_deser(blockr_ser(blk)), check.environment = FALSE)
#' all.equal(blk, from_json(to_json(blk)), check.environment = FALSE)
#'
#' @return Serialization helper function `blockr_ser()` returns lists, which
#' for most objects contain slots `object` and `payload`, where `object`
#' contains a class vector which is used by `blockr_deser()` to instantiate an
#' empty object of that class and use S3 dispatch to identify the correct method
#' that, given the content in `payload`, can re-create the original object.
#' These are wrapped by `to_json()`, which returns JSON and `from_json()` which
#' can consume JSON and returns the original object.
#'
#' @export
blockr_ser <- function(x, ...) {
  UseMethod("blockr_ser")
}

#' @param state Object state (as returned from an `expr_server`)
#' @rdname blockr_ser
#' @export
blockr_ser.block <- function(x, state = NULL, ...) {
  as.list(x, state)
}

#' @param blocks Block states (`NULL` defaults to values from ctor scope)
#' @rdname blockr_ser
#' @export
blockr_ser.blocks <- function(x, blocks = NULL, ...) {

  if (!length(blocks)) {

    res <- lapply(x, blockr_ser)

  } else {

    stopifnot(
      is.list(blocks), all(lgl_ply(blocks, is.list)),
      length(blocks) == length(x), setequal(names(blocks), names(x))
    )

    res <- Map(blockr_ser, x, blocks[names(x)])
  }

  list(
    object = class(x),
    payload = res
  )
}

#' @param options Board option values (`NULL` means default values)
#' @rdname blockr_ser
#' @export
blockr_ser.board_options <- function(x, options = NULL, ...) {

  if (is.null(options)) {
    options <- as.list(new_board_options())
  }

  expected <- list_board_options(x)

  stopifnot(is.list(options), setequal(expected, names(options)))

  board_opts <- as.list(x)

  list(
    object = class(x),
    payload = Map(
      coal,
      options[expected],
      board_opts[expected],
      MoreArgs = list(fail_null = FALSE)
    )
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.board <- function(x, blocks = NULL, options = NULL, ...) {
  list(
    object = class(x),
    blocks = blockr_ser(board_blocks(x), blocks),
    links = blockr_ser(board_links(x)),
    stacks = blockr_ser(board_stacks(x)),
    options = blockr_ser(board_options(x), options),
    version = as.character(pkg_version())
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.link <- function(x, ...) {
  list(
    object = class(x),
    payload = as.list(x)
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.links <- function(x, ...) {
  list(
    object = class(x),
    payload = lapply(x, blockr_ser)
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.stack <- function(x, ...) {
  list(
    object = class(x),
    payload = as.list(x)
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.stacks <- function(x, ...) {
  list(
    object = class(x),
    payload = lapply(x, blockr_ser)
  )
}

#' @rdname blockr_ser
#' @export
blockr_deser <- function(x, ...) {
  UseMethod("blockr_deser")
}

#' @rdname blockr_ser
#' @export
blockr_deser.list <- function(x, ...) {
  stopifnot("object" %in% names(x))
  blockr_deser(structure(list(), class = x[["object"]]), data = x)
}

#' @param data List valued data (converted from JSON)
#' @rdname blockr_ser
#' @export
blockr_deser.block <- function(x, data, ...) {
  as_block(data)
}

#' @rdname blockr_ser
#' @export
blockr_deser.blocks <- function(x, data, ...) {
  as_blocks(
    lapply(data[["payload"]], blockr_deser)
  )
}

#' @rdname blockr_ser
#' @export
blockr_deser.board <- function(x, data, ...) {
  new_board(
    blocks = blockr_deser(data[["blocks"]]),
    links = blockr_deser(data[["links"]]),
    stacks = blockr_deser(data[["stacks"]]),
    options = blockr_deser(data[["options"]]),
    class = setdiff(class(x), "board")
  )
}

#' @rdname blockr_ser
#' @export
blockr_deser.link <- function(x, data, ...) {
  as_link(data[["payload"]])
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
  as_stack(data[["payload"]])
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
  as_board_options(data[["payload"]])
}

#' @rdname blockr_ser
#' @export
to_json <- function(x, ...) {
  jsonlite::toJSON(blockr_ser(x, ...), auto_unbox = TRUE, null = "null")
}

#' @rdname blockr_ser
#' @export
from_json <- function(x) {

  if (is_string(x) && file.exists(x)) {
    x <- readLines(x)
  } else if (is.character(x) && length(x) > 1L) {
    x <- paste0(x, collapse = "")
  }

  blockr_deser(
    jsonlite::fromJSON(x, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  )
}
