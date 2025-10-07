#' Serialization utilities
#'
#' Blocks are serialized by writing out information on the constructor used to
#' create the object, combining this with block state information, which
#' constitutes values such that when passed to the constructor the original
#' object can be re-created.
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
#'
#' all.equal(blk, blockr_deser(blockr_ser(blk)), check.environment = FALSE)
#'
#' @return Serialization helper function `blockr_ser()` returns lists, which
#' for most objects contain slots `object` and `payload`, where `object`
#' contains a class vector which is used by `blockr_deser()` to instantiate an
#' empty object of that class and use S3 dispatch to identify the correct method
#' that, given the content in `payload`, can re-create the original object.
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

#' @param options Board option values (`NULL` uses values provided by `x`)
#' @rdname blockr_ser
#' @export
blockr_ser.board_options <- function(x, options = NULL, ...) {

  if (is.null(options)) {
    options <- board_option_values(x)
  }

  expected <- names(x)

  stopifnot(is.list(options), setequal(expected, names(options)))

  list(
    object = class(x),
    payload = map(blockr_ser, x, options[expected])
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.blockr_ctor <- function(x, ...) {

  fun <- attr(x, "fun")
  pkg <- attr(x, "pkg")

  if (is.null(fun)) {
    fun <- serialize(x, NULL)
    ver <- NULL
  } else {
    ver <- as.character(pkg_version(pkg))
  }

  list(
    object = class(x),
    constructor = fun,
    package = pkg,
    version = ver
  )
}

#' @param option Board option value (`NULL` uses values provided by `x`)
#' @rdname blockr_ser
#' @export
blockr_ser.board_option <- function(x, option = NULL, ...) {
  list(
    object = class(x),
    payload = coal(option, board_option_value(x), list()),
    constructor = blockr_ser(attr(x, "ctor"))
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.llm_model_option <- function(x, option = NULL, ...) {
  NextMethod(option = attr(option, "chat_name"))
}

#' @rdname blockr_ser
#' @export
blockr_ser.board <- function(x, blocks = NULL, options = NULL, ...) {
  list(
    object = class(x),
    blocks = blockr_ser(board_blocks(x), blocks),
    links = blockr_ser(board_links(x)),
    stacks = blockr_ser(board_stacks(x)),
    options = blockr_ser(as_board_options(x), options),
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
    new_blockr_ctor(unserialize(jsonlite::base64_dec(ctr)))
  } else {
    new_blockr_ctor(NULL, ctr, pkg)
  }
}

#' @rdname blockr_ser
#' @export
blockr_deser.board_option <- function(x, data, ...) {

  stopifnot(
    all(c("constructor", "payload") %in% names(data))
  )

  payload <- data[["payload"]]
  ctor <- blockr_deser(data[["constructor"]])

  if (is.atomic(payload)) {
    payload <- list(payload)
  }

  args <- c(
    payload,
    list(
      ctor = coal(attr(ctor, "fun"), ctor),
      pkg = attr(ctor, "pkg")
    )
  )

  do.call(ctor, args)
}
