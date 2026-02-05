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

  if (is.null(state)) {
    state <- initial_block_state(x)
  }

  attrs <- attributes(x)

  state <- c(
    state,
    attrs[setdiff(names(attrs), c("names", internal_block_attributes()))]
  )

  ctor <- block_ctor(x)

  list(
    object = class(x),
    payload = state,
    constructor = blockr_ser(ctor)
  )
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

  val <- coal(option, board_option_value(x), list())

  if (is.atomic(val)) {
    val <- list(val)
  }

  ctor <- board_option_ctor(x)
  name <- setdiff(names(formals(ctor)), c("...", "category"))

  if (is.null(names(val))) {

    if (length(val) && length(name) != length(val)) {
      blockr_abort(
        "Cannot match option values to constructor arguments for option ",
        "{board_option_id(x)}.",
        class = "option_value_arg_number_mismatch"
      )
    }

    if (length(val) && length(name) > 1L) {
      log_warn(
        "Uncertainty matching option values to constructor arguments for ",
        "option {board_option_id(x)}. Consider providing a specific ",
        "`blockr_ser()` method."
      )
    }

    if (length(val)) {
      names(val) <- name
    }
  }

  unknown <- setdiff(names(val), name)

  if (length(unknown)) {
    blockr_abort(
      "Unrecognized option {qty(unknown)} argument{?s} {unknown} for option ",
      "{board_option_id(x)}.",
      class = "option_value_arg_name_mismatch"
    )
  }

  list(
    object = class(x),
    payload = c(val, list(category = board_option_category(x))),
    constructor = blockr_ser(ctor)
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.llm_model_option <- function(x, option = NULL, ...) {
  NextMethod(
    option = coal(
      attr(coal(option, board_option_value(x)), "chat_name"),
      list()
    )
  )
}

#' @param board_id Board ID
#' @rdname blockr_ser
#' @export
blockr_ser.board <- function(x, board_id = NULL, ...) {

  stopifnot(is.null(board_id) || is_string(board_id))

  list(
    object = class(x),
    payload = Map(
      blockr_ser, x, list(...)[names(x)]
    ),
    constructor = blockr_ser(board_ctor(x)),
    version = as.character(pkg_version()),
    id = board_id
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.link <- function(x, ...) {
  list(
    object = class(x),
    payload = as.list(x),
    constructor = blockr_ser(attr(x, "ctor"))
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
    payload = as.list(x),
    constructor = blockr_ser(attr(x, "ctor"))
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

  cls <- x[["object"]]

  res <- blockr_deser(
    structure(list(), class = cls),
    data = x
  )

  if (!identical(class(res), cls)) {
    blockr_abort(
      "Could not deserialize object: expected {qty(cls)} class{?es} {cls}, ",
      "but received {qty(res)} {class(res)}.",
      class = "block_deser_class_error"
    )
  }

  res
}

#' @param data List valued data (converted from JSON)
#' @rdname blockr_ser
#' @export
blockr_deser.block <- function(x, data, ...) {

  stopifnot(
    all(c("constructor", "payload") %in% names(data))
  )

  ctor <- blockr_deser(data[["constructor"]])

  payload <- data[["payload"]]

  if ("name" %in% names(payload)) {
    payload[["block_name"]] <- payload[["name"]]
    payload[["name"]] <- NULL
  }

  args <- c(
    payload,
    list(
      ctor = coal(ctor_name(ctor), ctor_fun(ctor)),
      ctor_pkg = ctor_pkg(ctor)
    )
  )

  do.call(ctor_fun(ctor), args)
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

  stopifnot(
    all(c("constructor", "payload") %in% names(data))
  )

  ctor <- blockr_deser(data[["constructor"]])

  args <- c(
    lapply(data[["payload"]], blockr_deser),
    list(
      ctor = coal(ctor_name(ctor), ctor_fun(ctor)),
      pkg = ctor_pkg(ctor)
    )
  )

  res <- do.call(ctor_fun(ctor), args)

  attr(res, "id") <- data[["id"]]

  res
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

  args <- c(
    data[["payload"]],
    list(
      ctor = coal(ctor_name(ctor), ctor_fun(ctor)),
      pkg = ctor_pkg(ctor)
    )
  )

  do.call(ctor_fun(ctor), args)
}
