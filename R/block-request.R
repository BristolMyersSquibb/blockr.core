#' Block class requests
#'
#' A request is a function that a block class's UI calls through its host for
#' whatever it needs from R, such as the maximum of a numeric input, which
#' never enters the block's state. A class names its requests in its
#' `requests` field (see [new_block_class()]), and `block_request()` calls one
#' with its arguments bound by name: an input to the result of the upstream
#' block that feeds it, a variadic block's `...` to its inputs in link order,
#' named by their links' inputs, a state arg to the value `args` holds for it,
#' which is left missing where `args` holds none, and any other argument to a
#' parameter that `args` holds, where its default applies otherwise.
#'
#' A request reads board options only where `reads_options()` marks it with
#' the constructors of the options it reads. It then takes their values, by
#' board option id, in its argument `options`, and the client sends exactly
#' these values and keeps the request's answers by them, so that a board
#' option the request does not read changes none of them.
#'
#' A request's answer is kept by the client, but for a request marked by
#' `volatile()`, which goes to R every time and suits a cheap lookup outside
#' the board, such as a directory listing, and for one marked by `action()`,
#' which acts rather than reads, such as an upload, which receives the file the
#' user picked in its argument `file`. R runs an action once per call, and it
#' may write. Each of the three sets an attribute of the function it is given,
#' so they combine in any order.
#'
#' @param x A registry id or a block class
#' @param name The name of the request
#' @param inputs Upstream results by data input
#' @param args Values of the state args and parameters by name
#' @param options Values of the board options the request reads, by id
#' @param fun A request function
#' @param ... Constructors of the board options a request reads
#'
#' @examples
#' new_dir_option <- function(value = tempdir(), ...) {
#'   new_board_option(id = "dir", default = value, ...)
#' }
#'
#' dir_block <- new_block_class(
#'   "dir_block",
#'   expr = function(file = character()) bbquote(readLines(.(file))),
#'   ui = js_module("dir-block.js"),
#'   requests = list(
#'     files = volatile(
#'       reads_options(
#'         function(options, pattern = "") list.files(options$dir, pattern),
#'         new_dir_option
#'       )
#'     )
#'   )
#' )
#'
#' block_request(dir_block, "files", options = list(dir = R.home()))
#'
#' @return The answer of the request for `block_request()`, while
#' `volatile()`, `action()` and `reads_options()` return the function they are
#' given, marked by an attribute.
#'
#' @export
block_request <- function(x, name, inputs = list(), args = list(),
                          options = list()) {

  cls <- as_block_class(x)

  if (!is_string(name) || is.null(cls[["requests"]][[name]])) {
    blockr_abort(
      "Block class {cls[['class']][1L]} has no request {name}.",
      class = "block_request_unknown"
    )
  }

  fun <- cls[["requests"]][[name]]

  do.call(
    fun,
    request_args(fun, inputs, args, options),
    quote = TRUE
  )
}

request_args <- function(fun, inputs, args, options) {

  fmls <- formal_names(fun)
  unknown <- setdiff(names(args), setdiff(fmls, "options"))

  if (length(unknown)) {
    blockr_abort(
      "The request takes no {qty(unknown)}argument{?s} {unknown}.",
      class = "block_request_args_invalid"
    )
  }

  if (length(options) && !"options" %in% fmls) {
    blockr_abort(
      "The request reads no board options.",
      class = "block_request_args_invalid"
    )
  }

  given <- c(inputs[setdiff(names(inputs), "...")], args)
  res <- given[intersect(fmls, names(given))]

  if ("options" %in% fmls) {
    res <- c(res, list(options = options))
  }

  if ("..." %in% fmls) {
    res <- c(res, inputs[["..."]])
  }

  res
}

request_file_arg <- function(fun) {
  if (is_action(fun)) "file" else character()
}

request_entry <- function(fun, cls) {

  fmls <- formal_names(fun)

  list(
    inputs = intersect(fmls, class_inputs(cls)),
    state_args = intersect(fmls, class_state_args(cls)),
    options = chr_ply(
      call_option_ctors(request_option_ctors(fun)),
      board_option_id
    ),
    volatile = is_volatile(fun),
    action = is_action(fun)
  )
}

#' @rdname block_request
#' @export
volatile <- function(fun) {
  mark_request(fun, "volatile", TRUE)
}

#' @rdname block_request
#' @export
action <- function(fun) {
  mark_request(fun, "action", TRUE)
}

#' @rdname block_request
#' @export
reads_options <- function(fun, ...) {

  ctors <- list(...)

  if (!all(lgl_ply(ctors, is.function))) {
    blockr_abort(
      "The board options a request reads are given by their constructors.",
      class = "block_request_invalid"
    )
  }

  mark_request(fun, "reads_options", c(request_option_ctors(fun), ctors))
}

mark_request <- function(fun, mark, value) {

  if (!is.function(fun)) {
    blockr_abort(
      "A request is expected to be a function.",
      class = "block_request_invalid"
    )
  }

  attr(fun, mark) <- value

  fun
}

is_volatile <- function(fun) {
  isTRUE(attr(fun, "volatile", exact = TRUE))
}

is_action <- function(fun) {
  isTRUE(attr(fun, "action", exact = TRUE))
}

request_option_ctors <- function(fun) {
  attr(fun, "reads_options", exact = TRUE)
}
