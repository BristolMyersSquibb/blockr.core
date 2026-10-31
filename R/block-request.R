#' Block class requests
#'
#' A request is a function that a block class's UI calls through its host for
#' whatever it needs from R, such as the maximum of a numeric input, which
#' never enters the block's state. A class names its requests in its
#' `requests` field (see [new_block_class()]), and `block_request()` calls one
#' with its arguments bound by name: an input to the result of the upstream
#' block that feeds it, a variadic block's `...` to its inputs in link order,
#' named by their links' inputs, a state arg to the value `args` holds for it,
#' which is left missing where `args` holds none, a board option the class
#' names to the value `options` holds for it, and any other argument to a
#' parameter that `args` holds, where its default applies otherwise.
#'
#' A request's answer is kept by the client, but for a request wrapped in
#' `volatile()`, which goes to R every time and suits a cheap lookup outside
#' the board, such as a directory listing, and for one wrapped in `action()`,
#' which acts rather than reads, such as an upload, which receives the file the
#' user picked in its argument `file`. R runs an action once per call, and it
#' may write.
#'
#' @param x A registry id or a block class
#' @param name The name of the request
#' @param inputs Upstream results by data input
#' @param args Values of the state args and parameters by name
#' @param options Values of board options by id
#' @param fun A request function
#'
#' @examples
#' head_block <- new_block_class(
#'   "head_block",
#'   inputs = "data",
#'   expr = function(n = 6L) bbquote(utils::head(.(data), n = .(n))),
#'   ui = js_module("head-block.js"),
#'   requests = list(n_max = function(data) nrow(data))
#' )
#'
#' block_request(head_block, "n_max", inputs = list(data = datasets::iris))
#'
#' @return The answer of the request for `block_request()`, while `volatile()`
#' and `action()` return the function they wrap, with the class `volatile` or
#' `action` added.
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
  unknown <- setdiff(names(c(args, options)), fmls)

  if (length(unknown)) {
    blockr_abort(
      "The request takes no {qty(unknown)}argument{?s} {unknown}.",
      class = "block_request_args_invalid"
    )
  }

  given <- c(inputs[setdiff(names(inputs), "...")], args, options)
  res <- given[intersect(fmls, names(given))]

  if ("..." %in% fmls) {
    res <- c(res, inputs[["..."]])
  }

  res
}

request_file_arg <- function(fun) {
  if (inherits(fun, "action")) "file" else character()
}

request_entry <- function(fun, cls) {

  fmls <- formal_names(fun)

  list(
    inputs = intersect(fmls, class_inputs(cls)),
    state_args = intersect(fmls, class_state_args(cls)),
    options = intersect(fmls, class_option_ids(cls)),
    volatile = inherits(fun, "volatile"),
    action = inherits(fun, "action")
  )
}

#' @rdname block_request
#' @export
volatile <- function(fun) {
  wrap_request(fun, "volatile")
}

#' @rdname block_request
#' @export
action <- function(fun) {
  wrap_request(fun, "action")
}

wrap_request <- function(fun, class) {

  if (!is.function(fun)) {
    blockr_abort(
      "A request is expected to be a function.",
      class = "block_request_invalid"
    )
  }

  class(fun) <- unique(c(class, class(fun)))

  fun
}
