#' Block class requests
#'
#' A request is a function that a block class's UI calls through its host for
#' whatever it needs from R, such as the maximum of a numeric input, which
#' never enters the block's state. A class names its requests in its
#' `requests` field (see [new_block_class()]), each a function or an object
#' made by `new_request()`, which says what else R and the client need to know
#' about it, and `block_request()` calls one with its arguments bound by name:
#' an input to the result of the upstream block that feeds it, a variadic
#' block's `...` to its inputs in link order, named by their links' inputs, a
#' state arg to the value `args` holds for it, which is left missing where
#' `args` holds none, and any other argument to a parameter that `args` holds,
#' where its default applies otherwise.
#'
#' A request reads board options only where `new_request()` names them, by
#' their constructors. Given one constructor, the request takes the board
#' option's value in its argument `options`, and given a named list of them,
#' it takes each value in the argument of that name. A value comes whole,
#' whatever its shape, a single value, several, `NULL` or a list. The client
#' sends exactly the values of the board options a request reads, by id, and
#' keeps its answers by them, so that a board option it does not read changes
#' none of them, and an argument whose board option the client sends no value
#' for is left missing.
#'
#' A request's answer is kept by the client, but for a `volatile` request,
#' which goes to R every time and suits a cheap lookup outside the board, such
#' as a directory listing, and for an `action`, which acts rather than reads,
#' such as an upload, which receives the file the user picked in its argument
#' `file`. R runs an action once per call, and it may write.
#'
#' @param x A registry id or a block class
#' @param name The name of the request
#' @param inputs Upstream results by data input
#' @param args Values of the state args and parameters by name
#' @param options For `block_request()`, the values of the board options the
#'   request reads, by id, and for `new_request()`, the constructor of the
#'   board option it reads, or a list of constructors named by the arguments
#'   that take their values
#' @param fun The request's function
#' @param volatile,action Whether the request is volatile or an action
#'
#' @examples
#' new_dir_option <- function(value = tempdir(), ...) {
#'   new_board_option(id = "dir", default = value, ...)
#' }
#'
#' dir_block <- new_block_class(
#'   "dir_block",
#'   expr = function(path = character()) bbquote(readLines(.(path))),
#'   ui = js_module("dir-block.js"),
#'   requests = list(
#'     files = new_request(
#'       function(options, pattern = "") list.files(options, pattern),
#'       options = new_dir_option,
#'       volatile = TRUE
#'     ),
#'     preview = new_request(
#'       function(path, dir, n = 3L) readLines(file.path(dir, path), n),
#'       options = list(dir = new_dir_option)
#'     )
#'   )
#' )
#'
#' dir_block$requests$preview
#'
#' pkg <- system.file(package = "blockr.core")
#'
#' block_request(dir_block, "files", options = list(dir = pkg))
#' block_request(dir_block, "preview", args = list(path = "DESCRIPTION"),
#'               options = list(dir = pkg))
#'
#' @return The answer of the request for `block_request()`, while
#' `new_request()` returns an object of class `blockr_request`.
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

  req <- cls[["requests"]][[name]]

  bound <- if (is_string(x)) {
    get_registry_entry(x)[["requests"]][[name]][["options"]]
  } else {
    request_option_ids(req)
  }

  do.call(
    req[["fun"]],
    request_args(req[["fun"]], bound, inputs, args, options),
    quote = TRUE
  )
}

#' @rdname block_request
#' @export
new_request <- function(fun, options = NULL, volatile = FALSE,
                        action = FALSE) {

  if (!is.function(fun)) {
    blockr_abort(
      "A request's `fun` is expected to be a function.",
      class = "block_request_invalid"
    )
  }

  if (is.function(options)) {
    options <- list(options = options)
  }

  if (!is.null(options) && !is_option_ctors(options)) {
    blockr_abort(
      "A request's `options` are expected to be a board option constructor ",
      "or a list of them named by the arguments that take their values.",
      class = "block_request_invalid"
    )
  }

  absent <- setdiff(names(options), formal_names(fun))

  if (length(absent)) {
    blockr_abort(
      "A request reads board options into {absent}, which its function does ",
      "not take.",
      class = "block_request_invalid"
    )
  }

  if (!is_bool(volatile) || !is_bool(action)) {
    blockr_abort(
      "A request's `volatile` and `action` are expected to be `TRUE` or ",
      "`FALSE`.",
      class = "block_request_invalid"
    )
  }

  structure(
    list(fun = fun, options = options, volatile = volatile, action = action),
    class = "blockr_request"
  )
}

#' @export
format.blockr_request <- function(x, ...) {

  args <- formal_names(x[["fun"]])
  read <- args %in% request_option_args(x)
  kind <- c("volatile", "action")[c(x[["volatile"]], x[["action"]])]

  c(
    paste(c("<blockr_request>", kind), collapse = " "),
    if (length(args)) {
      paste0(
        "Arguments: ",
        paste_enum(
          paste0("\"", args, "\"", ifelse(read, " (board option)", "")),
          quotes = ""
        )
      )
    }
  )
}

#' @export
print.blockr_request <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

is_request <- function(x) {
  inherits(x, "blockr_request")
}

as_request <- function(x) {
  if (is.function(x)) new_request(x) else x
}

is_option_ctors <- function(x) {
  is.list(x) && all(lgl_ply(x, is.function)) &&
    length(names(x)) == length(x) && all(nzchar(names(x))) &&
    !anyDuplicated(names(x))
}

request_option_args <- function(x) {
  coal(names(x[["options"]]), character(), fail_all = FALSE)
}

request_option_ids <- function(x) {
  chr_ply(call_option_ctors(x[["options"]]), board_option_id, use_names = TRUE)
}

request_args <- function(fun, bound, inputs, args, options) {

  fmls <- formal_names(fun)
  unknown <- setdiff(names(args), setdiff(fmls, names(bound)))

  if (length(unknown)) {
    blockr_abort(
      "The request takes no {qty(unknown)}argument{?s} {unknown} from `args`.",
      class = "block_request_args_invalid"
    )
  }

  unread <- setdiff(names(options), bound)

  if (length(unread)) {
    blockr_abort(
      "The request reads no {qty(unread)}board option{?s} {unread}.",
      class = "block_request_args_invalid"
    )
  }

  given <- c(inputs[setdiff(names(inputs), "...")], args)
  res <- given[intersect(fmls, names(given))]

  for (arg in names(bound)[bound %in% names(options)]) {
    res[arg] <- list(options[[bound[[arg]]]])
  }

  if ("..." %in% fmls) {
    res <- c(res, inputs[["..."]])
  }

  res
}

request_file_arg <- function(x) {
  if (x[["action"]]) "file" else character()
}

request_entry <- function(x, cls) {

  fmls <- formal_names(x[["fun"]])

  list(
    inputs = intersect(fmls, class_inputs(cls)),
    state_args = intersect(fmls, class_state_args(cls)),
    options = request_option_ids(x),
    volatile = x[["volatile"]],
    action = x[["action"]]
  )
}
