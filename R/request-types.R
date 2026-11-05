#' Request types
#'
#' A request asks R for an answer that the client needs,
#' rather than for a block's result.
#' Every request names its type in `type`,
#' and R reads it into an object of class `{type}_request`,
#' which holds the request's fields.
#' R answers it through the generic `request_call()`,
#' whose method for that class finds the function that answers the request,
#' and binds the function's arguments.
#' Core's types are these:
#'
#' * The type `block_class`, a request of a block class,
#'   which the block's UI sends (see [new_block_request()]).
#'   R finds it among the requests of the class that `registry_id` names.
#' * The type `output_class`, a request of an output class,
#'   which a renderer sends (see [output_requests()]).
#'   R finds it by the class vector of the latest output that it made from the
#'   result that `output` names, through the classes' methods of
#'   `output_requests()`.
#' * The type `renderer`, a renderer's own request (see [new_renderer()]).
#'   R finds it among the requests of the renderer that draws that output,
#'   which has to be the one that `renderer` names.
#' * The type `option`, a board option's request,
#'   which the board option's UI sends.
#'   R finds it among the requests that `board_option_requests()` returns for
#'   the board option that `option` names, by the board option's class,
#'   `{id}_option`.
#'
#' A package adds a type with a method of `request_call()` for its class,
#' such as the dock might for the requests of its extensions.
#' The client then sends requests of that type,
#' with the fields that the package gives them.
#' R checks the results that every request names in `reads`, whatever its
#' type, and replies `missing` where it lacks one.
#' So a method gets only results that R holds.
#' The method returns the function that answers the request,
#' with the arguments to call it with.
#' R calls the function, and replies with its answer,
#' or with an `error` that names the phase `request` where the function fails.
#' A function reports how far it has come with [report_progress()].
#' R replies `error` to a request of a type that no method answers.
#'
#' The package that ships a board option gives the board option's requests with
#' a method of `board_option_requests()` for the board option's class.
#' R calls a board option's request with the arguments that the client passes,
#' by name, and with the request's body in its argument `file`,
#' where the function takes one.
#' That is a list of the path of the temporary file that R wrote the bytes to,
#' in `datapath`, and the file's `name`, `type` and `size`.
#'
#' @param x For `request_call()`, the request, as R reads it from the client's
#'   message, and for `board_option_requests()`, a board option
#' @param reads The results that the request reads, by block ID, each a list
#'   of the block's `result`, its block expression `expr` and, where R made an
#'   output from the result, the class vector of the latest output in `output`
#' @param ... Generic consistency
#'
#' @examples
#' # A package that adds the type "code", for the script of the blocks that a
#' # request reads, puts `S3method(request_call, code_request)` in its
#' # NAMESPACE.
#' request_call.code_request <- function(x, reads, ...) {
#'   script <- function(reads) {
#'     code <- vapply(reads, function(r) deparse1(r$expr), character(1L))
#'     paste(names(reads), "<-", code)
#'   }
#'   list(fun = script, args = list(reads = reads))
#' }
#'
#' # A package that ships the board option data_dir gives its requests.
#' board_option_requests.data_dir_option <- function(x, ...) {
#'   list(list_dir = function(path = ".") list.files(path))
#' }
#'
#' names(board_option_requests(new_board_option("data_dir")))
#'
#' @return The generic `request_call()` returns a list of the function that
#' answers the request, in `fun`, the arguments to call it with, by name, in
#' `args`, and optionally `volatile`, which marks the reply volatile where it is
#' `TRUE`. The generic `board_option_requests()` returns the board option's
#' requests, a list of functions by name.
#'
#' @export
request_call <- function(x, reads, ...) {
  UseMethod("request_call")
}

#' @rdname request_call
#' @export
request_call.default <- function(x, reads, ...) {

  type <- sub("_request$", "", class(x)[1L])

  blockr_abort(
    "R answers no request of type ", type, ", since no package has a method ",
    "of `request_call()` for ", class(x)[1L], ".",
    class = "request_type_unknown"
  )
}

#' @rdname request_call
#' @export
request_call.block_class_request <- function(x, reads, ...) {

  entry <- get_registry_entry(x[["registry_id"]])
  bound <- bind_inputs(x[["reads"]], reads, entry[["inputs"]])
  args <- x[["args"]]

  # An action receives the request's body in its argument `file`.
  if (isTRUE(entry[["requests"]][[x[["name"]]]][["action"]])) {
    args[["file"]] <- x[["body"]]
  }

  block_request_call(
    x[["registry_id"]],
    x[["name"]],
    input_values(bound),
    args,
    x[["options"]]
  )
}

#' @rdname request_call
#' @export
request_call.output_class_request <- function(x, reads, ...) {
  output_read_call(x, reads, NULL)
}

#' @rdname request_call
#' @export
request_call.renderer_request <- function(x, reads, ...) {
  output_read_call(x, reads, x[["renderer"]])
}

#' @rdname request_call
#' @export
request_call.option_request <- function(x, reads, ...) {

  option <- x[["option"]]
  name <- x[["name"]]
  fun <- board_option_requests(new_board_option(option))[[name]]

  if (!is.function(fun)) {
    blockr_abort(
      "Board option {option} has no request {name}.",
      class = "option_request_unknown"
    )
  }

  list(
    fun = fun,
    args = request_call_args(
      fun,
      name,
      x[["args"]],
      Filter(not_null, list(file = x[["body"]])),
      "file",
      "option_request_args_invalid"
    )
  )
}

#' @rdname request_call
#' @export
board_option_requests <- function(x, ...) {
  UseMethod("board_option_requests")
}

#' @rdname request_call
#' @export
board_option_requests.board_option <- function(x, ...) {
  list()
}

# The call of a request of an output, of its output class or of the renderer
# given, resolved against the class vector of the latest output that R made
# from the result that the request names, and run with that result.
output_read_call <- function(x, reads, renderer) {

  out <- x[["output"]]
  held <- reads[[out[["block"]]]]

  if (is.null(held[["output"]])) {
    blockr_abort(
      "R made no output of block {out$block} at revision {out$revision},",
      " so it knows of no output class to resolve request {x$name} by.",
      class = "output_request_unresolved"
    )
  }

  output_request_call(
    held[["output"]],
    x[["name"]],
    held[["result"]],
    x[["args"]],
    x[["options"]],
    x[["params"]],
    x[["body"]],
    renderer
  )
}
