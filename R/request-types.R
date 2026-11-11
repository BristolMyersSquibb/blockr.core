#' Request types
#'
#' A request asks R for an answer that the client needs,
#' rather than for a block's result.
#' Every request names its type in `type`, one of core's five,
#' and R reads it into an object of class `{type}_request`,
#' which holds the request's properties.
#' By the request's type, R then finds the function that answers the request,
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
#' * The type `plugin`, a plugin's request,
#'   which a plugin sends through `CoreApi.request()` in the client.
#'   R finds it among the requests that `plugin_requests()` returns for the
#'   plugin class whose ID `plugin` names, as `blockr.md-md_plugin`.
#'   R calls the method on an object of class `c("{name}_plugin", "plugin")`,
#'   which it makes from the ID, as `c("md_plugin", "plugin")`.
#'
#' No package adds a type of its own,
#' and R replies `error` to a request of a type that is not core's.
#' R checks the results that a request reads,
#' and replies `missing` where it lacks one.
#' R calls the request's function, and replies with its answer,
#' or with an `error` that names the phase `request` where the function fails.
#' A function may answer with a file that [new_reply_body()] makes,
#' and reports how far it has come with [report_progress()].
#'
#' The package that ships a board option gives the board option's requests with
#' a method of `board_option_requests()` for the board option's class.
#' The package of a plugin class gives the class's requests with a method of
#' `plugin_requests()` for the class.
#' Each method returns the requests as a list of functions by name.
#' R calls a board option's request, or a plugin's, with the arguments that the
#' client passes, by name, and with the request's body in its argument `file`,
#' where the function takes one.
#' That is a list of the path of the temporary file that R wrote the bytes to,
#' in `datapath`, and the file's `name`, `type` and `size`.
#' R passes a plugin's request the blocks that it reads as well,
#' in its argument `reads`, where the function takes one.
#' That is a list by block ID, each a list of the block's `result` and the block
#' expression `expr` that R built for it.
#' A board option's request and a plugin's may write,
#' so the client keeps none of their answers.
#' R runs a plugin's request on a locked board as on any other.
#'
#' @param x A board option, for `board_option_requests()`, and for
#'   `plugin_requests()`, a plugin, which R makes from the ID of the plugin's
#'   class as an object of class `c("{name}_plugin", "plugin")`
#' @param ... Generic consistency
#'
#' @examples
#' # A package that ships the board option data_dir gives its requests.
#' board_option_requests.data_dir_option <- function(x, ...) {
#'   list(list_dir = function(path = ".") list.files(path))
#' }
#'
#' names(board_option_requests(new_board_option("data_dir")))
#'
#' # blockr.code, whose plugin class is blockr.code-code_plugin, gives the
#' # request script, for the code of the blocks that the request reads.
#' plugin_requests.code_plugin <- function(x, ...) {
#'   list(
#'     script = function(reads) {
#'       code <- vapply(reads, function(r) deparse1(r$expr), character(1L))
#'       paste(names(reads), "<-", code)
#'     }
#'   )
#' }
#'
#' plugin <- structure(list(), class = c("code_plugin", "plugin"))
#' names(plugin_requests(plugin))
#'
#' @return The generics `board_option_requests()` and `plugin_requests()`
#' return the requests of a board option and of a plugin class, each a list of
#' functions by name.
#'
#' @name request-types
NULL

# The function that answers a request, which R reads into an object of class
# `{type}_request`, and the arguments to call it with: a list of the function,
# in `fun`, its arguments by name, in `args`, and optionally `volatile`, which
# marks the reply volatile where it is `TRUE`.
# The results that the request reads come in `reads`, by block ID, each a list
# of the block's `result`, its block expression `expr` and, where R made an
# output from the result, the class vector of the latest output in `output`.
# A request of a block class reads the class from the app's block registry,
# which R hands it in `registry`.
# Core keeps the generic to itself, since no package adds a type.
# R reads no request of another type, so the generic needs no default method.
request_call <- function(x, reads, ...) {
  UseMethod("request_call")
}

#' @export
request_call.block_class_request <- function(x, reads, registry, ...) {

  entry <- get_registry_entry(x[["registry_id"]], registry)
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
    x[["options"]],
    registry
  )
}

#' @export
request_call.output_class_request <- function(x, reads, ...) {
  output_read_call(x, reads, NULL)
}

#' @export
request_call.renderer_request <- function(x, reads, ...) {
  output_read_call(x, reads, x[["renderer"]])
}

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

# A plugin's request, which R finds by the ID of the plugin's class among the
# requests of the class.
# Its function gets the blocks that the request reads in `reads`, each with its
# result and its block expression alone, and the request's body in `file`.
#' @export
request_call.plugin_request <- function(x, reads, ...) {

  plugin <- x[["plugin"]]
  name <- x[["name"]]
  fun <- plugin_class_requests(plugin)[[name]]

  if (!is.function(fun)) {
    blockr_abort(
      "Plugin class {plugin} has no request {name}.",
      class = "plugin_request_unknown"
    )
  }

  list(
    fun = fun,
    args = request_call_args(
      fun,
      name,
      x[["args"]],
      c(
        list(reads = lapply(reads, `[`, c("result", "expr"))),
        Filter(not_null, list(file = x[["body"]]))
      ),
      c("reads", "file"),
      "plugin_request_args_invalid"
    )
  )
}

#' @rdname request-types
#' @export
board_option_requests <- function(x, ...) {
  UseMethod("board_option_requests")
}

#' @rdname request-types
#' @export
board_option_requests.board_option <- function(x, ...) {
  list()
}

#' @rdname request-types
#' @export
plugin_requests <- function(x, ...) {
  UseMethod("plugin_requests")
}

#' @rdname request-types
#' @export
plugin_requests.plugin <- function(x, ...) {
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
