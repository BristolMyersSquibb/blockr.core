#' Outputs
#'
#' A block's result can be any R object. An output is what R shows of it: the
#' coercion that a block class names in its `output` field makes an output
#' from the result (see [new_block_class()]). R sends the output to the client
#' as JSON, and a renderer draws it there. So nothing in the protocol assumes a
#' table.
#'
#' An output is an S3 object of an output class, whose class vector ends in
#' `block_output`. The package that defines an output class gives it two
#' functions. A constructor makes an output of the class and checks its
#' fields. A coercion is a generic that makes an output from a result, by the
#' result's class. Core defines three output classes, for tables, text and
#' plots (see [new_table_output()], [new_text_output()] and
#' [new_plot_output()]). Their constructors build on `new_block_output()`.
#' Another package extends an output class with a subclass. Its outputs hold
#' every field of the class and add fields of their own.
#'
#' An output may hold one binary body, such as a plot's PNG. R sends the body
#' beside the JSON, not in it. The function `new_reply_body()` makes a body
#' from a raw vector or from a file's path. It takes the body's MIME type and,
#' for a download, a file name.
#'
#' The package that defines an output class gives R what it needs of the
#' class, its JSON Schema, its requests and its renderer, through its methods of
#' three generics (see [output_schema()]).
#'
#' @param x For `new_block_output()`, the output's fields, as a named list, and
#'   for `new_reply_body()`, a raw vector or the path of a file
#' @param class The output's class vector, without `block_output`
#' @param type The body's MIME type
#' @param name The file name that a download gets
#'
#' @examples
#' out <- new_block_output(list(text = "Hello"), "greeting_output")
#' class(out)
#'
#' new_reply_body(charToRaw("a,b\n1,2\n"), "text/csv", name = "data.csv")
#'
#' @return The constructor `new_block_output()` returns an object of class
#' `block_output` and `new_reply_body()` one of class `reply_body`.
#'
#' @export
new_block_output <- function(x, class) {

  if (!is.list(x) || is.object(x) || !has_unique_names(x)) {
    blockr_abort(
      "The fields of an output are expected to be a list with unique names.",
      class = "block_output_invalid"
    )
  }

  if ("renderer" %in% names(x)) {
    blockr_abort(
      "An output cannot hold a field `renderer`, which R adds when it sends ",
      "the output.",
      class = "block_output_invalid"
    )
  }

  if (sum(lgl_ply(x, is_reply_body)) > 1L) {
    blockr_abort(
      "An output may hold one reply body at most.",
      class = "block_output_invalid"
    )
  }

  if (!is.character(class) || !length(class) || anyNA(class) ||
        !all(nzchar(class)) || "block_output" %in% class) {
    blockr_abort(
      "The class vector of an output is expected to name its classes, ",
      "without `block_output`, which is added.",
      class = "block_output_invalid"
    )
  }

  structure(x, class = c(class, "block_output"))
}

has_unique_names <- function(x) {

  if (!length(x)) {
    return(TRUE)
  }

  nms <- names(x)

  !is.null(nms) && !anyNA(nms) && all(nzchar(nms)) && !anyDuplicated(nms)
}

is_block_output <- function(x) {
  inherits(x, "block_output")
}

#' @export
format.block_output <- function(x, ...) {

  fields <- unclass(x)
  body <- lgl_ply(fields, is_reply_body)

  c(
    paste0("<", class(x)[1L], ">"),
    if (any(!body)) {
      paste0("Fields: ", paste_enum(names(fields)[!body], quotes = "\""))
    },
    if (any(body)) {
      sub("^<reply_body> ", "Body: ", format(fields[body][[1L]]))
    }
  )
}

#' @export
print.block_output <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @rdname new_block_output
#' @export
new_reply_body <- function(x, type, name = NULL) {

  if (is_string(x) && file.exists(x)) {
    x <- normalizePath(x)
  } else if (!is.raw(x)) {
    blockr_abort(
      "A reply body is made from a raw vector or the path of a file.",
      class = "reply_body_invalid"
    )
  }

  if (!is_string(type) || !nzchar(type)) {
    blockr_abort(
      "The type of a reply body is expected to be a MIME type, as a string.",
      class = "reply_body_invalid"
    )
  }

  if (!is.null(name) && !is_string(name)) {
    blockr_abort(
      "The name of a reply body is expected to be a string or `NULL`.",
      class = "reply_body_invalid"
    )
  }

  structure(
    list(content = x, type = type, name = name),
    class = "reply_body"
  )
}

is_reply_body <- function(x) {
  inherits(x, "reply_body")
}

reply_body_bytes <- function(x) {

  content <- x[["content"]]

  if (is.raw(content)) {
    return(content)
  }

  readBin(content, "raw", file.size(content))
}

# What a reply or request says of the body it carries, in its field `body`: the
# MIME type and, for a download, the file name.
body_announcement <- function(x) {

  res <- list(type = x[["type"]])

  if (not_null(x[["name"]])) {
    res[["name"]] <- x[["name"]]
  }

  res
}

#' @export
format.reply_body <- function(x, ...) {

  content <- x[["content"]]
  size <- if (is.raw(content)) length(content) else file.size(content)

  paste0(
    "<reply_body> ",
    paste(
      c(
        x[["type"]],
        if (not_null(x[["name"]])) paste0("\"", x[["name"]], "\""),
        format(
          structure(size, class = "object_size"),
          units = "auto",
          standard = "SI"
        )
      ),
      collapse = ", "
    )
  )
}

#' @export
print.reply_body <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' Output classes in R
#'
#' The package that defines an output class gives R what it needs of the class
#' through its methods of three generics, which it registers as it registers
#' any S3 method, so that S3 dispatch gives a subclass what its class gives,
#' unless it gives its own.
#' A method answers by the class alone, whatever the output holds, since R also
#' calls it on an empty output of the class, as it resolves a request of an
#' output that it made before.
#'
#' The method of `output_schema()` returns the JSON Schema of the class's
#' outputs, by which R writes each output, as it writes a block's state by the
#' schema that the block class's UI ships, so that a vector of length one stays
#' an array where the schema has an array, and a data frame becomes an array of
#' its rows.
#' The function `read_json_schema()` reads one from a file that the package
#' ships, relative to its `inst/`, as core reads those that its build
#' generates into `inst/outputs`.
#' R sends no output whose classes give no schema.
#'
#' The method of `output_requests()` returns the class's requests, a named list
#' of functions that its renderers call through their host, for what the output
#' does not hold, such as a download of what they show.
#' A subclass that adds requests puts them before those of its class, which
#' `NextMethod()` returns, so that a request of its own takes the place of one
#' of the same name.
#' R calls a request's function with these arguments, each where the function
#' takes it:
#'
#' * The result that the output was made from, in `result`.
#' * The board option values that the output was asked with, in `options`.
#' * The parameters that the output was asked with, in `params`.
#' * A file that the renderer sends, such as the bitmap of a chart, in `file`.
#' * Any other argument, from the `args` that the renderer passes.
#'
#' A request may answer with a reply body, such as a file to download.
#' The client keeps a request's answers, as it keeps the output, unless the
#' class wraps the request's function in `volatile()`.
#' No request of an output class writes.
#' The function `run_output_class_request()` runs a request as R does, to test
#' an output class, or the requests of a renderer, without Shiny.
#'
#' The method of `output_renderer()` gives the renderer that draws the class's
#' outputs (see [new_renderer()]).
#'
#' @param x An output, and for `run_output_class_request()`, an output or its
#'   class vector
#' @param ... Generic consistency
#' @param fun The function of a request
#' @param path The path of a JSON Schema's file, relative to the package's
#'   `inst/`, or as given outside a package
#' @param package The package that ships the file (`NULL` outside a package)
#' @param name The name of the request
#' @param result The result that the output was made from
#' @param args Further arguments by name, which the renderer passes
#' @param options The board option values that the output was asked with, by
#'   board option id
#' @param params The parameters that the output was asked with
#' @param file A file that the renderer sends, or `NULL`
#' @param renderer The ID of the renderer whose own request it is, or `NULL`
#'   for a request of the output class
#'
#' @examples
#' output_schema.words_output <- function(x, ...) {
#'   list(type = "object", properties = list(words = list(type = "array")))
#' }
#'
#' output_requests.words_output <- function(x, ...) {
#'   list(count = volatile(function(result) length(result)))
#' }
#'
#' words <- new_block_output(list(words = "hello"), "words_output")
#'
#' output_schema(words)
#' run_output_class_request(words, "count", result = c("hello", "world"))
#'
#' @return The generic `output_schema()` returns a JSON Schema, as a list,
#' `output_requests()` a named list of functions, `volatile()` the function it
#' is given, marked as volatile, `read_json_schema()` the JSON Schema that it
#' reads, as a list, and `run_output_class_request()` the request's answer.
#'
#' @export
output_schema <- function(x, ...) {
  UseMethod("output_schema")
}

#' @rdname output_schema
#' @export
output_schema.block_output <- function(x, ...) {
  blockr_abort(
    "Output class {class(x)[1L]} gives no JSON Schema of its outputs, which ",
    "its package gives through a method of `output_schema()`.",
    class = "output_schema_missing"
  )
}

#' @rdname output_schema
#' @export
output_requests <- function(x, ...) {
  UseMethod("output_requests")
}

#' @rdname output_schema
#' @export
output_requests.block_output <- function(x, ...) {
  list()
}

#' @rdname output_schema
#' @export
volatile <- function(fun) {

  if (!is.function(fun)) {
    blockr_abort(
      "A volatile request is a function, which `volatile()` marks.",
      class = "output_class_requests_invalid"
    )
  }

  attr(fun, "volatile") <- TRUE

  fun
}

is_volatile <- function(fun) {
  isTRUE(attr(fun, "volatile", exact = TRUE))
}

#' @rdname output_schema
#' @export
read_json_schema <- function(path, package = pkg_name(parent.frame())) {

  if (!is_string(path)) {
    blockr_abort(
      "The path of a JSON Schema's file is expected to be a string.",
      class = "json_schema_file_invalid"
    )
  }

  file <- if (is.null(package)) path else system.file(path, package = package)

  if (!nzchar(file) || !file.exists(file)) {
    blockr_abort(
      "Found no JSON Schema file {path}",
      if (is.null(package)) "." else " in package {package}.",
      class = "json_schema_file_invalid"
    )
  }

  decode_schema(file)
}

# A request of an output: where the request names a renderer, one of the
# requests of the renderer that draws the output, which has to be the one
# named, and otherwise one of the output class's, as its method of
# `output_requests()` gives them, a subclass's before its class's.
output_class_request <- function(class, name, renderer = NULL) {

  out <- structure(list(), class = unique(c(class, "block_output")))

  if (not_null(renderer)) {
    return(renderer_request(renderer, out, name))
  }

  reqs <- output_requests(out)

  if (!is.list(reqs) || is.object(reqs) || !all(lgl_ply(reqs, is.function)) ||
        (length(reqs) && (is.null(names(reqs)) || !all(nzchar(names(reqs)))))) {
    blockr_abort(
      "The method of `output_requests()` for output class {class[1L]} ",
      "returns no named list of functions.",
      class = "output_class_requests_invalid"
    )
  }

  fun <- reqs[[name]]

  if (!is.function(fun)) {
    blockr_abort(
      "Output class {class[1L]} has no request {name}.",
      class = "output_class_request_unknown"
    )
  }

  list(fun = fun, volatile = is_volatile(fun))
}

#' @rdname output_schema
#' @export
run_output_class_request <- function(x, name, result, args = list(),
                                     options = list(), params = list(),
                                     file = NULL, renderer = NULL) {

  call <- output_request_call(x, name, result, args, options, params, file,
                              renderer)

  do.call(call[["fun"]], call[["args"]], quote = TRUE)
}

# The function of a request of an output, or of a renderer,
# with the arguments that R binds for it and whether the request is volatile,
# so that R can resolve a request apart from running it.
output_request_call <- function(x, name, result, args, options, params, file,
                                renderer) {

  if (!is_string(name)) {
    blockr_abort(
      "The name of a request is expected to be a string.",
      class = "output_class_request_unknown"
    )
  }

  if (not_null(renderer) && !is_string(renderer)) {
    blockr_abort(
      "The renderer that asks is expected to be named by its ID, a string.",
      class = "output_class_request_unknown"
    )
  }

  class <- if (is.character(x)) x else class(x)
  req <- output_class_request(class, name, renderer)

  if (!has_unique_names(args)) {
    blockr_abort(
      "The `args` of a request are expected to be named.",
      class = "output_class_request_args_invalid"
    )
  }

  bound <- list(result = result, options = options, params = params,
                file = file)

  if (is.null(file)) {
    bound[["file"]] <- NULL
  }

  list(
    fun = req[["fun"]],
    args = request_call_args(
      req[["fun"]],
      name,
      args,
      bound,
      c("result", "options", "params", "file"),
      "output_class_request_args_invalid"
    ),
    volatile = isTRUE(req[["volatile"]])
  )
}

# The arguments that R calls a request's function with:
# those that the client passes in `args`, by name,
# and the values that R binds itself, in `bound`,
# where the function takes them.
# The client passes none of the arguments that `reserved` names,
# which R binds where it has a value for them.
request_call_args <- function(fun, name, args, bound, reserved, class) {

  fmls <- formal_names(fun)
  unknown <- setdiff(names(args), setdiff(fmls, c(reserved, "...")))

  if ("..." %in% fmls) {
    unknown <- intersect(names(args), reserved)
  }

  if (length(unknown)) {
    blockr_abort(
      "Request {name} takes no {qty(unknown)}argument{?s} {unknown} from ",
      "`args`.",
      class = class
    )
  }

  c(bound[intersect(names(bound), fmls)], args)
}

# The parts of an output that R sends. Its fields are shaped by the JSON Schema
# that its classes give, through their methods of `output_schema()`. Its body,
# if it holds one, travels beside them.
shape_output <- function(x) {

  if (!is_block_output(x)) {
    blockr_abort(
      "An output is expected to inherit from `block_output`.",
      class = "block_output_invalid"
    )
  }

  fields <- unclass(x)
  body <- lgl_ply(fields, is_reply_body)

  list(
    output = shape_by_schema(fields[!body], output_schema(x)),
    body = if (any(body)) fields[body][[1L]]
  )
}
