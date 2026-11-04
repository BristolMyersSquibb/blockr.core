output_class_registry <- new.env()

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
#' A package registers each output class it defines with
#' `register_output_class()`, from its `.onLoad()`. Registration takes the JSON
#' Schema of the class's outputs, as a path relative to the package's `inst/`.
#' R writes each output by this schema, as it writes a block's state by the
#' schema that the block class's UI ships. So a vector of length one stays an
#' array where the schema has an array, and a data frame becomes an array of
#' its rows.
#'
#' Registration also takes the class's requests. A request is a function that
#' the class's renderers call through their host, for what the output does not
#' hold, such as a download of what they show. R resolves a request by the most
#' specific class in the output's class vector that has it. R calls the
#' request's function with these arguments, each where the function takes it:
#'
#' * The result that the output was made from, in `result`.
#' * The board option values that the output was asked with, in `options`.
#' * The parameters that the output was asked with, in `params`.
#' * A file that the renderer sends, such as the bitmap of a chart, in `file`.
#' * Any other argument, from the `args` that the renderer passes.
#'
#' A request may answer with a reply body, such as a file to download. The
#' client keeps a request's answers, as it keeps the output, unless the class
#' names the request in `volatile`. No request of an output class writes. The
#' function `run_output_class_request()` runs a request as R does, to test an
#' output class without Shiny.
#'
#' @param x For `new_block_output()`, the output's fields, as a named list, for
#'   `new_reply_body()`, a raw vector or the path of a file, and for
#'   `run_output_class_request()`, an output or its class vector
#' @param class For `new_block_output()`, the output's class vector, without
#'   `block_output`, and for `register_output_class()`, the name of the output
#'   class
#' @param type The body's MIME type
#' @param name For `new_reply_body()`, the file name that a download gets, and
#'   for `run_output_class_request()`, the name of the request
#' @param schema The JSON Schema of the class's outputs: the path of its file,
#'   relative to the package's `inst/`, or relative to the working directory
#'   outside a package
#' @param requests A named list of the class's requests, each a function
#' @param volatile The names of the requests whose answers the client never
#'   keeps
#' @param package The package that defines the output class (`NULL` outside a
#'   package)
#' @param result The result that the output was made from
#' @param args Further arguments by name, which the renderer passes
#' @param options The board option values that the output was asked with, by
#'   board option id
#' @param params The parameters that the output was asked with
#' @param file A file that the renderer sends, or `NULL`
#'
#' @examples
#' out <- new_block_output(list(text = "Hello"), "greeting_output")
#' class(out)
#'
#' new_reply_body(charToRaw("a,b\n1,2\n"), "text/csv", name = "data.csv")
#'
#' schema <- tempfile(fileext = ".json")
#' writeLines(
#'   '{"type": "object", "properties": {"words": {"type": "array"}}}',
#'   schema
#' )
#'
#' register_output_class(
#'   "words_output",
#'   schema = schema,
#'   requests = list(count = function(result, params) length(result))
#' )
#'
#' words <- new_block_output(list(words = "hello"), "words_output")
#'
#' run_output_class_request(words, "count", result = c("hello", "world"))
#'
#' @return The constructor `new_block_output()` returns an object of class
#' `block_output` and `new_reply_body()` one of class `reply_body`. The function
#' `register_output_class()` is called for its side effect and returns the
#' class's registry entry invisibly, while `run_output_class_request()` returns
#' the request's answer.
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

#' @rdname new_block_output
#' @export
register_output_class <- function(class, schema, requests = NULL,
                                  volatile = character(),
                                  package = pkg_name(parent.frame())) {

  if (!is_string(class) || !nzchar(class) || identical(class, "block_output")) {
    blockr_abort(
      "An output class is registered under its name, a string other than ",
      "`block_output`.",
      class = "output_class_invalid"
    )
  }

  requests <- coal(requests, list(), fail_all = FALSE)

  if (!is.list(requests) || !all(lgl_ply(requests, is.function)) ||
        !has_unique_names(requests)) {
    blockr_abort(
      "The requests of output class {class} are expected to be a list of ",
      "functions with unique names.",
      class = "output_class_requests_invalid"
    )
  }

  if (!is.character(volatile) || !all(volatile %in% names(requests))) {
    blockr_abort(
      "The volatile requests of output class {class} are expected to be ",
      "named among its requests.",
      class = "output_class_requests_invalid"
    )
  }

  known <- output_class_entry(class)

  if (not_null(known) && !identical(known[["package"]], package)) {

    owner <- coal(known[["package"]], "a script", fail_all = FALSE)

    blockr_abort(
      "Output class {class} is registered already, by ", owner, ".",
      class = "output_class_in_registry"
    )
  }

  entry <- structure(
    list(
      class = class,
      package = package,
      schema = read_output_schema(schema, package),
      requests = requests,
      volatile = volatile
    ),
    class = "output_class_entry"
  )

  assign(class, entry, envir = output_class_registry)

  invisible(entry)
}

read_output_schema <- function(schema, package) {

  if (is.list(schema)) {
    return(schema)
  }

  if (!is_string(schema)) {
    blockr_abort(
      "The schema of an output class is expected to be the path of its file.",
      class = "output_class_schema_invalid"
    )
  }

  path <- schema

  if (not_null(package)) {
    path <- system.file(schema, package = package)
  }

  if (!nzchar(path) || !file.exists(path)) {
    blockr_abort(
      "Found no schema file {schema} of an output class.",
      class = "output_class_schema_invalid"
    )
  }

  decode_schema(path)
}

output_class_entry <- function(class) {
  get0(class, envir = output_class_registry, inherits = FALSE)
}

unregister_output_classes <- function(class) {
  rm(list = class, envir = output_class_registry, inherits = FALSE)
  invisible()
}

#' @export
format.output_class_entry <- function(x, ...) {
  c(
    paste0("<output_class_entry> ", x[["class"]]),
    if (not_null(x[["package"]])) paste0("Package: ", x[["package"]]),
    if (length(x[["requests"]])) {
      paste0("Requests: ", paste_enum(names(x[["requests"]]), quotes = "\""))
    }
  )
}

#' @export
print.output_class_entry <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

# The schema of an output, from the most specific class in its class vector
# that is registered.
output_class_schema <- function(class) {

  for (cls in class) {

    entry <- output_class_entry(cls)

    if (not_null(entry)) {
      return(entry[["schema"]])
    }
  }

  blockr_abort(
    "Output class {class[1L]} is not registered. A package registers its ",
    "output classes with `register_output_class()`.",
    class = "output_class_unknown"
  )
}

# A request of an output, from the most specific class in its class vector
# that has it.
output_class_request <- function(class, name) {

  for (cls in class) {

    entry <- output_class_entry(cls)
    fun <- entry[["requests"]][[name]]

    if (is.function(fun)) {
      return(
        list(fun = fun, volatile = name %in% entry[["volatile"]], class = cls)
      )
    }
  }

  blockr_abort(
    "Output class {class[1L]} has no request {name}.",
    class = "output_class_request_unknown"
  )
}

#' @rdname new_block_output
#' @export
run_output_class_request <- function(x, name, result, args = list(),
                                     options = list(), params = list(),
                                     file = NULL) {

  if (!is_string(name)) {
    blockr_abort(
      "The name of a request is expected to be a string.",
      class = "output_class_request_unknown"
    )
  }

  class <- if (is.character(x)) x else class(x)
  req <- output_class_request(class, name)

  if (!has_unique_names(args)) {
    blockr_abort(
      "The `args` of a request are expected to be named.",
      class = "output_class_request_args_invalid"
    )
  }

  core <- c("result", "options", "params", "file")
  fmls <- formal_names(req[["fun"]])
  unknown <- setdiff(names(args), setdiff(fmls, c(core, "...")))

  if ("..." %in% fmls) {
    unknown <- intersect(names(args), core)
  }

  if (length(unknown)) {
    blockr_abort(
      "Request {name} takes no {qty(unknown)}argument{?s} {unknown} from ",
      "`args`.",
      class = "output_class_request_args_invalid"
    )
  }

  bound <- list(result = result, options = options, params = params,
                file = file)
  bound <- bound[intersect(names(bound), fmls)]

  if (is.null(file)) {
    bound[["file"]] <- NULL
  }

  do.call(req[["fun"]], c(bound, args), quote = TRUE)
}

# The parts of an output that R sends. Its fields are shaped by the JSON Schema
# of the most specific class in its class vector that is registered. Its body,
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
    output = shape_by_schema(fields[!body], output_class_schema(class(x))),
    body = if (any(body)) fields[body][[1L]]
  )
}
