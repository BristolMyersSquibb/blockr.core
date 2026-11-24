#' Outputs
#'
#' A block's result can be any R object. An output is what R shows of it: the
#' output family that a block class names in its `output` property makes an
#' output from the result (see [new_output_family()]).
#' R sends the output to the client
#' as JSON, and a renderer draws it there. So nothing in the protocol assumes a
#' table.
#'
#' An output is an S3 object of an output class, whose class vector ends in
#' `block_output`. The package that defines an output class gives it two
#' functions. A constructor makes an output of the class and checks its
#' properties.
#' An output family is a generic that makes an output from a result, by the
#' result's class, under the name of the output class, as `table_output()`
#' makes table outputs (see [new_output_family()]).
#' Core defines three output classes, for tables, text and
#' plots (see [new_table_output()], [new_text_output()] and
#' [new_plot_output()]). Their constructors build on `new_block_output()`.
#' Another package extends an output class with a subclass. Its outputs hold
#' every property of the class and add properties of their own.
#'
#' An output may hold one binary body, such as a plot's PNG. R sends the body
#' beside the JSON, not in it. The function `new_reply_body()` makes a body
#' from a raw vector or from a file's path. It takes the body's MIME type and,
#' for a download, a file name.
#'
#' The package that defines an output class gives R its JSON Schema and its
#' requests through its methods of two generics (see [output_schema()]), and
#' the renderer of its outputs through the output family that makes them.
#'
#' @include utils-assertions.R utils-cnd.R utils-ply.R
#' @param x For `new_block_output()`, the output's properties, as a named list,
#'   and for `new_reply_body()`, a raw vector or the path of a file
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
      "The properties of an output are expected to be a list with unique ",
      "names.",
      class = "block_output_invalid"
    )
  }

  if ("renderer" %in% names(x)) {
    blockr_abort(
      "An output cannot hold a property `renderer`, which R adds when it ",
      "sends the output.",
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

validate_block_output <- function(x) {

  if (!is_block_output(x)) {
    blockr_abort(
      "An output is expected to inherit from `block_output`.",
      class = "block_output_invalid"
    )
  }

  invisible(x)
}

#' @export
format.block_output <- function(x, ...) {

  fields <- unclass(x)
  body <- lgl_ply(fields, is_reply_body)

  c(
    paste0("<", class(x)[1L], ">"),
    if (any(!body)) {
      paste0("Properties: ", paste_enum(names(fields)[!body], quotes = "\""))
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

# What a reply or request says of the body it carries, in its property
# `body`: the MIME type and, for a download, the file name.
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

#' Output families
#'
#' An output family makes the outputs of a block class from its results, of
#' whatever data model: a function with a class of its own, which gets a result
#' into the shape of an output class, as [table_output()] makes table outputs
#' from data frames and from any other table that a package gives it a method
#' for.
#' A block class names the family of its outputs in its `output` property (see
#' [new_block_class()]), and R calls the family on a block's result to make an
#' output.
#'
#' A family holds what R needs of it before any output exists: the
#' constructors of the board options of its outputs, as a request that
#' [new_block_request()] makes holds those that the request reads, and the
#' renderer that draws its outputs (see [new_renderer()]).
#' Those board options are the ones that the family reads and the ones that
#' the renderers of its outputs read, since no renderer names any of its own.
#' R names both in the entry of each block class that names the family, in the
#' block registry, so that the client loads the renderer and puts the board
#' options on the board before it asks for a block's first output, and sends
#' their values with every output request.
#' Every renderer of the family's outputs reads their values through its host,
#' whichever renderer a deployment draws them with.
#' A deployment draws a family's outputs with another renderer by naming it in
#' the blockr option `renderers` under the family's class, as
#' `options(blockr.renderers = list(table_output = blockr.ui::table_preview()))`
#' does (see [blockr_option()]).
#' R reads what a family holds from the family as its package defines it now:
#' the one bound under the name of its class where its function was defined.
#' So a block class of a package that was built against an older version of
#' the family's package reads the family's current board options and renderer.
#'
#' The function `new_output_family()` makes a family of a function, by giving
#' it the class `class`, followed by `output_family` and `function`, and what
#' it holds.
#' A family that makes outputs of an output class of its own takes that class's
#' name as its class, as `table_output` does, gives their renderer in
#' `renderer`, and is bound under that name.
#' A family that hands its results to other families, as one that shows a
#' model as a table of its coefficients hands its table to `table_output`,
#' names them in `families`.
#' It then reads their board options too, beside any that it names in
#' `options`, and each of its outputs is drawn by the renderer of the family
#' whose class the output has.
#' A family may do both, and an output of a class that neither the family nor
#' a family that it names draws is an error.
#' A family that is a generic stays one, since S3 registers its methods and
#' dispatches on the result as it does for any generic.
#'
#' A family that takes results of any class, as a code block's family does, is
#' open.
#' An open family gives `families = TRUE`, and hands its results to every
#' family that the available packages list in the `families` of their
#' `._BLOCKR_MODULES_` (see [read_blocks_yml()]).
#' S3 lets any package add a method to an open family, such as
#' `code_output.sf()`, which hands a result to that package's `map_output`.
#' That package then lists `map_output`.
#' R reads the families that the packages list as [serve()] builds the app, as
#' it reads their board options.
#' The entry of a block class whose family is open names the board options and
#' the renderer of every listed family.
#' So each output request of such a block carries the board options of every
#' listed family, and the client loads every listed renderer before the
#' block's first output.
#'
#' @param fun The function that makes an output from the result, with the
#'   board option values, the `info` and the parameters that the output was
#'   asked with, as `options`, `info` and `params`
#' @param class The family's class, such as `"table_output"`, which no other
#'   family has
#' @param options The constructors of the board options that `fun` itself
#'   reads, or that the renderers of its outputs read, a function such as
#'   `new_n_rows_option` or a list of them
#' @param renderer The renderer of the outputs of class `class` that `fun`
#'   makes, made by [new_renderer()]
#' @param families The families that `fun` hands its results to, a family or
#'   a list of them, or `TRUE` for every family that the available packages
#'   list
#'
#' @examples
#' class(table_output)
#'
#' # A family that shows a linear model as a table of its coefficients, drawn
#' # as table_output draws its tables
#' coef_output <- new_output_family(
#'   function(result, ...) {
#'     table_output(as.data.frame(stats::coef(summary(result))), ...)
#'   },
#'   "coef_output",
#'   families = table_output
#' )
#'
#' fit <- stats::lm(Sepal.Length ~ Sepal.Width, datasets::iris)
#' coef_output(fit, options = list(n_rows = 5L))$total
#'
#' # An open family, which hands a data frame to table_output and any other
#' # result to text_output, both of which core lists
#' any_output <- new_output_family(
#'   function(result, ...) {
#'     if (is.data.frame(result)) {
#'       return(table_output(result, ...))
#'     }
#'     text_output(result, ...)
#'   },
#'   "any_output",
#'   families = TRUE
#' )
#'
#' class(any_output(month.name[1:3]))
#'
#' @return The function `fun`, of class `class`, `output_family` and
#' `function`.
#'
#' @export
new_output_family <- function(fun, class, options = NULL, renderer = NULL,
                              families = NULL) {

  if (!is.function(fun)) {
    blockr_abort(
      "An output family is expected to be a function, which makes its ",
      "outputs.",
      class = "output_family_invalid"
    )
  }

  if (!is_nonempty_string(class) ||
        class %in% c("output_family", "function")) {
    blockr_abort(
      "The class of an output family is expected to be a string of its own, ",
      "such as \"table_output\".",
      class = "output_family_invalid"
    )
  }

  if (is_taken_family_class(class, environment(fun))) {
    blockr_abort(
      "Class {class} is that of another output family, which a family that ",
      "hands it its results names in `families`.",
      class = "output_family_invalid"
    )
  }

  options <- as_option_ctors(
    options,
    paste0(
      "The board options that an output family reads are expected to be ",
      "given by their constructors, a function such as `new_n_rows_option` ",
      "or a list of them."
    ),
    "output_family_invalid"
  )

  if (!is.null(renderer) && !is_renderer(renderer)) {
    blockr_abort(
      "The renderer of an output family is expected to be made by ",
      "`new_renderer()`.",
      class = "output_family_invalid"
    )
  }

  if (is_output_family(families)) {
    families <- list(families)
  }

  if (!is.null(families) && !isTRUE(families) &&
        !(is.list(families) && all(lgl_ply(families, is_output_family)))) {
    blockr_abort(
      "The families that an output family hands its results to are expected ",
      "to be an output family or a list of them, or TRUE for every ",
      "family that the available packages list.",
      class = "output_family_invalid"
    )
  }

  if (is.null(renderer) && !length(families)) {
    blockr_abort(
      "An output family gives the renderer of its outputs, or names the ",
      "families that it hands its results to, or both.",
      class = "output_family_invalid"
    )
  }

  structure(
    fun,
    options = options,
    renderer = renderer,
    families = if (length(families)) unname(families),
    class = c(class, "output_family", "function")
  )
}

is_output_family <- function(x) {
  is.function(x) && inherits(x, "output_family")
}

# Whether family `x` is open: one that hands its results to every family that
# the available packages list.
is_open_family <- function(x) {
  isTRUE(attr(x, "families"))
}

# Whether another output family has the class `class`: one bound under that
# name where `env`, the environment of the new family's function, finds it, or
# in core, other than one whose function was defined in `env` too, as a script
# that runs again defines its families anew.
is_taken_family_class <- function(class, env) {

  if (!is.environment(env)) {
    env <- globalenv()
  }

  taken <- function(found) {
    is_output_family(found) && identical(class(found)[1L], class) &&
      !identical(environment(found), env)
  }

  core <- topenv(environment(is_taken_family_class))

  taken(get0(class, envir = env, mode = "function")) ||
    taken(get0(class, envir = core, mode = "function", inherits = FALSE))
}

# The family as its package defines it now: the one bound under the name of
# its class where its function was defined, or else `x` itself, as where that
# binding has gone or holds something else.
current_family <- function(x) {

  env <- environment(x)

  now <- if (is.environment(env)) {
    get0(class(x)[1L], envir = env, inherits = FALSE)
  }

  if (is_output_family(now) && identical(class(now)[1L], class(x)[1L])) {
    return(now)
  }

  x
}

# Every family whose outputs `x` makes, by class: `x` and the families that it
# hands its results to, and theirs, each as its package defines it now and
# each once.
# An open family hands its results to the families that the available packages
# list, `families`, as `app_families()` gives them.
# R reads those lists only where it reaches an open family.
reached_families <- function(x, families = app_families()) {

  todo <- list(x)
  found <- list()

  while (length(todo)) {

    family <- current_family(todo[[1L]])
    todo <- todo[-1L]

    if (class(family)[1L] %in% names(found)) {
      next
    }

    found[[class(family)[1L]]] <- family

    if (is_open_family(family)) {
      todo <- c(todo, unname(families))
    } else {
      todo <- c(todo, attr(family, "families"))
    }
  }

  found
}

# The IDs of the board options that the outputs of `x` read, those of every
# family that it reaches, as their constructors give them, which R names in
# the entry of each block class that names `x`, in the block registry.
# An open family reaches the families that the available packages list,
# `families`.
family_option_ids <- function(x, families = app_families()) {

  ctors <- unlist(
    lapply(reached_families(x, families), attr, "options"),
    recursive = FALSE,
    use.names = FALSE
  )

  option_ctor_ids(ctors)
}

#' Output classes in R
#'
#' The package that defines an output class gives R what it needs of the class
#' through its methods of two generics, which it registers as it registers
#' any S3 method, so that S3 dispatch gives a subclass what its class gives,
#' unless it gives its own.
#' R calls each method on an output that it makes.
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
#' R keeps the requests that the method returns for an output with the block's
#' result, and answers the requests of that output by them.
#' So a method may give requests by what the output holds, as a table's might
#' give one that counts the rows only where the output holds no `total`.
#' A request's function may capture the output, which R then keeps with the
#' requests.
#' R fails to make an output whose method returns no named list of functions.
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
#' The client keeps a request's answers, as it keeps the output.
#' No request of an output class writes.
#' The function `run_output_class_request()` runs a request as R does, to test
#' an output class, or the requests of a renderer, without Shiny.
#' It takes an output, on which it calls the method of `output_requests()`, as
#' R does on each output that it makes.
#'
#' The renderer that draws the class's outputs comes from the output family
#' that makes them, which a deployment may replace (see [new_output_family()]).
#'
#' @param x An output
#' @param ... Generic consistency
#' @param path The path of the file that holds a JSON Schema, relative to the
#'   package's `inst/`, or as given outside a package
#' @param package The package that ships the file (`NULL` outside a package)
#' @param name The name of the request
#' @param result The result that the output was made from
#' @param args Further arguments by name, which the renderer passes
#' @param options The board option values that the output was asked with, by
#'   board option id
#' @param params The parameters that the output was asked with
#' @param file A file that the renderer sends, or `NULL`
#' @param renderer The renderer whose own request it is, made by
#'   [new_renderer()], or `NULL` for a request of the output class
#'
#' @examples
#' words_output <- new_output_family(
#'   function(result, ...) {
#'     new_block_output(list(words = as.character(result)), "words_output")
#'   },
#'   "words_output",
#'   renderer = new_renderer("words_output", js_module("words.js", NULL))
#' )
#'
#' output_schema.words_output <- function(x, ...) {
#'   list(type = "object", properties = list(words = list(type = "array")))
#' }
#'
#' output_requests.words_output <- function(x, ...) {
#'   list(count = function(result) length(result))
#' }
#'
#' words <- words_output("hello")
#'
#' output_schema(words)
#' run_output_class_request(words, "count", result = c("hello", "world"))
#'
#' @return The generic `output_schema()` returns a JSON Schema, as a list,
#' `output_requests()` a named list of functions,
#' `read_json_schema()` the JSON Schema that it reads, as a list, and
#' `run_output_class_request()` the request's answer.
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
read_json_schema <- function(path, package = pkg_name(parent.frame())) {

  if (!is_string(path)) {
    blockr_abort(
      "The path of the file of a JSON Schema is expected to be a string.",
      class = "json_schema_file_invalid"
    )
  }

  file <- if (is.null(package)) path else pkg_file(path, pkg = package)

  if (!nzchar(file) || !file.exists(file)) {
    blockr_abort(
      "Found no JSON Schema file {path}",
      if (is.null(package)) "." else " in package {package}.",
      class = "json_schema_file_invalid"
    )
  }

  decode_schema(file)
}

# The requests that the classes of output `x` give for it, through their
# methods of `output_requests()`, a subclass's before its class's.
# R gets them as it makes the output, and keeps them with the result, by which
# it answers the requests of the output.
output_class_requests <- function(x) {

  reqs <- output_requests(x)

  if (!is.list(reqs) || is.object(reqs) || !all(lgl_ply(reqs, is.function)) ||
        (length(reqs) && (is.null(names(reqs)) || !all(nzchar(names(reqs)))))) {
    blockr_abort(
      "The method of `output_requests()` for output class {class(x)[1L]} ",
      "returns no named list of functions.",
      class = "output_class_requests_invalid"
    )
  }

  reqs
}

# The function of a request of an output: where a renderer is given, one of
# that renderer's own requests, and otherwise one of `requests`, those that
# the output's classes give for it, where `class` is the output's class vector.
output_class_request <- function(requests, class, name, renderer = NULL) {

  if (not_null(renderer)) {
    return(renderer_request(renderer, name))
  }

  fun <- requests[[name]]

  if (!is.function(fun)) {
    blockr_abort(
      "Output class {class[1L]} has no request {name}.",
      class = "output_class_request_unknown"
    )
  }

  fun
}

#' @rdname output_schema
#' @export
run_output_class_request <- function(x, name, result, args = list(),
                                     options = list(), params = list(),
                                     file = NULL, renderer = NULL) {

  validate_block_output(x)

  call <- output_request_call(output_class_requests(x), class(x), name,
                              result, args, options, params, file, renderer)

  do.call(call[["fun"]], call[["args"]], quote = TRUE)
}

# The function of a request of an output, or of a renderer,
# with the arguments that R binds for it,
# so that R can resolve a request apart from running it.
# The output has the class vector `class`, and its classes give it `requests`.
output_request_call <- function(requests, class, name, result, args, options,
                                params, file, renderer) {

  if (!is_string(name)) {
    blockr_abort(
      "The name of a request is expected to be a string.",
      class = "output_class_request_unknown"
    )
  }

  if (not_null(renderer) && !is_renderer(renderer)) {
    blockr_abort(
      "The renderer whose request it is is expected to be made by ",
      "`new_renderer()`.",
      class = "output_class_request_unknown"
    )
  }

  fun <- output_class_request(requests, class, name, renderer)

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
    fun = fun,
    args = request_call_args(
      fun,
      name,
      args,
      bound,
      c("result", "options", "params", "file"),
      "output_class_request_args_invalid"
    )
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

# The parts of an output that R sends. Its properties are shaped by the JSON
# Schema that its classes give, through their methods of `output_schema()`.
# Its body, if it holds one, travels beside them.
shape_output <- function(x) {

  validate_block_output(x)

  fields <- unclass(x)
  body <- lgl_ply(fields, is_reply_body)

  list(
    output = shape_by_schema(fields[!body], output_schema(x)),
    body = if (any(body)) fields[body][[1L]]
  )
}
