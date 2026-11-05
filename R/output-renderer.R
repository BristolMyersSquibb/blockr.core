#' Renderers
#'
#' A renderer is a module that draws the outputs of an output class (see
#' [output_schema()]) or the value of a board option.
#' A renderer object, which `new_renderer()` makes, holds the renderer's
#' module, made by [js_module()], its ID and the renderer's own requests.
#' Its ID is `{pkg}-{name}`, as a registry ID is, where `pkg` is the package
#' of its module, and its name alone for a module from no package, such as one
#' beside an app script.
#'
#' The package that defines an output class gives the renderer that draws its
#' outputs through its method of `output_renderer()`, as core's method for
#' table outputs gives `blockr.core-table_output`.
#' S3 dispatch gives a subclass the renderer of its class, unless the subclass
#' has a method of its own, so that the outputs of a lazy table are drawn as
#' tables until a package or an app script gives the subclass a renderer.
#' R names the renderer in each output that it sends, with where it serves the
#' renderer's module, so that the client loads a renderer the first time that
#' an output names it, however late R learns of the output's class.
#'
#' Any package can offer another renderer, such as a richer table, through a
#' function that makes it.
#' An app draws with it by naming it in the blockr option `renderers`, under
#' the output class that it is to draw, as
#' `options(blockr.renderers = list(table_output = blockr.ui::table_preview()))`
#' does (see [blockr_option()]).
#' A method of `output_renderer()` takes the app's choice for its class with
#' `chosen_renderer()`, which gives the renderer that the app names for the
#' class, or else the default it is given.
#' So loading a package that offers a renderer changes nothing.
#' A renderer that draws a board option names it in `option` and replaces the
#' board option's UI where the app names it in `renderers` under the board
#' option's ID, and R sends every such renderer in its reply to `hello`.
#'
#' A renderer of an output class may bring requests of its own, for what the
#' output does not hold, such as the labels of a table's columns, which core's
#' table output does not carry.
#' R runs a renderer's request as it runs a request of the output class (see
#' [run_output_class_request()]), with the result that the output was made
#' from and the board option values and parameters that the output was asked
#' with.
#' A request that names a renderer is one of the requests of the renderer that
#' draws the output, which has to be the one named, and a request that names
#' none is one of the output class's.
#' So a renderer's request and a request of the output class may share a name,
#' and neither hides the other.
#'
#' @param name The name part of the renderer's ID
#' @param module The renderer's module, made by [js_module()]
#' @param requests A named list of the renderer's own requests, each a
#'   function, which only a renderer of an output class has
#' @param option The ID of the board option that the renderer draws, or `NULL`
#'   for a renderer of an output class
#' @param x An output
#' @param target The output class or the board option that an app may name a
#'   renderer for
#' @param default The renderer to give where the app names none
#' @param ... Generic consistency
#'
#' @examples
#' # A renderer that a package such as blockr.ui offers
#' table_preview <- function() {
#'   new_renderer(
#'     "table_preview",
#'     js_module("js/table-preview.js", package = "blockr.ui"),
#'     requests = list(
#'       labels = function(result) lapply(result, attr, "label", exact = TRUE)
#'     )
#'   )
#' }
#'
#' output_renderer(as_table_output(datasets::BOD))
#'
#' # In an app script
#' options(blockr.renderers = list(table_output = table_preview()))
#'
#' output_renderer(as_table_output(datasets::BOD))
#'
#' options(blockr.renderers = NULL)
#'
#' @return The constructor `new_renderer()` returns an object of class
#' `renderer`, the generic `output_renderer()` the renderer that draws an
#' output, and `chosen_renderer()` the renderer that the app names for
#' `target`, or `default`.
#'
#' @export
new_renderer <- function(name, module, requests = list(), option = NULL) {

  if (!is_js_module(module)) {
    blockr_abort(
      "A renderer's module is expected to be made by `js_module()`.",
      class = "renderer_invalid"
    )
  }

  if (!is_nonempty_string(name)) {
    blockr_abort(
      "A renderer's name is expected to be a non-empty string.",
      class = "renderer_invalid"
    )
  }

  package <- module[["package"]]
  id <- if (is.null(package)) name else paste0(package, "-", name)

  if (not_null(option) && !is_nonempty_string(option)) {
    blockr_abort(
      "The board option that renderer {id} draws is expected to be named by ",
      "its ID, a non-empty string.",
      class = "renderer_invalid"
    )
  }

  requests <- coal(requests, list(), fail_all = FALSE)

  if (!is.list(requests) || !all(lgl_ply(requests, is.function)) ||
        !has_unique_names(requests)) {
    blockr_abort(
      "The requests of renderer {id} are expected to be a list of functions ",
      "with unique names.",
      class = "renderer_requests_invalid"
    )
  }

  if (length(requests) && not_null(option)) {
    blockr_abort(
      "Renderer {id} draws a board option, and only a renderer of an output ",
      "class has requests of its own.",
      class = "renderer_requests_invalid"
    )
  }

  structure(
    list(
      id = id,
      name = name,
      package = package,
      module = module,
      option = option,
      requests = requests
    ),
    class = "renderer"
  )
}

is_renderer <- function(x) {
  inherits(x, "renderer")
}

#' @export
format.renderer <- function(x, ...) {
  c(
    paste0("<renderer> ", x[["id"]]),
    if (not_null(x[["option"]])) paste0("Board option: ", x[["option"]]),
    if (length(x[["requests"]])) {
      paste0("Requests: ", paste_enum(names(x[["requests"]]), quotes = "\""))
    }
  )
}

#' @export
print.renderer <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @rdname new_renderer
#' @export
output_renderer <- function(x, ...) {
  UseMethod("output_renderer")
}

#' @rdname new_renderer
#' @export
output_renderer.block_output <- function(x, ...) {
  blockr_abort(
    "R knows no renderer for output class {class(x)[1L]}, which its package ",
    "gives through a method of `output_renderer()`.",
    class = "output_renderer_missing"
  )
}

#' @rdname new_renderer
#' @export
chosen_renderer <- function(target, default = NULL) {

  if (!is_nonempty_string(target)) {
    blockr_abort(
      "An app names a renderer for an output class or a board option, by its ",
      "name or ID, a non-empty string.",
      class = "renderer_invalid"
    )
  }

  coal(chosen_renderers()[[target]], default, fail_all = FALSE)
}

# The renderers that the blockr option `renderers` names, by the output class
# or the board option that each is to draw.
chosen_renderers <- function() {

  chosen <- blockr_option("renderers", list())

  if (!is.list(chosen) || is.object(chosen) || !has_unique_names(chosen) ||
        !all(lgl_ply(chosen, is_renderer))) {
    blockr_abort(
      "The blockr option `renderers` is expected to be a list of renderers, ",
      "each made by `new_renderer()` and named by the output class or the ",
      "board option that it draws, as ",
      "`list(table_output = blockr.ui::table_preview())` is.",
      class = "renderers_blockr_option_invalid"
    )
  }

  for (target in names(chosen)) {

    option <- chosen[[target]][["option"]]

    if (not_null(option) && !identical(option, target)) {
      blockr_abort(
        "The blockr option `renderers` names renderer ",
        "{chosen[[target]]$id} for {target}, while it draws board option ",
        "{option}.",
        class = "renderers_blockr_option_invalid"
      )
    }
  }

  chosen
}

# The renderer that draws an output, as the method of `output_renderer()` for
# the most specific of its classes that has one gives it.
renderer_of <- function(x) {

  res <- output_renderer(x)

  if (!is_renderer(res) || not_null(res[["option"]])) {
    blockr_abort(
      "The method of `output_renderer()` for output class {class(x)[1L]} ",
      "gives no renderer of an output class.",
      class = "renderer_invalid"
    )
  }

  res
}

# The renderers that the app names for board options, by ID, which R sends in
# its reply to `hello`, since a board option's UI mounts without an output.
option_renderers <- function() {

  chosen <- Filter(function(x) not_null(x[["option"]]), chosen_renderers())

  set_names(unname(chosen), chr_xtr(chosen, "id"))
}

# A renderer's own request, for an output that the renderer has to draw.
renderer_request <- function(renderer, out, name) {

  drawn <- renderer_of(out)

  if (!identical(drawn[["id"]], renderer)) {
    blockr_abort(
      "Renderer {renderer} draws no output of class {class(out)[1L]}, which ",
      "renderer {drawn$id} draws.",
      class = "renderer_class_mismatch"
    )
  }

  fun <- drawn[["requests"]][[name]]

  if (!is.function(fun)) {
    blockr_abort(
      "Renderer {renderer} has no request {name}.",
      class = "renderer_request_unknown"
    )
  }

  list(fun = fun, volatile = FALSE)
}
