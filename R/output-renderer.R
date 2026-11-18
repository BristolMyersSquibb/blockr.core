#' Renderers
#'
#' A renderer is a module that draws the outputs of an output family (see
#' [new_output_family()]) or the value of a board option.
#' A renderer object, which `new_renderer()` makes, holds the renderer's
#' module, made by [js_module()], its ID and the renderer's own requests.
#' Its ID is `{pkg}-{name}`, as a registry ID is, where `pkg` is the package
#' of its module, and its name alone for a module from no package, such as one
#' beside an app script.
#'
#' The package that defines an output family gives the renderer that draws its
#' outputs as the family's `renderer`, as core's `table_output` holds
#' `blockr.core-table_output`.
#' R names it in the entry of each block class that names the family, in the
#' block registry, so that the client loads it before it asks for a block's
#' first output, and in each output that it sends.
#' A renderer names only the board options that it reads itself, since the
#' family names those that it reads.
#' It draws its family's outputs by their output class, whatever data model a
#' method of the family made them from, so a new renderer needs no change to
#' any of the family's methods.
#'
#' Any package can offer another renderer, such as a richer table, through a
#' function that makes it.
#' A deployment draws with it by naming it in the blockr option `renderers`,
#' under the family whose outputs it is to draw, as
#' `options(blockr.renderers = list(table_output = blockr.ui::table_preview()))`
#' does (see [blockr_option()]).
#' R reads that choice before the family's own renderer, so loading a package
#' that offers a renderer changes nothing, and no package takes the choice
#' away.
#' A renderer that draws a board option names it in `option` and replaces the
#' board option's UI where the app names it in `renderers` under the board
#' option's ID, and R sends every such renderer in its reply to `hello`.
#'
#' A renderer of outputs may bring requests of its own, for what the output
#' does not hold, such as the labels of a table's columns, which core's table
#' output does not carry.
#' R runs a renderer's request as it runs a request of the output class (see
#' [run_output_class_request()]), with the result that the output was made
#' from and the board option values and parameters that the output was asked
#' with.
#' A request that names a renderer is one of the requests of the renderer that
#' drew the output, which has to be the one named, and a request that names
#' none is one of the output class's.
#' So a renderer's request and a request of the output class may share a name,
#' and neither hides the other.
#'
#' @param name The name part of the renderer's ID
#' @param module The renderer's module, made by [js_module()]
#' @param requests A named list of the renderer's own requests, each a
#'   function, which only a renderer of outputs has
#' @param option The ID of the board option that the renderer draws, or `NULL`
#'   for a renderer of outputs
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
#' table_preview()
#'
#' # In an app script, which draws every table output with it
#' options(blockr.renderers = list(table_output = table_preview()))
#'
#' options(blockr.renderers = NULL)
#'
#' @return The constructor `new_renderer()` returns an object of class
#' `renderer`.
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

# The renderers that draw the outputs of family `x`, by the class of the family
# whose outputs each draws: for each family that `x` reaches and that gives a
# renderer, the one that the deployment names for its class in the blockr
# option `renderers`, or else its own.
family_renderers <- function(x) {

  chosen <- chosen_renderers()
  drawing <- Filter(function(f) not_null(attr(f, "renderer")),
                    reached_families(x))

  Map(
    function(family, class) {
      validate_renderer(
        coal(chosen[[class]], attr(family, "renderer"), fail_all = FALSE),
        family
      )
    },
    drawing,
    names(drawing)
  )
}

# The renderer that draws `out`, an output that family `x` made: that of the
# family whose class comes first in the output's class vector, among the
# families that `x` reaches.
drawing_renderer <- function(x, out) {

  renderers <- family_renderers(x)
  drawn <- intersect(class(out), names(renderers))

  if (!length(drawn)) {
    blockr_abort(
      "Output family {class(x)[1L]} made an output of class ",
      "{class(out)[1L]}, which neither it nor a family that it names in ",
      "`families` draws.",
      class = "output_renderer_missing"
    )
  }

  renderers[[drawn[1L]]]
}

# Checks that `x`, the renderer that draws the outputs of family `from`, is a
# renderer of outputs, as a deployment's choice in `renderers` may not be.
validate_renderer <- function(x, from) {

  if (!is_renderer(x) || not_null(x[["option"]])) {
    blockr_abort(
      "The renderer of output family {class(from)[1L]} is expected to be a ",
      "renderer of outputs, which draws no board option.",
      class = "renderer_invalid"
    )
  }

  invisible(x)
}

# The renderers that the app names for board options, by ID, which R sends in
# its reply to `hello`, since a board option's UI mounts without an output.
option_renderers <- function() {

  chosen <- Filter(function(x) not_null(x[["option"]]), chosen_renderers())

  set_names(unname(chosen), chr_xtr(chosen, "id"))
}

# The function of a request of `renderer`, its own.
renderer_request <- function(renderer, name) {

  fun <- renderer[["requests"]][[name]]

  if (!is.function(fun)) {
    blockr_abort(
      "Renderer {renderer$id} has no request {name}.",
      class = "renderer_request_unknown"
    )
  }

  fun
}
