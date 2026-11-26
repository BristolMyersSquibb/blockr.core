#' Renderers
#'
#' A renderer is a module that draws the outputs of an output family (see
#' [new_output_family()]).
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
#' A renderer names no board options of its own.
#' It reads those that its family names for its outputs (see
#' [new_output_family()]), through its host, as `host.option("n_rows")`,
#' which throws for any other ID, naming the block class, the renderer and the
#' ID.
#' So the board options of a family's outputs are the same, whichever renderer
#' a deployment draws them with.
#' It draws its family's outputs by their output class, whatever data model a
#' method of the family made them from, so a new renderer needs no change to
#' any of the family's methods.
#'
#' Any package can offer another renderer, such as a richer table, through a
#' function that makes it.
#' A deployment draws with it by naming it in the blockr option `renderers`,
#' under the class of the family whose outputs it is to draw, such as
#' `table_output`, as the examples do (see [blockr_option()]).
#' No package offers a renderer for core's outputs yet, so core's own draw
#' them until one does.
#' R reads that choice before the family's own renderer, so loading a package
#' that offers a renderer changes nothing, and no package takes the choice
#' away.
#'
#' A renderer may bring requests of its own, for what the output
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
#'   function
#'
#' @examples
#' # A renderer that a package, here mypkg, offers, which draws a table with
#' # the labels of its columns
#' labelled_table <- function() {
#'   new_renderer(
#'     "labelled_table",
#'     js_module("js/labelled-table.js", package = "mypkg"),
#'     requests = list(
#'       labels = function(result) lapply(result, attr, "label", exact = TRUE)
#'     )
#'   )
#' }
#'
#' labelled_table()
#'
#' # In an app script, which draws every table output with it
#' options(blockr.renderers = list(table_output = labelled_table()))
#'
#' options(blockr.renderers = NULL)
#'
#' @return The constructor `new_renderer()` returns an object of class
#' `renderer`.
#'
#' @export
new_renderer <- function(name, module, requests = list()) {

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

  requests <- coal(requests, list(), fail_all = FALSE)

  if (!is.list(requests) || !all(lgl_ply(requests, is.function)) ||
        !has_unique_names(requests)) {
    blockr_abort(
      "The requests of renderer {id} are expected to be a list of functions ",
      "with unique names.",
      class = "renderer_requests_invalid"
    )
  }

  structure(
    list(
      id = id,
      name = name,
      package = package,
      module = module,
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

# The renderers that the blockr option `renderers` names, by the class of the
# output family whose outputs each is to draw.
chosen_renderers <- function() {

  chosen <- blockr_option("renderers", list())

  if (!is.list(chosen) || is.object(chosen) || !has_unique_names(chosen) ||
        !all(lgl_ply(chosen, is_renderer))) {
    blockr_abort(
      "The blockr option `renderers` is expected to be a list of renderers, ",
      "each made by `new_renderer()` and named by the class of the output ",
      "family whose outputs it draws, such as `table_output`.",
      class = "renderers_blockr_option_invalid"
    )
  }

  chosen
}

# The renderers that draw the outputs of family `x`, by the class of the family
# whose outputs each draws: for each family that `x` reaches and that gives a
# renderer, the one that the deployment names for its class in the blockr
# option `renderers`, or else its own.
# An open family reaches the families that the available packages list,
# `families`.
family_renderers <- function(x, families = app_families()) {

  chosen <- chosen_renderers()
  drawing <- Filter(function(f) not_null(attr(f, "renderer")),
                    reached_families(x, families))

  Map(
    function(family, class) {
      coal(chosen[[class]], attr(family, "renderer"), fail_all = FALSE)
    },
    drawing,
    names(drawing)
  )
}

# The renderer that draws `out`, an output that family `x` made: that of the
# family whose class comes first in the output's class vector, among the
# families that `x` reaches, where an open family reaches the families that
# the available packages list, `families`.
drawing_renderer <- function(x, out, families = app_families()) {

  renderers <- family_renderers(x, families)
  drawn <- intersect(class(out), names(renderers))

  if (!length(drawn)) {

    others <- if (is_open_family(x)) {
      "a family that the available packages list"
    } else {
      "a family that it names in `families`"
    }

    blockr_abort(
      "Output family {class(x)[1L]} made an output of class ",
      "{class(out)[1L]}, which neither it nor ", others, " draws.",
      class = "output_renderer_missing"
    )
  }

  renderers[[drawn[1L]]]
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
