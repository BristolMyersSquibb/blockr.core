declared_renderers <- new.env()

#' Renderers
#'
#' A renderer is a module that draws the outputs of one output class (see
#' [new_block_output()]) or the value of one board option. A package declares
#' its renderers in R, from its `.onLoad()`, with `register_renderer()`: under
#' a name, which gives the renderer the ID `{pkg}-{name}`, as a registry id
#' is, with its module, made by [js_module()], whose package is the one that
#' declares it, and the output class it draws, in `class`, or the board option
#' it draws, in `option`. One module may stand behind several renderers.
#' Declaring a renderer again under the same ID replaces it, as when a package
#' is loaded again. Core declares a plain renderer for each of its output
#' classes: `blockr.core-basic_table`, `blockr.core-basic_text` and
#' `blockr.core-basic_plot`.
#'
#' A renderer of an output class may bring requests of its own, for what the
#' output does not hold, such as the labels of a table's columns, which core's
#' table output does not carry. R runs a renderer's request as it runs a
#' request of the output class (see [run_output_class_request()]), with the
#' result that the output was made from and the board option values and
#' parameters that the output was asked with. A request names the renderer
#' that asks, and R looks for the request among that renderer's own requests
#' first, then among those of the output class, most specific class first.
#'
#' R picks one renderer for each output class and each board option with
#' `pick_renderers()`, for the front-end of the board that the client draws.
#' The pick takes the renderer of the first of these steps that has one:
#'
#' 1. The renderer that the blockr option `renderers` names for the output
#'    class or board option, by ID, as
#'    `options(blockr.renderers = c(table_output = "blockr.core-basic_table"))`
#'    does (see [blockr_option()]).
#' 2. A renderer that the package of the board's front-end declares.
#' 3. A renderer that another package declares.
#' 4. For an output class, a renderer that the package which defines the class
#'    declares.
#'
#' Two renderers at one step are an error naming both, which the app settles
#' through the blockr option. A board option that no renderer is declared for
#' keeps the UI of its definition. Where no package that R has loaded declares
#' the board's front-end, the second step has no renderer, since the
#' front-end's package is unknown.
#'
#' R names in each output it sends the renderer that it picked for the most
#' specific class in the output's class vector that has one, as S3 dispatch
#' would. So the outputs of a subclass that no package draws are drawn by
#' their class's renderer.
#'
#' @param name The name part of the renderer's ID
#' @param module The renderer's module, made by [js_module()]
#' @param class The output class that the renderer draws
#' @param option The ID of the board option that the renderer draws
#' @param requests A named list of the renderer's own requests, each a
#'   function, which only a renderer of an output class has
#' @param front_end The ID of the front-end of the board that the client draws
#'
#' @examples
#' \dontrun{
#' # In the `.onLoad()` of a package such as blockr.ui
#' register_renderer(
#'   "table_preview",
#'   js_module("js/table-preview.js"),
#'   class = "table_output",
#'   requests = list(
#'     labels = function(result) lapply(result, attr, "label", exact = TRUE)
#'   )
#' )
#' }
#'
#' names(pick_renderers("blockr.core-basic"))
#'
#' @return The declaration `register_renderer()` returns the renderer's entry
#' invisibly, a list holding its `id`, `name`, `package`, `module`, `class`,
#' `option` and `requests`, where one of `class` and `option` is `NULL`, while
#' `pick_renderers()` returns the entries of the renderers that R picked, by
#' ID.
#'
#' @export
register_renderer <- function(name, module, class = NULL, option = NULL,
                              requests = NULL) {

  if (!is_js_module(module)) {
    blockr_abort(
      "A renderer's module is expected to be made by `js_module()`.",
      class = "renderer_invalid"
    )
  }

  id <- declared_id(name, module[["package"]], "renderer", "renderer_invalid")

  if (is.null(class) == is.null(option)) {
    blockr_abort(
      "Renderer {id} is declared for either an output class, in `class`, or ",
      "a board option, in `option`.",
      class = "renderer_invalid"
    )
  }

  if (not_null(class) &&
        (!is_nonempty_string(class) || identical(class, "block_output"))) {
    blockr_abort(
      "The output class that renderer {id} draws is expected to be a ",
      "non-empty string other than `block_output`.",
      class = "renderer_invalid"
    )
  }

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

  entry <- list(
    id = id,
    name = name,
    package = module[["package"]],
    module = module,
    class = class,
    option = option,
    requests = requests
  )

  assign(id, entry, envir = declared_renderers)

  invisible(entry)
}

#' @rdname register_renderer
#' @export
pick_renderers <- function(front_end) {

  if (!is_string(front_end)) {
    blockr_abort(
      "A front-end is named by its ID, a string.",
      class = "front_end_invalid"
    )
  }

  entries <- lapply(set_names(nm = list_renderers()), get_renderer)
  chosen <- chosen_renderers(entries)

  front <- get0(front_end, envir = declared_front_ends, inherits = FALSE)

  picked <- c(
    chr_ply(unique(unlst(lst_xtr(entries, "class"))), pick_renderer,
            entries, "class", chosen, front[["package"]]),
    chr_ply(unique(unlst(lst_xtr(entries, "option"))), pick_renderer,
            entries, "option", chosen, front[["package"]])
  )

  entries[names(entries) %in% picked]
}

# The renderer that R picks for an output class or a board option, as `what`
# says, from those declared for it, by the steps of the pick, which
# `register_renderer()` gives. Every renderer declared for it falls into one
# step, so the pick finds one.
pick_renderer <- function(target, entries, what, chosen, front_pkg) {

  named <- chosen[names(chosen) == target]
  cands <- entries[lgl_ply(lst_xtr(entries, what), identical, target)]

  if (any(named %in% names(cands))) {
    return(unname(named))
  }

  pkgs <- chr_xtr(cands, "package")

  owner <- if (identical(what, "class")) {
    output_class_entry(target)[["package"]]
  }

  steps <- list(
    pkgs %in% front_pkg,
    !pkgs %in% c(front_pkg, owner),
    pkgs %in% owner
  )

  for (step in steps) {

    ids <- names(cands)[step]

    if (length(ids) > 1L) {

      what <- c(class = "output class", option = "board option")[[what]]

      blockr_abort(
        "Renderers {ids} draw {what} {target} at the same step of the pick, ",
        "so R cannot pick one. Name the one to draw it in the blockr option ",
        "`renderers`.",
        class = "renderers_ambiguous"
      )
    }

    if (length(ids)) {
      return(ids)
    }
  }
}

# The renderers that the blockr option `renderers` names, by the output class
# or board option that each is to draw, each of which has to be declared for
# it.
chosen_renderers <- function(entries) {

  chosen <- blockr_option("renderers", character())

  if (!is.character(chosen) || anyNA(chosen) || !has_unique_names(chosen)) {
    blockr_abort(
      "The blockr option `renderers` is expected to name a renderer by ID for ",
      "each output class or board option that it picks one for, as ",
      "`c(table_output = \"blockr.core-basic_table\")` does.",
      class = "renderers_blockr_option_invalid"
    )
  }

  for (target in names(chosen)) {

    id <- chosen[[target]]
    entry <- entries[[id]]

    if (is.null(entry)) {
      blockr_abort(
        "The blockr option `renderers` names renderer {id}, which no package ",
        "that R has loaded declares.",
        class = "renderers_blockr_option_invalid"
      )
    }

    if (!identical(entry[["class"]], target) &&
          !identical(entry[["option"]], target)) {
      blockr_abort(
        "The blockr option `renderers` names renderer {id} for {target}, ",
        "which it does not draw.",
        class = "renderers_blockr_option_invalid"
      )
    }
  }

  chosen
}

# The renderer that R names in an output it sends: the one among those it
# picked that draws the most specific class in the output's class vector that
# has one.
output_renderer <- function(x, renderers) {

  class <- if (is.character(x)) x else class(x)

  res <- most_specific(
    class,
    unlst(lst_xtr(renderers, "class"), use_names = TRUE)
  )

  if (is.null(res)) {
    blockr_abort(
      "R picked no renderer for output class {class[1L]} or a class it ",
      "extends, since no package that R has loaded declares one.",
      class = "output_renderer_missing"
    )
  }

  res
}

list_renderers <- function() {
  ls(envir = declared_renderers)
}

get_renderer <- function(id) {
  get_declared(id, declared_renderers, "renderer", "renderer_missing")
}
