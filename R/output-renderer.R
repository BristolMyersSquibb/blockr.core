declared_renderers <- new.env()

#' Renderers
#'
#' A renderer is a module that draws the outputs of one output class (see
#' [new_block_output()]) or the value of one board option. Each output class
#' has a default renderer, which the package that defines the class registers
#' with it (see [register_output_class()]) under the ID `{pkg}-{class}`. Core's
#' are `blockr.core-table_output`, `blockr.core-text_output` and
#' `blockr.core-plot_output`. A board option has the UI of its definition.
#'
#' Any package can supply another renderer, such as a richer table. It declares
#' the renderer in R, from its `.onLoad()`, with `register_renderer()`: under a
#' name, which gives the renderer the ID `{pkg}-{name}`, as a registry id is,
#' with its module, made by [js_module()], whose package is the one that
#' declares it, and the output class it draws, in `class`, or the board option
#' it draws, in `option`. One module may stand behind several renderers.
#' Declaring a renderer again under the same ID replaces it, as when a package
#' is loaded again. An app draws with such a renderer by naming it in the
#' blockr option `renderers`, by ID, as
#' `options(blockr.renderers = c(table_output = "blockr.ui-table_preview"))`
#' does (see [blockr_option()]). So a renderer that `register_renderer()`
#' declares draws only where the app names it, and loading its package changes
#' nothing.
#'
#' Such a renderer of an output class may bring requests of its own, for what
#' the output does not hold, such as the labels of a table's columns, which
#' core's table output does not carry. R runs a renderer's request as it runs a
#' request of the output class (see [run_output_class_request()]), with the
#' result that the output was made from and the board option values and
#' parameters that the output was asked with. A request that names a renderer
#' is one of that renderer's own, which R finds among its requests alone, and
#' a request that names none is one of the output class's. So a renderer's
#' request and a request of the output class may share a name, and neither
#' hides the other.
#'
#' R picks one renderer for each output class and each board option with
#' `pick_renderers()`, whatever the board and its front-end: the one that the
#' blockr option names, and otherwise an output class's default. R names in
#' each output it sends the renderer that it picked for the most specific class
#' in the output's class vector that has one, as S3 dispatch would. So the
#' outputs of a subclass registered without a renderer are drawn by the
#' renderer of the class it extends.
#'
#' @param name The name part of the renderer's ID
#' @param module The renderer's module, made by [js_module()]
#' @param class The output class that the renderer draws
#' @param option The ID of the board option that the renderer draws
#' @param requests A named list of the renderer's own requests, each a
#'   function, which only a renderer of an output class has
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
#' names(pick_renderers())
#'
#' @return The declaration `register_renderer()` returns the renderer's entry
#' invisibly, a list holding its `id`, `name`, `package`, `module`, `class`,
#' `option` and `requests`, where one of `class` and `option` is `NULL`, while
#' `pick_renderers()` returns the entries of the renderers that R picked, by
#' ID, among them the defaults of output classes, each with its class as its
#' name and no requests.
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
pick_renderers <- function() {

  entries <- known_renderers()
  chosen <- chosen_renderers(entries)

  picked <- c(
    chr_ply(unique(unlst(lst_xtr(entries, "class"))), pick_renderer, chosen,
            "class"),
    chr_ply(unique(unlst(lst_xtr(entries, "option"))), pick_renderer, chosen,
            "option")
  )

  entries[names(entries) %in% picked[!is.na(picked)]]
}

# The renderer that R picks for an output class or a board option, as `what`
# says: the one that the blockr option `renderers` names for it, and otherwise,
# for an output class, its default, or `NA` where neither is.
pick_renderer <- function(target, chosen, what) {

  if (target %in% names(chosen)) {
    return(chosen[[target]])
  }

  entry <- if (identical(what, "class")) output_class_entry(target)

  if (not_null(entry[["renderer"]])) {
    return(default_renderer_id(entry))
  }

  NA_character_
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
      "`c(table_output = \"blockr.ui-table_preview\")` does.",
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

# Every renderer that R can pick, by ID: the defaults of the output classes
# registered, and those that packages declare.
known_renderers <- function() {

  declared <- lapply(set_names(nm = list_renderers()), get0,
                     envir = declared_renderers, inherits = FALSE)
  defaults <- default_renderers()

  clash <- intersect(names(declared), names(defaults))

  if (length(clash)) {
    blockr_abort(
      "Renderer{?s} {clash} {?is/are} declared under the ID of the default ",
      "renderer of an output class.",
      class = "renderer_invalid"
    )
  }

  c(defaults, declared)
}

# The default renderers of the output classes registered, as entries like
# those that `register_renderer()` makes.
default_renderers <- function() {

  classes <- lapply(ls(envir = output_class_registry), output_class_entry)
  classes <- classes[!lgl_ply(lst_xtr(classes, "renderer"), is.null)]

  res <- lapply(
    classes,
    function(x) {
      list(
        id = default_renderer_id(x),
        name = x[["class"]],
        package = x[["package"]],
        module = x[["renderer"]],
        class = x[["class"]],
        option = NULL,
        requests = list()
      )
    }
  )

  set_names(res, chr_xtr(res, "id"))
}

default_renderer_id <- function(entry) {
  paste0(entry[["package"]], "-", entry[["class"]])
}

list_renderers <- function() {
  ls(envir = declared_renderers)
}

get_renderer <- function(id) {

  res <- known_renderers()[[id]]

  if (is.null(res)) {
    blockr_abort("No renderer is declared as {id}.", class = "renderer_missing")
  }

  res
}

# A renderer's own request, for an output of the class vector given, which the
# renderer has to draw.
renderer_request <- function(renderer, class, name) {

  entry <- get_renderer(renderer)

  if (!isTRUE(entry[["class"]] %in% class)) {
    blockr_abort(
      "Renderer {renderer} draws no output of class {class[1L]}.",
      class = "renderer_class_mismatch"
    )
  }

  fun <- entry[["requests"]][[name]]

  if (!is.function(fun)) {
    blockr_abort(
      "Renderer {renderer} has no request {name}.",
      class = "renderer_request_unknown"
    )
  }

  list(fun = fun, volatile = FALSE, renderer = renderer)
}
