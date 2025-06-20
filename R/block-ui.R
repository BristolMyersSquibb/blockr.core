#' Block UI
#'
#' The UI associated with a block is created via the generics `expr_ui()` and
#' `block_ui()`. The former is mainly responsible for user inputs that are
#' specific to every block type (i.e. a `subset_block` requires different user
#' inputs compared to a `head_block`, see [new_transform_block()]) and
#' essentially calls the UI function passed as `ui` to [new_block()]. UI that
#' represents block outputs typically is shared among similar block types (i.e.
#' blocks with shared inheritance structure, such as `subset_block` and
#' `head_block`, which both inherit from `transform_block`). This type of UI us
#' created by `block_ui()` and block inheritance is used to deduplicate shared
#' functionality (i.e. by implementing a method for the `transform_block` class
#' only instead of separate methods for `subset_block` and `head_block`.
#' Working in tandem with `block_ui()`, the generic `block_output()` generates
#' the output to be displayed by the UI portion defined via `block_ui()`.
#'
#' The result of `block_output()`, which is evaluated in the [block_server()]
#' context is assigned to `output$result`. Consequently, when referencing
#' the block result in `block_ui()`, this naming convention has to be followed
#' by referring to this as something like `shiny::NS(id, "result")`.
#'
#' @param id Namespace ID
#' @param x Object for which to generate a UI container
#' @param ... Generic consistency
#'
#' @return Both `expr_ui()` and `block_ui()` are expected to return shiny UI
#' (e.g. objects wrapped in a [shiny::tagList()]). For rendering the UI,
#' `block_output()` is required to return the result of a shiny render function.
#' For example, a transform block might show the resulting `data.frame` as an
#' HTML table using the DT package. The corresponding `block_ui()` function
#' would then contain UI created by [DT::dataTableOutput()] and rendering in
#' `block_output()` would then be handled by [DT::renderDT()].
#'
#' @export
block_ui <- function(id, x, ...) {
  UseMethod("block_ui", x)
}

#' @export
block_ui.block <- function(id, x, ...) {
  NULL
}

#' @param state Optional block state, which will be added to the environment
#' where the block `expr` UI is created
#' @rdname block_ui
#' @export
expr_ui <- function(id, x, state = list(), ...) {
  UseMethod("expr_ui", x)
}

#' @export
expr_ui.block <- function(id, x, state = list(), ...) {

  if (...length()) {
    abort(
      paste(
        "Unknown arguments", paste_enum(...names()), "in call to `expr_ui()`."
      ),
      class = "superfluous_expr_ui_args"
    )
  }

  if (!is.list(state) || length(unique(names(state))) != length(state)) {
    abort(
      paste(
        "Argument `state` in call to `expr_ui()` is expected to be a named",
        "list with unique names."
      ),
      class = "unsuitable_expr_ui_state"
    )
  }

  ui_fun <- block_expr_ui(x)

  if (length(state)) {
    environment(ui_fun) <- list2env(state, parent = environment(ui_fun))
  }

  do.call(ui_fun, list(id = NS(id, "expr")))
}

#' @param result Block result
#' @param session Shiny session object
#' @rdname block_ui
#' @export
block_output <- function(x, result, session) {
  UseMethod("block_output")
}

#' @export
block_output.block <- function(x, result, session) {
  NULL
}
