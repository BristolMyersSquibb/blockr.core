#' Data block constructors
#'
#' Data blocks typically do not have data inputs and represent root nodes in
#' analysis graphs. Intended as initial steps in a pipeline, such blocks are
#' responsible for providing down-stream blocks with data.
#'
#' @param ... Forwarded to `new_data_block()` and [new_block()]
#' @inheritParams new_block
#'
#' @return All blocks constructed via `new_data_block()` inherit from
#' `data_block`.
#'
#' @seealso Real-world data blocks built on `new_data_block()` are provided by
#' the [blockr.io](https://bristolmyerssquibb.github.io/blockr.io/) package,
#' which sources data from a range of external formats.
#'
#' @export
new_data_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "data_block"), ctor, ...)
}

#' @export
block_output.data_block <- function(x, result, session) {
  tabular_output(tabular_display(), result, x, session)
}

#' @export
block_ui.data_block <- function(id, x, ...) {
  tagList(
    tabular_ui(tabular_display(), NS(id, "result"))
  )
}

#' @export
#' @include tabular-display.R
block_render_trigger.data_block <- function(x, session = get_session()) {
  tabular_render_trigger(tabular_display(), session)
}

#' @export
board_options.data_block <- function(x, ...) {
  combine_board_options(
    tabular_board_options(tabular_display(), ...),
    NextMethod()
  )
}
