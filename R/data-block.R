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
#' @export
new_data_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "data_block"), ctor, ...)
}

#' @export
block_output.data_block <- function(x, result, session) {
  dt_result(result, session)
}

#' @export
block_ui.data_block <- function(id, x, ...) {
  tagList(
    DT::dataTableOutput(NS(id, "result"))
  )
}

#' @export
#' @include utils-dt.R
block_render_trigger.data_block <- dt_render_trigger
