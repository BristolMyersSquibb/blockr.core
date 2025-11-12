#' Transform block constructors
#'
#' Many data transformations are be provided by blocks constructed via
#' `new_transform_block()`, including examples where a single `data.frame` is
#' transformed into another (e.g. `subset_block`), and two or more `data.frame`s
#' are combined (e.g. `merge_block` or `rbind_block`).
#'
#' @param ... Forwarded to `new_transform_block()` and [new_block()]
#' @inheritParams new_block
#'
#' @return All blocks constructed via `new_transform_block()` inherit from
#' `transform_block`.
#'
#' @export
new_transform_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "transform_block"), ctor, ...)
}

#' @export
block_output.transform_block <- function(x, result, session) {
  dt_result(result, session)
}

#' @export
block_ui.transform_block <- function(id, x, ...) {
  tagList(
    DT::dataTableOutput(NS(id, "result"))
  )
}

#' @export
block_render_trigger.transform_block <- dt_render_trigger

#' @export
board_options.transform_block <- function(x, ...) {
  new_board_options(
    new_n_rows_option(...),
    new_page_size_option(...),
    new_filter_rows_option(...)
  )
}
