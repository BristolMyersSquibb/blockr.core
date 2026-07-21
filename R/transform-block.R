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
#' @seealso Real-world transform blocks (e.g. `select`, `mutate`, `filter` and
#' `join`) built on `new_transform_block()` are provided by the
#' [blockr.dplyr](https://bristolmyerssquibb.github.io/blockr.dplyr/) package.
#'
#' @export
new_transform_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "transform_block"), ctor, ...)
}

#' @export
block_output.transform_block <- function(x, result, session) {
  tabular_output(tabular_display(), result, x, session)
}

#' @export
block_ui.transform_block <- function(id, x, ...) {
  tagList(
    tabular_ui(tabular_display(), NS(id, "result"))
  )
}

#' @export
#' @include tabular-display.R
block_render_trigger.transform_block <- function(x, session = get_session()) {
  tabular_trigger(tabular_display(), session)
}

#' @export
board_options.transform_block <- function(x, ...) {
  combine_board_options(
    tabular_options(tabular_display(), ...),
    NextMethod()
  )
}
