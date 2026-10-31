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
