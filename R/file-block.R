#' File block constructors
#'
#' Similarly to [new_data_block()], blocks created via `new_file_block()` serve
#' as starting points in analysis pipelines by providing data to down-stream
#' blocks. They typically will not have data inputs and represent root nodes in
#' analysis graphs.
#'
#' @param ... Forwarded to `new_file_block()` and [new_block()]
#' @inheritParams new_block
#'
#' @return All blocks constructed via `new_file_block()` inherit from
#' `file_block`.
#'
#' @export
new_file_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "file_block"), ctor, ...)
}
