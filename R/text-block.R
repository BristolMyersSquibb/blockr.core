#' Text block constructors
#'
#' A text block produces (markdown styled) text, given some (optional) data
#' input.
#'
#' @param ... Forwarded to `new_text_block()` and [new_block()]
#' @inheritParams new_block
#'
#' @return All blocks constructed via `new_text_block()` inherit from
#' `text_block`.
#'
#' @export
new_text_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "text_block"), ctor, ...)
}
