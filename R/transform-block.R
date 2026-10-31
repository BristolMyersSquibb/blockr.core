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
