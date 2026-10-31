#' Utilities for code export
#'
#' A script holds each block expression assigned to its block id. Since every
#' block expression holds the ids of the blocks upstream of it (see
#' [interpolate()]), `export_code()` needs no board: it assigns each block
#' expression to its block id, in the order its caller gives, which puts every
#' block after those it reads where the caller passes them in topological
#' order, as [topo_sort()] gives it.
#'
#' @param x Block expressions, a list named by block id
#'
#' @examples
#' export_code(
#'   list(
#'     a = quote(datasets::BOD),
#'     b = quote(utils::head(a, n = 3L))
#'   )
#' )
#'
#' @return A list of calls, each assigning a block expression to its block id,
#' named by block id.
#'
#' @export
export_code <- function(x) {

  ids <- names(x)
  named <- !length(x) ||
    (not_null(ids) && all(nzchar(ids)) && !anyDuplicated(ids))

  if (!is.list(x) || !named) {
    blockr_abort(
      "Block expressions are expected as a list named by unique block ids.",
      class = "export_code_ids_invalid"
    )
  }

  if (!all(lgl_ply(x, is.language))) {
    blockr_abort(
      "Block expressions are expected to be calls or names.",
      class = "export_code_exprs_invalid"
    )
  }

  set_names(map(assign_block_expr, ids, x), ids)
}

assign_block_expr <- function(id, expr) {
  call("<-", as.name(id), expr)
}
