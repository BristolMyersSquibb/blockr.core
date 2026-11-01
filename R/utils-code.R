#' Utilities for code export
#'
#' To facilitate code export, this utility performs much of the heavy lifting
#' to properly arrange and scope block-level expressions.
#'
#' @param expressions Block expressions
#' @param board Board object
#'
#' @return String containing properly arranged block expressions.
#'
#' @export
export_code <- function(expressions, board) {

  if (!all(nzchar(board_block_ids(board)))) {
    blockr_abort(
      "Exporting code requires every block to have an ID.",
      class = "export_code_block_id_missing"
    )
  }

  lnks <- board_links(board)

  arg_map <- lapply(
    split(as.list(lnks), lnks$to),
    function(x) {
      set_names(
        lapply(lst_xtr(x, "from"), as.name),
        arg_refs(chr_xtr(x, "input"))
      )
    }
  )

  expr_type <- set_names(
    chr_ply(board_blocks(board), block_expr_type),
    board_block_ids(board)
  )

  ordering <- topo_sort(as.matrix(board))

  list(
    exprs = expressions[ordering],
    args = set_names(arg_map[ordering], ordering),
    types = expr_type[ordering]
  )
}
