#' Utilities for code export
#'
#' To facilitate other means of code export than implemented by the default
#' [generate_code()] plugin, this utility performs much of the heavy lifting
#' to properly arrange and scope block-level expressions.
#'
#' @param expressions Block expressions
#' @param board Board object
#'
#' @return String containing properly arranged block expressions.
#'
#' @export
export_code <- function(expressions, board) {

  lnks <- board_links(board)

  arg_map <- lapply(
    split(as.list(lnks), lnks$to),
    function(x) {
      set_names(lapply(lst_xtr(x, "from"), as.name), lst_xtr(x, "input"))
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

export_wrapped_code <- function(expressions, board) {

  exprs <- do.call(
    Map,
    c(
      list(wrap_expr),
      export_code(expressions, board)
    )
  )

  exprs <- map(assignment, names(exprs), exprs)
  exprs <- lapply(exprs, deparse)
  exprs <- chr_ply(exprs, paste0, collapse = "\n")

  paste0(exprs, collapse = "\n\n")
}

wrap_expr <- function(exprs, args, types) {

  if (identical(types, "bquoted")) {
    exprs <- do.call(bquote, list(exprs, args))
  }

  if (length(args) && identical(types, "quoted")) {
    call("with", args, exprs)
  } else {
    call("local", exprs)
  }
}

assignment <- function(name, value) {
  bquote(.(nme) <- .(val), list(nme = as.name(name), val = value))
}
