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

  ordering <- topo_sort(as.matrix(board))

  exprs <- Map(wrap_expr, expressions[ordering], arg_map[ordering])

  exprs <- map(assignment, names(exprs), exprs)
  exprs <- lapply(exprs, deparse)
  exprs <- chr_ply(exprs, paste0, collapse = "\n")

  paste0(exprs, collapse = "\n\n")
}

wrap_expr <- function(expr, env) {
  if (length(env)) {
    call("with", env, expr)
  } else {
    call("local", expr)
  }
}

assignment <- function(name, value) {
  bquote(.(nme) <- .(val), list(nme = as.name(name), val = value))
}
