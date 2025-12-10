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

  exprs <- Map(
    wrap_expr,
    expressions[ordering],
    arg_map[ordering],
    expr_type[ordering]
  )

  exprs <- map(assignment, names(exprs), exprs)
  exprs <- lapply(exprs, deparse)
  exprs <- chr_ply(exprs, paste0, collapse = "\n")

  paste0(exprs, collapse = "\n\n")
}

wrap_expr <- function(expr, env, type) {

  if (identical(type, "bquoted")) {
    expr <- do.call(bquote, list(expr, env))
  }

  if (length(env) && identical(type, "quoted")) {
    call("with", env, expr)
  } else if (has_assignment(expr)) {
    call("local", expr)
  } else {
    expr
  }
}

assignment <- function(name, value) {
  bquote(.(nme) <- .(val), list(nme = as.name(name), val = value))
}

has_assignment <- function(expr) {

  if (is.atomic(expr) || is.name(expr)) {
    return(FALSE)
  }

  if (is.call(expr)) {

    fn <- as.character(expr[[1]])

    if (fn == "<-") {
      return(TRUE)
    } else if (fn %in% c("<<-", "assign")) {
      blockr_warn(
        "Using the global assignment operator `<<-` or calling ",
        "assign() is discouraged, as it can lead to unreliable ",
        "code generation.",
        class = "code_generation_discouraged_assignments"
      )
      return(TRUE)
    }

    for (i in seq_along(expr)[-1]) {
      if (has_assignment(expr[[i]])) {
        return(TRUE)
      }
    }
  }

  if (is.pairlist(expr)) {

    for (i in seq_along(expr)) {
      if (has_assignment(expr[[i]])) {
        return(TRUE)
      }
    }
  }

  FALSE
}

