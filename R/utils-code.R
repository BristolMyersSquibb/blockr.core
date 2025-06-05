gen_code <- function(rv) {

  exprs <- lapply(lst_xtr(rv$blocks, c("server", "expr")), reval)

  lnks <- board_links(rv$board)

  arg_map <- lapply(
    split(as.list(lnks), lnks$to),
    function(x) {
      set_names(lapply(lst_xtr(x, "from"), as.name), lst_xtr(x, "input"))
    }
  )

  ordering <- topo_sort(as.matrix(rv$board))

  exprs <- Map(wrap_expr, exprs[ordering], arg_map[ordering])

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
