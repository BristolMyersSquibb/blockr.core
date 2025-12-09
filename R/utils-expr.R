bbquote <- function(expr, where = parent.frame(), splice = FALSE) {

  maybe_dots <- function(x) {
    is.call(x) && length(x) == 2 && is.name(x[[2]])
  }

  is_dot <- function(x) {
    maybe_dots(x) && identical(x[[1]], quote(.))
  }

  is_dots <- function(x) {
    maybe_dots(x) && identical(x[[1]], quote(..))
  }

  find_dots <- function(x) {
    if (is_dot(x)) {
      list(list(var = as.character(x[[2]]), splice = FALSE))
    } else if (is_dots(x)) {
      list(list(var = as.character(x[[2]]), splice = TRUE))
    } else if (is.call(x) || is.pairlist(x)) {
      unlst(lapply(as.list(x), find_dots))
    } else {
      NULL
    }
  }

  process_splices <- function(e) {

    if (!is.call(e)) {
      return(e)
    }

    e_list <- as.list(e)
    names_e <- coal(names(e_list), rep("", length(e_list)))

    for (i in seq_along(e_list)) {
      if (nchar(names_e[i]) && is_dots(e_list[[i]])) {
        names_e[i] <- ""
      } else {
        e_list[[i]] <- process_splices(e_list[[i]])
      }
    }

    names(e_list) <- names_e

    as.call(e_list)
  }

  expr_sub <- substitute(expr)
  
  if (is.name(expr_sub) && !missing(expr)) {

    expr_val <- tryCatch(
      eval(expr_sub, parent.frame()),
      error = function(e) NULL
    )

    if (!is.null(expr_val) && (is.call(expr_val) || is.expression(expr_val))) {
      expr_sub <- expr_val
    }
  }
  
  dot_vars_list <- find_dots(expr_sub)

  if (length(dot_vars_list) > 0) {
    dot_vars <- chr_xtr(dot_vars_list, "var")
    spl_vars <- unique(dot_vars[lgl_xtr(dot_vars_list, "splice")])
    dot_vars <- unique(dot_vars)
  } else {
    dot_vars <- character(0)
    spl_vars <- character(0)
}
  
  if (is.list(where)) {
    available <- names(where)
  } else {
    available <- ls(envir = where, all.names = TRUE)
  }
  
  subst_list <- list()
  
  for (var in dot_vars) {

    if (var %in% available) {

      if (is.list(where)) {
        subst_list[[var]] <- where[[var]]
      } else {
        subst_list[[var]] <- get(var, envir = where)
      }

    } else {

      if (var %in% spl_vars) {
        subst_list[[var]] <- set_names(list(call("..", as.name(var))), var)
      } else {
        subst_list[[var]] <- call(".", as.name(var))
      }
    }
  }

  process_splices(
    eval(call("bquote", expr_sub, subst_list, splice = splice))
  )
}
