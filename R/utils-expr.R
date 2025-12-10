#' Quoting utilities
#'
#' Block expressions in blockr are evaluated in a 2-step manner: first in the
#' context of arguments supplied by the user via UI elements and in a second
#' step in the context of input data. The function [base::bquote()] does not
#' allow for terms wrapped in `.()` or `..()` to be missing and this makes it
#' incompatible with this requirement. A drop-in replacement, provided as
#' `bbquote()` addresses this shortcoming.
#'
#' A block like `new_head_block()` is expected to return an expression of the
#' form `utils::head(data, n = 10L)`, which will then be evaluated in an
#' environment where he have a name `data` bound to some dataset. In order to
#' perform some manipulations of such block expressions it is required to
#' somehow mark the terms that correspond to input data and for that we can
#' use the syntax introduced by [base::bquote()]. What we would prefer to have
#' as block expression therefore is not the above, but something like
#' `utils::head(.(data), n = 10L)`, as this affords us more possibilities for
#' performing substitutions (and therefore generates cleaner code).
#'
#' In order to interpolate certain arguments in a first step, we unfortunately
#' cannot use [base::bquote()], but we can use `bbquote()` instead to generate
#' the desired expression.
#'
#' ```r
#' bquote(utils::head(.(data), n = .(n)), list(n = 10L))
#' #> Error in eval(e[[2L]], where) : object 'data' not found
#' bbquote(utils::head(.(data), n = .(n)), list(n = 10L))
#' #> utils::head(.(data), n = 10L)
#' ```
#'
#' This also works with `..()` and splicing.
#'
#' @inheritParams base::bquote
#'
#' @examples
#' bbquote(utils::head(.(data), n = .(n)), list(n = 10L))
#' bbquote(c(.(a), ..(bc)), list(a = "a"))
#' bbquote(c(.(a), ..(bc)), list(a = "a", bc = c("b", "c")), splice = TRUE)
#'
#' @return A language object in the same way as returned by [base::bquote()].
#' Functions `.()` and `..()` throw errors when invoked an only exist to mask
#' check notes "no visible global function definition" for their use.
#'
#' @export
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

  if (missing(expr)) {
    return(bquote())
  }
  
  if (is.name(expr_sub)) {

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

#' @param x Object
#' @rdname bbquote
#' @export
. <- function(x) {
  blockr_abort(
    "Function `.()` is not intended to be called.",
    class = "dot_should_not_be_called"
  )
}

#' @rdname bbquote
#' @export
.. <- function(x) {
  blockr_abort(
    "Function `..()` is not intended to be called.",
    class = "dot_dot_should_not_be_called"
  )
}
