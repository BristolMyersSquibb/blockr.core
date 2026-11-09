#' Quoting utilities
#'
#' A block class's `expr` writes the data inputs of the block expression as
#' placeholders, `.(data)`, and its state args as values, `.(n)`, with
#' `bbquote()`, which returns an object of class `bquoted_expr`, and
#' interpolation then puts the upstream block ids in place of the placeholders
#' (see [interpolate()]). The `bbquote()` function fills in only the values
#' named in `where`, by default the frame it is called from, which for a class's
#' `expr` is the frame holding its state args, and leaves `.(data)` for
#' interpolation, while [base::bquote()] would try to fill in `.(data)` as well
#' and fail, or put in a function such as [utils::data()]. A placeholder keeps
#' an input apart from a column of the same name where a function evaluates an
#' argument in the data, so that
#' `bbquote(subset(.(data), threshold > .(threshold)))` keeps a column called
#' `threshold`.
#'
#' ```r
#' n <- 10L
#' bbquote(utils::head(.(data), n = .(n)))
#' #> <bquoted_expr>
#' #> utils::head(.(data), n = 10L)
#' ```
#'
#' This also works with `..()` and splicing.
#'
#' Both `.()` and `..()` take a single name, given without an argument name.
#' The `bbquote()` function throws an error for any other call to either,
#' such as `.(n + 1L)` or data.table's `.(total = sum(value))`.
#' A class that writes data.table code returns a call or a string instead,
#' where nothing fills in `.()` (see [interpolate()]).
#'
#' @inheritParams base::bquote
#'
#' @examples
#' bbquote(utils::head(.(data), n = .(n)), list(n = 10L))
#' bbquote(c(.(a), ..(bc)), list(a = "a"))
#' bbquote(c(.(a), ..(bc)), list(a = "a", bc = c("b", "c")), splice = TRUE)
#'
#' @return A `bquoted_expr`, a call of class `c("bquoted_expr", "blockr_expr")`,
#' where [base::bquote()] returns a call. A result that is not a call, such as
#' a name, is returned as is. Functions `.()` and `..()` throw errors when
#' invoked and only exist to mask check notes "no visible global function
#' definition" for their use.
#'
#' @export
bbquote <- function(expr, where = parent.frame(), splice = FALSE) {

  is_dot <- function(x) {
    is_single_name_call(x, quote(.))
  }

  is_dots <- function(x) {
    is_single_name_call(x, quote(..))
  }

  # Base bquote() evaluates the first argument of every call to `.`, and with
  # `splice = TRUE` of every call to `..`, where only the names found here are
  # bound.
  # A call to either that holds anything but a single name would fail there or
  # turn into other code, as data.table's `.(x, y)` would into `.(x)`, so it is
  # an error.
  find_dots <- function(x) {
    if (is_dot(x)) {
      list(list(var = as.character(x[[2]]), splice = FALSE))
    } else if (is_dots(x)) {
      list(list(var = as.character(x[[2]]), splice = TRUE))
    } else if (is.call(x) && is.name(x[[1L]]) &&
                 as.character(x[[1L]]) %in% c(".", "..")) {
      blockr_abort(
        "In `bbquote()`, a call to `.()` or `..()` is expected to hold a ",
        "single name, which `{deparse1(x)}` does not. Work a value out ",
        "first and fill it in by its name, or have `expr` return code that ",
        "uses data.table's `.()` as a call or a string.",
        class = "bbquote_invalid"
      )
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
        # Assigning NULL via `[[<-` would delete the element; `[<-` replaces it
        e_list[i] <- list(process_splices(e_list[[i]]))
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
        subst_list[[var]] <- set_names(list(as_dots_call(var)), var)
      } else {
        subst_list[[var]] <- as_dot_call(var)
      }
    }
  }

  new_bquoted_expr(
    process_splices(
      eval(call("bquote", expr_sub, subst_list, splice = splice))
    )
  )
}

# The syntax of bbquote() is a call to `.` or `..` around a single name, given
# without an argument name, so that data.table's `.(n = .N)` is not.
is_single_name_call <- function(x, fun) {
  is.call(x) && length(x) == 2L && identical(x[[1L]], fun) &&
    is.name(x[[2L]]) && !nzchar(coal(names(x)[2L], ""))
}

as_dot_call <- function(x) {
  call(".", as.name(x))
}

as_dots_call <- function(x) {
  call("..", as.name(x))
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
    class = "dots_should_not_be_called"
  )
}
