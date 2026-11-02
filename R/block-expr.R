#' Interpolation
#'
#' Interpolation turns what a block class's `expr` returns into the block
#' expression, which R evaluates and an exported script holds. The generic
#' `interpolate()` dispatches on the class of what `expr` returns, `x`, given
#' the upstream block ids by data input, `inputs`, as `list(data = "a")`, with a
#' variadic block's under `...` in link order, each named by its link's input,
#' which is empty for a positional link, as `list(... = list("a", b2 = "b"))`.
#'
#' Four kinds of value are interpolated by core's methods:
#' * A call or a name, built with any tool, such as [base::bquote()] or
#'   [base::call()], which writes the data inputs bare.
#' * A string, which R parses into such a call.
#' * A function whose formals are the data inputs, written bare in its body,
#'   where `.()` stands for a value of the block state, which [base::bquote()]
#'   fills in from the function's environment, the frame `expr` ran in.
#' * An object of class `bquoted_expr`, as [bbquote()] returns it, which
#'   writes the data inputs as placeholders, such as `.(data)`.
#'
#' For a call, a string or a function, the block expression calls a function of
#' the block's data inputs with the upstream ids, so that a head block
#' downstream of block `a` gets
#' `(function(data) utils::head(data, n = 10L))(data = a)`, and a variadic
#' block's `...` is a formal of that function, which the call binds to the ids
#' in link order. For a `bquoted_expr`, the upstream
#' ids take the place of its placeholders, and `...` as an argument of a call
#' stands for a variadic block's inputs, but for the `...` of a function the
#' expression defines, so that `bbquote(utils::head(.(data), n = 10L))` gives
#' `utils::head(a, n = 10L)` and `bbquote(rbind(...))` gives `rbind(a, b2 = b)`.
#' A placeholder is a call to `.` whose only argument is the name of a data
#' input, so any other call to `.`, such as data.table's in `j`, stays as it is.
#'
#' A package adds a class of its own that inherits from `blockr_expr`, with an
#' `interpolate()` method, such as one that fills in a template, without
#' changing core.
#'
#' @param x What a block class's `expr` returns
#' @param inputs Upstream block ids by data input
#'
#' @examples
#' inputs <- list(data = "a")
#'
#' interpolate(quote(utils::head(data, n = 10L)), inputs)
#' interpolate("utils::head(data, n = 10L)", inputs)
#'
#' n <- 10L
#' interpolate(function(data) utils::head(data, n = .(n)), inputs)
#'
#' interpolate(bbquote(utils::head(.(data), n = .(n))), inputs)
#'
#' interpolate(bbquote(rbind(...)), list(... = list("a", b2 = "b")))
#'
#' @return A block expression, a call.
#'
#' @export
interpolate <- function(x, inputs) {
  UseMethod("interpolate")
}

#' @rdname interpolate
#' @export
interpolate.default <- function(x, inputs) {

  if (!is.call(x) && !is.name(x)) {
    blockr_abort(
      "Cannot interpolate an object of class {class(x)} into a block ",
      "expression.",
      class = "interpolate_invalid"
    )
  }

  call_with_inputs(x, empty_formals(names(inputs)), inputs)
}

#' @rdname interpolate
#' @export
interpolate.character <- function(x, inputs) {

  exprs <- parse(text = x, keep.source = FALSE)

  if (length(exprs) == 1L) {
    return(interpolate(exprs[[1L]], inputs))
  }

  interpolate(as.call(c(quote(`{`), as.list(exprs))), inputs)
}

#' @rdname interpolate
#' @export
interpolate.function <- function(x, inputs) {

  if (!setequal(formal_names(x), names(inputs))) {
    blockr_abort(
      "A function to interpolate is expected to have the block's inputs ",
      "{names(inputs)} as its formals.",
      class = "interpolate_invalid"
    )
  }

  call_with_inputs(
    eval(call("bquote", body(x), environment(x))),
    formals(x),
    inputs
  )
}

#' @rdname interpolate
#' @export
interpolate.bquoted_expr <- function(x, inputs) {
  fill_placeholders(unclass(x), inputs)
}

#' @export
print.bquoted_expr <- function(x, ...) {
  cat("<bquoted_expr>", deparse(unclass(x)), sep = "\n")
  invisible(x)
}

new_bquoted_expr <- function(x) {

  if (!is.call(x)) {
    return(x)
  }

  structure(x, class = c("bquoted_expr", "blockr_expr"))
}

empty_formals <- function(names) {
  set_names(rep(list(quote(expr = )), length(names)), names)
}

call_with_inputs <- function(body, formals, inputs) {

  fun <- call("function", as.pairlist(formals), body)

  new_call(c(list(call("(", fun)), input_args(inputs)))
}

input_args <- function(inputs) {

  args <- list()

  for (nm in names(inputs)) {
    if (identical(nm, "...")) {
      args <- c(args, lapply(inputs[[nm]], as.name))
    } else {
      args <- c(args, set_names(list(as.name(inputs[[nm]])), nm))
    }
  }

  args
}

fill_placeholders <- function(x, inputs, in_function = FALSE) {

  if (is_placeholder(x, inputs)) {
    return(as.name(inputs[[as.character(x[[2L]])]]))
  }

  if (!is.call(x)) {
    return(x)
  }

  if (identical(x[[1L]], quote(`function`))) {
    x[3L] <- list(fill_placeholders(x[[3L]], inputs, in_function = TRUE))
    return(x)
  }

  parts <- as.list(x)
  nms <- coal(names(parts), character(length(parts)))
  res <- list()

  for (i in seq_along(parts)) {

    if (is_empty_arg(parts[[i]])) {
      res <- c(res, parts[i])
    } else if (i > 1L && !in_function && !nzchar(nms[i]) &&
                 is_variadic_dots(parts[[i]], inputs)) {
      res <- c(res, lapply(inputs[["..."]], as.name))
    } else {
      res <- c(
        res,
        set_names(
          list(fill_placeholders(parts[[i]], inputs, in_function)),
          nms[i]
        )
      )
    }
  }

  new_call(res)
}

new_call <- function(parts) {

  if (!any(nzchar(coal(names(parts), "")))) {
    names(parts) <- NULL
  }

  as.call(parts)
}

is_variadic_dots <- function(x, inputs) {
  identical(x, quote(...)) && "..." %in% names(inputs)
}

is_placeholder <- function(x, inputs) {
  is.call(x) && length(x) == 2L && identical(x[[1L]], quote(.)) &&
    is.name(x[[2L]]) &&
    as.character(x[[2L]]) %in% setdiff(names(inputs), "...")
}
