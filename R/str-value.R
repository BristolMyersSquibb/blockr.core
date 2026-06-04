#' Compact one-line rendering
#'
#' `str_value()` returns a compact, one-line string describing an object. It
#' is the value-returning half of the compact rendering tier: where
#' [utils::str()] only displays (it `cat()`s and returns `NULL`),
#' `str_value()` returns the string, mirroring how [format()] returns what
#' [print()] displays in the full, multi-line tier. The [utils::str()]
#' methods for blocks and stacks are thin wrappers that display
#' `str_value()`.
#'
#' The `block` method lists a block's constructor inputs, marking the
#' externally controllable ones (those reported by [external_ctrl_vars()])
#' with a trailing `*`; the `stack` method shows the stack name and its
#' member block ids.
#'
#' This is the blockr extension point for token-dense renderings such as a
#' board summary. A home package surfaces a subclass's state by defining a
#' `str_value()` method, typically extending the parent's via [NextMethod()]
#' (the way `format.dock_stack()` appends a stack colour). [print()] and
#' [format()] are unaffected and remain the full, multi-line tier.
#'
#' @param x Object to render.
#' @param ... Generic consistency.
#'
#' @return `str_value()` returns a length-one character vector. The
#' [utils::str()] methods are called for their side effect (one line on the
#' console) and return their `object` invisibly.
#'
#' @examples
#' str_value(new_dataset_block())
#'
#' str_value(new_stack(c("plot", "data"), name = "My stack"))
#'
#' @export
str_value <- function(x, ...) {
  UseMethod("str_value")
}

#' @rdname str_value
#' @export
str_value.default <- function(x, ...) {
  paste0("<", class(x)[1L], ">")
}

cat_str_value <- function(object, ...) {
  cat(" ", str_value(object), "\n", sep = "")
  invisible(object)
}
