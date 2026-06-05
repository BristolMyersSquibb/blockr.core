#' Compact rendering
#'
#' `str_value()` returns a compact string describing an object. It is the
#' value-returning half of the compact rendering tier: where [utils::str()]
#' only displays (it `cat()`s and returns `NULL`), `str_value()` returns the
#' string, mirroring how [format()] returns what [print()] displays in the
#' full, multi-line tier. The [utils::str()] methods are thin wrappers that
#' display `str_value()`.
#'
#' A scalar object (a `block`, `stack`, `link`, `board_option` or `plugin`)
#' renders as a single line; a container (`blocks`, `stacks`, `links`,
#' `board_options`, `plugins`) and a whole `board` render as one element per
#' line below a `<class[n]>` header. The `block` method lists a block's
#' constructor inputs, marking the externally controllable ones (those
#' reported by [external_ctrl_vars()]) with a trailing `*`; the `stack`
#' method shows the stack name and its member block ids.
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
#' @return `str_value()` returns a length-one character vector (multi-line for
#' containers). The [utils::str()] methods are called for their side effect
#' (display) and return their `object` invisibly.
#'
#' @examples
#' str_value(new_dataset_block())
#'
#' str_value(new_stack(c("plot", "data"), name = "My stack"))
#'
#' board <- new_board(c(a = new_dataset_block()))
#' str(board)
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

str_value_collection <- function(x, vals, ids = names(x)) {

  items <- if (length(x)) {
    if (is.null(ids)) paste0("  ", vals) else paste0("  ", ids, ": ", vals)
  }

  paste(
    c(paste0("<", class(x)[1L], "[", length(x), "]>"), items),
    collapse = "\n"
  )
}
