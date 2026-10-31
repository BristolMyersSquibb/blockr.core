#' Board options
#'
#' User settings at the board level are managed by a `board_options` object.
#' This can be constructed via `new_board_options()` and in case the set of
#' user options is to be extended, the constructor is designed with sub-classing
#' in mind. Consequently, the associated validator `validate_board_options()`
#' is available as S3 generic. Inheritance checking is available as
#' `is_board_options()` and coercion as `as_board_options()`.
#'
#' @param ... Options passed as individual arguments
#'
#' @examples
#' opt <- new_board_options(
#'    new_board_name_option(),
#'    new_n_rows_option()
#'  )
#'
#' is_board_options(opt)
#' names(opt)
#'
#' opt[["n_rows"]]
#'
#' @return All of `new_board_options()` and `as_board_options()` return a
#' `board_options` object, as does the validator `validate_board_options()`,
#' which is typically called for side effects of throwing errors is validation
#' does not pass. Inheritance checking as `is_board_options()` returns a scalar
#' logical, while `board_option_values()` returns a named list of option values.
#'
#' @export
new_board_options <- function(...) {

  res <- new_vctr(
    list_to_list_of_opts(list(...)),
    class = "board_options"
  )

  validate_board_options(res)

  res
}

#' @rdname new_board_options
#' @export
default_board_options <- function(...) {
  new_board_options(
    new_board_name_option(...),
    new_show_conditions_option(...),
    new_dark_mode_option(...)
  )
}

#' @param x Board options object
#' @rdname new_board_options
#' @export
is_board_options <- function(x) {
  inherits(x, "board_options")
}

#' @rdname new_board_options
#' @export
as_board_options <- function(x) {
  UseMethod("as_board_options", x)
}

#' @rdname new_board_options
#' @export
as_board_options.board_options <- function(x) {
  validate_board_options(x)
}

#' @rdname new_board_options
#' @export
as_board_options.board_option <- function(x) {
  new_board_options(x)
}

#' @rdname new_board_options
#' @export
as_board_options.list <- function(x) {
  do.call(new_board_options, x)
}

#' @rdname new_board_options
#' @export
as_board_options.board <- function(x) {
  board_options(x)
}

#' @rdname new_board_options
#' @export
validate_board_options <- function(x) {

  if (!is_board_options(x)) {
    blockr_abort(
      "Expecting board options to inherit from `board_options`.",
      class = "board_options_inheritance_invalid"
    )
  }

  for (opt in x) {
    validate_board_option(opt)
  }

  ids <- names(x)

  if (length(unique(ids)) != length(x)) {
    blockr_abort(
      "Non-unique board options IDs are being used.",
      class = "board_options_duplicated_ids"
    )
  }

  invisible(x)
}

#' @export
as.list.board_options <- function(x, ...) {

  res <- unclass(x)

  names(res) <- chr_ply(res, board_option_id)

  res
}

#' @export
names.board_options <- function(x) {
  chr_ply(x, board_option_id)
}

#' @rdname new_board_options
#' @export
board_option_values <- function(x) {

  x <- as_board_options(x)

  set_names(
    lapply(x, board_option_value),
    names(x)
  )
}

#' @export
format.board_options <- function(x, ...) {
  c(
    paste0("<", class(x)[1L], "[", length(x), "]>"),
    if (length(x)) paste0("  ", chr_ply(x, format))
  )
}

#' @export
print.board_options <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
str_value.board_options <- function(x, ...) {
  str_value_collection(x, chr_ply(x, str_value), ids = NULL)
}

#' @export
str.board_options <- function(object, ...) {
  cat_str_value(object, ...)
}

#' @export
c.board_options <- function(...) {
  as_board_options(list_to_list_of_opts(list(...)))
}

#' @export
`[.board_options` <- function(x, i, ...) {

  res <- opts_slice(x, vec_as_location(i, length(x), names(x)))

  if (!length(res)) {
    return(new_board_options())
  }

  res
}

#' @export
`[[.board_options` <- function(x, i, ...) {
  validate_board_option(
    .subset2(x, vec_as_location2(i, length(x), names(x)))
  )
}

opts_slice <- function(...) {
  validate_board_options(vec_slice(...))
}

harmonize_list_of_opts <- function(x) {
  if (is_board_option(x)) {
    list(x)
  } else if (is_board_options(x)) {
    as.list(x)
  } else if (is.list(x) && all(lgl_ply(x, is_board_option))) {
    x
  } else if (is.list(x) && any(lgl_ply(x, is_board_options))) {
    c(
      x[lgl_ply(x, Negate(is_board_options))],
      unlst(lapply(x[lgl_ply(x, is_board_options)], as.list))
    )
  } else {
    list(as_board_option(x))
  }
}

list_to_list_of_opts <- function(x) {

  x <- Filter(not_null, x)

  if (length(x)) {
    unlst(lapply(x, harmonize_list_of_opts))
  } else {
    list()
  }
}

#' @rdname new_board_options
#' @export
combine_board_options <- function(...) {

  opts <- list_to_list_of_opts(list(...))

  as_board_options(
    opts[!duplicated(chr_ply(opts, board_option_id))]
  )
}
