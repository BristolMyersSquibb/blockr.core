#' @param id Board option ID, which says which board option it is, such as
#'   `"n_rows"`
#' @param value The board option's value, which a board holds under its ID.
#'   A board option made without one has none.

#' @rdname new_board_options
#' @export
new_board_option <- function(id, value) {

  if (!is_nonempty_string(id)) {
    blockr_abort(
      "Expecting a board option ID to be a non-empty string.",
      class = "board_option_component_id_invalid"
    )
  }

  structure(
    if (missing(value)) list() else list(value = value),
    id = id,
    class = c(paste0(id, "_option"), "board_option")
  )
}

#' @rdname new_board_options
#' @export
is_board_option <- function(x) {
  inherits(x, "board_option")
}

validate_board_option <- function(x) {

  if (!is_board_option(x)) {
    blockr_abort(
      "Expecting a board option to inherit from `board_option`.",
      class = "board_option_inheritance_invalid"
    )
  }

  if (!is_nonempty_string(board_option_id(x))) {
    blockr_abort(
      "Expecting a board option ID to be a non-empty string.",
      class = "board_option_component_id_invalid"
    )
  }

  invisible(x)
}

#' @rdname new_board_options
#' @export
as_board_option <- function(x, ...) {
  UseMethod("as_board_option", x)
}

#' @rdname new_board_options
#' @export
as_board_option.board_option <- function(x, ...) {
  x
}

#' @rdname new_board_options
#' @export
board_option_id <- function(x) {
  stopifnot(is_board_option(x))
  attr(x, "id")
}

#' @rdname new_board_options
#' @export
board_option_value <- function(x) {
  stopifnot(is_board_option(x))
  x[["value"]]
}

has_board_option_value <- function(x) {
  stopifnot(is_board_option(x))
  "value" %in% names(x)
}

#' @export
format.board_option <- function(x, ...) {

  if (has_board_option_value(x)) {
    val <- format_head(board_option_value(x), 5L)
  } else {
    val <- "<unset>"
  }

  paste0(board_option_id(x), ": ", val)
}

#' @export
print.board_option <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
str_value.board_option <- function(x, ...) {
  format(x, ...)
}

#' @export
str.board_option <- function(object, ...) {
  cat_str_value(object, ...)
}

#' @export
c.board_option <- function(...) {
  as_board_options(list_to_list_of_opts(list(...)))
}

#' @rdname new_board_options
#' @export
new_n_rows_option <- function(value) {
  new_board_option("n_rows", value)
}

#' @rdname new_board_options
#' @export
new_dark_mode_option <- function(value) {
  new_board_option("dark_mode", value)
}

#' @rdname new_board_options
#' @export
new_show_conditions_option <- function(value) {
  new_board_option("show_conditions", value)
}
