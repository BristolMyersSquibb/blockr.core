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

# A getter checks only that it reads a board option. The function
# `validate_board_option()` checks a board option in full.
check_board_option <- function(x) {

  if (!is_board_option(x)) {
    blockr_abort(
      "Expecting a board option to inherit from `board_option`.",
      class = "board_option_inheritance_invalid"
    )
  }

  invisible(x)
}

validate_board_option <- function(x) {

  check_board_option(x)

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
  check_board_option(x)
  attr(x, "id")
}

#' @rdname new_board_options
#' @export
board_option_value <- function(x) {
  check_board_option(x)
  x[["value"]]
}

has_board_option_value <- function(x) {
  check_board_option(x)
  "value" %in% names(x)
}

#' Board option versions
#'
#' The version of a board option is the version of the shape of its value, a
#' whole number from 1.
#' The package that defines a board option gives it through its method of
#' `board_option_version()` for the board option's class, which is its ID
#' followed by `_option`, as `n_rows_option`.
#' A board option without such a method is at version 1.
#' The package raises the version as it changes that shape, so that the client
#' migrates the values that a JSON board holds of an older version.
#' R sends the version of each board option that a method gives one in its
#' reply to `hello`, and writes each value of a board with its version (see
#' [blockr_ser()]).
#'
#' @param x Board option
#' @param ... Generic consistency
#'
#' @examples
#' board_option_version(new_n_rows_option(25L))
#'
#' @return A whole number.
#'
#' @export
board_option_version <- function(x, ...) {
  UseMethod("board_option_version")
}

#' @rdname board_option_version
#' @export
board_option_version.board_option <- function(x, ...) {
  1L
}

# The version of the board option with the ID `id`, as the method of
# `board_option_version()` for its class gives it.
option_version <- function(id) {

  version <- board_option_version(new_board_option(id))

  if (!is_count(version, allow_zero = FALSE)) {
    blockr_abort(
      "The method of `board_option_version()` for board option {id} returns ",
      "no whole number of 1 or more.",
      class = "board_option_version_invalid"
    )
  }

  version
}

# The version of each board option that a method of `board_option_version()`
# gives one, by ID, as R sends them in its reply to `hello`.
option_versions <- function() {

  methods <- utils::.S3methods(
    "board_option_version",
    envir = asNamespace("blockr.core")
  )

  classes <- sub("^board_option_version[.]", "", as.character(methods))
  classes <- grep("_option$", setdiff(classes, "board_option"), value = TRUE)
  ids <- sub("_option$", "", classes)

  as_json_object(set_names(lapply(ids, option_version), ids))
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
