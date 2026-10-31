#' @param id Board option ID
#' @param default Default value
#' @param transform (Optional) transform function
#' @param category (Optional) string-valued category
#' @param label (Optional) string-valued label, the name a UI can show the
#'   option by, as in a list of options. If `NULL`, the ID in sentence case,
#'   such as "Data dir" for `data_dir`.
#' @param ctor,pkg Constructor information (used for serialization)

#' @rdname new_board_options
#' @export
new_board_option <- function(id, default, transform = identity,
                             category = NULL, label = NULL,
                             ctor = sys.parent(), pkg = NULL) {

  res <- structure(
    list(transform = transform),
    default = default,
    id = id,
    category = category,
    label = coal(label, id_to_sentence_case(id)),
    ctor = resolve_ctor(ctor, pkg),
    class = c(paste0(id, "_option"), "board_option")
  )

  validate_board_option(res)

  res
}

#' @rdname new_board_options
#' @export
is_board_option <- function(x) {
  inherits(x, "board_option")
}

#' @rdname new_board_options
#' @export
validate_board_option <- function(x) {
  UseMethod("validate_board_option", x)
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
board_option_default <- function(x) {
  stopifnot(is_board_option(x))
  attr(x, "default")
}

#' @rdname new_board_options
#' @export
board_option_category <- function(x) {
  stopifnot(is_board_option(x))
  attr(x, "category")
}

#' @rdname new_board_options
#' @export
board_option_label <- function(x) {
  stopifnot(is_board_option(x))
  attr(x, "label")
}

#' @rdname new_board_options
#' @export
board_option_transform <- function(x) {
  stopifnot(is_board_option(x))
  x[["transform"]]
}

#' @param value Option value
#' @rdname new_board_options
#' @export
board_option_value <- function(x, value = board_option_default(x)) {
  trans <- board_option_transform(x)
  trans(value)
}

#' @rdname new_board_options
#' @export
board_option_ctor <- function(x) {
  stopifnot(is_board_option(x))
  attr(x, "ctor")
}

#' @rdname new_board_options
#' @export
validate_board_option.default <- function(x) {

  if (!is_board_option(x)) {
    blockr_abort(
      "Expecting a board option to inherit from `board_option`.",
      class = "board_option_inheritance_invalid"
    )
  }

  if (!is_string(board_option_id(x))) {
    blockr_abort(
      "Expecting a board option ID to be string-valued.",
      class = "board_option_component_id_invalid"
    )
  }

  tfm <- board_option_transform(x)

  if (!is.function(tfm) || !identical(names(formals(tfm)), "x")) {
    blockr_abort(
      "Expecting a board option transform to have a single argument `x`.",
      class = "board_option_component_transform_invalid"
    )
  }

  if (!is_string(board_option_label(x))) {
    blockr_abort(
      "Expecting a board option label to be string-valued.",
      class = "board_option_component_label_invalid"
    )
  }

  invisible(x)
}

#' @export
format.board_option <- function(x, ...) {
  paste0(board_option_id(x), ": ", format_head(board_option_value(x), 5L))
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
new_n_rows_option <- function(value = blockr_option("n_rows", 50L),
                              category = "Table options",
                              label = "Preview rows", ...) {

  new_board_option(
    id = "n_rows",
    default = value,
    transform = function(x) as.integer(x),
    category = category,
    label = label,
    ...
  )
}

#' @export
validate_board_option.n_rows_option <- function(x) {

  val <- board_option_value(NextMethod())

  if (!is_count(val)) {
    blockr_abort(
      "Expecting `n_rows` to represent a count.",
      class = "board_options_n_rows_invalid"
    )
  }

  invisible(x)
}

#' @rdname new_board_options
#' @export
new_dark_mode_option <- function(value = blockr_option("dark_mode", NULL),
                                 category = "Theme options",
                                 label = "Light/dark mode", ...) {

  if (isTRUE(value)) {
    value <- "dark"
  }

  if (isFALSE(value)) {
    value <- "light"
  }

  if (length(value) && is.na(value)) {
    value <- NULL
  }

  new_board_option(
    id = "dark_mode",
    default = value,
    category = category,
    label = label,
    ...
  )
}

#' @export
validate_board_option.dark_mode_option <- function(x) {

  val <- board_option_value(NextMethod())

  if (!(is.null(val) || (is_string(val) && val %in% c("light", "dark")))) {
    blockr_abort(
      "Expecting `dark_mode` to be either `NULL`, \"light\" or \"dark\".",
      class = "board_options_dark_mode_invalid"
    )
  }

  invisible(x)
}

#' @rdname new_board_options
#' @export
new_show_conditions_option <- function(value = blockr_option("show_conditions",
                                                             c("warning",
                                                               "error")),
                                       category = "Board options",
                                       label = "Message types to display",
                                       ...) {

  new_board_option(
    id = "show_conditions",
    default = value,
    category = category,
    label = label,
    ...
  )
}

#' @export
validate_board_option.show_conditions_option <- function(x) {

  val <- board_option_value(NextMethod())

  opt <- c("message", "warning", "error")

  if (!(is.character(val) || all(val %in% opt))) {
    blockr_abort(
      "Expecting `show_conditions` to be any of {opt}.",
      class = "board_options_show_conditions_invalid"
    )
  }

  invisible(x)
}
