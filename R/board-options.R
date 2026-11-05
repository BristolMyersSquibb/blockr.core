#' Board options
#'
#' A board option is an option that the user sees and changes while the app
#' runs, such as `n_rows`, the rows of a table output's page. The package that
#' ships a board option defines it in the client, with its label, category,
#' schema, UI and default. A board holds only its value, under its ID, once
#' something sets one: the app script or, while the app runs, a command.
#'
#' In R, a board option is its ID and the value that a script gives it, which
#' `new_board_option()` makes. The constructors of core's board options, such
#' as `new_n_rows_option(25L)`, make one each and compute nothing, neither a
#' default nor a check. A board option made without a value has none. The board
#' then holds no value for it, so that whatever needs it takes the default of
#' its definition in the client.
#'
#' Core ships three board options: `n_rows`, the rows of a table output's page,
#' `show_conditions`, the severities of a block's conditions that a front-end
#' shows, and `dark_mode`, which switches between the light and the dark theme.
#' Their schemas ship beside core's runtime, and R writes the value that a
#' script gives by them (see [blockr_ser()]).
#'
#' Each of core's board options takes its default from the blockr option of
#' its name, which the client asks R for the first time that something on the
#' page requires the board option (see [blockr_option()]).
#' So `options(blockr.n_rows = 25L)`, or the environment variable
#' `BLOCKR_N_ROWS`, still sets the rows of a page on a board that holds no
#' value for `n_rows`.
#' Without a blockr option, `n_rows` takes 50, `show_conditions` warnings and
#' errors, and `dark_mode` the theme that the user's system prefers.
#'
#' Several board options make a `board_options` object, which
#' `new_board_options()` makes, holding one board option per ID. The predicate
#' `is_board_options()` tests for one, `as_board_options()` coerces to one and
#' `validate_board_options()` checks one. The values that it holds, by ID, are
#' given by `board_option_values()`, and `combine_board_options()` combines
#' sets of board options, keeping the first for each ID.
#'
#' @param ... Board options
#'
#' @examples
#' opts <- new_board_options(
#'   new_n_rows_option(25L),
#'   new_show_conditions_option("error"),
#'   new_dark_mode_option()
#' )
#'
#' is_board_options(opts)
#' names(opts)
#'
#' opts[["n_rows"]]
#' board_option_values(opts)
#'
#' @return Both `new_board_option()` and the constructors of core's board
#' options return a `board_option` object, while `new_board_options()`,
#' `as_board_options()` and `combine_board_options()` return a `board_options`
#' object, as does the validator `validate_board_options()`, which is called
#' for the errors it throws. The predicates `is_board_option()` and
#' `is_board_options()` return a scalar logical, `board_option_id()` returns a
#' string, `board_option_value()` the value, or `NULL` where there is none, and
#' `board_option_values()` a named list of the values that a set of board
#' options holds, by ID.
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

  x <- as.list(as_board_options(x))
  x <- x[lgl_ply(x, has_board_option_value)]

  set_names(lapply(x, board_option_value), names(x))
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
