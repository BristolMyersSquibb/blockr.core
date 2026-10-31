#' Assertions
#'
#' Utility functions, mainly intended for asserting common preconditions are
#' exported for convenience in dependent packages.
#'
#' @param x Object to check
#'
#' @return Scalar logical value.
#'
#' @rdname assertions
#' @export
is_scalar <- function(x) length(x) == 1L

#' @rdname assertions
#' @export
is_string <- function(x) {
  is.character(x) && is_scalar(x)
}

#' @rdname assertions
#' @export
is_bool <- function(x) {
  is_scalar(x) && (isTRUE(x) || isFALSE(x))
}

#' @rdname assertions
#' @export
is_intish <- function(x) {
  is.integer(x) || (is.numeric(x) && all(x == trunc(x)) && !is.na(x))
}

#' @param allow_zero Determines whether the value 0 is considered a valid count
#' @rdname assertions
#' @export
is_count <- function(x, allow_zero = TRUE) {

  if (!is_scalar(x)) {
    return(FALSE)
  }

  if (!is_intish(x)) {
    return(FALSE)
  }

  if (isTRUE(allow_zero)) {
    x >= 0 && !is.na(x)
  } else {
    x > 0 && !is.na(x)
  }
}

#' @rdname assertions
#' @export
is_number <- function(x) {
  is.numeric(x) && is_scalar(x) && !is.na(x) && !is.nan(x) && is.finite(x)
}

#' @param ... Silently ignored
#' @rdname assertions
#' @export
not_null <- Negate(is.null)

#' @rdname assertions
#' @export
has_length <- function(x) length(x) > 0

is_zero_len <- function(x) {
  length(x) == 0L
}

all_zero_len <- function(x) {
  if (is.list(x)) {
    all(lgl_ply(x, all_zero_len))
  } else {
    is_zero_len(x)
  }
}

is_empty <- function(x) {
  is_zero_len(x) || all(is.na(x) | !nchar(x))
}

filter_all_zero_len <- function(x) {
  if (all_zero_len(x)) {
    NULL
  } else if (is.list(x)) {
    Filter(Negate(is.null), lapply(x, filter_all_zero_len))
  } else {
    x
  }
}

filter_empty <- function(x) Filter(Negate(is_empty), x)

# The IDs of a board, its blocks, links and stacks may be neither empty nor made
# of digits alone, since a JS object puts keys that look like integers first,
# whatever the order they were inserted in.
validate_ids <- function(ids, what, class) {

  bad <- is.na(ids) | !nzchar(ids) | grepl("^[0-9]+$", ids)

  if (any(bad)) {

    ids <- encodeString(ids[bad], quote = "\"")

    blockr_abort(
      "{what} IDs are required to be strings that are neither empty nor ",
      "made of digits alone, which does not hold for {ids}.",
      class = class
    )
  }

  invisible()
}
