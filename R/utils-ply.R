#' Functional programming utilities
#'
#' A set of wrappers for [base::vapply()] with some convenient defaults.
#' Intended mainly for internal use, but available for dependent packages
#' wherever this is deemed convenient.
#'
#' @param x Object to iterate over
#' @param fun Function to apply to each component
#' @param ... Forwarded to `fun`
#' @param length Expected result length
#' @param use_names Name the result using `names(x)`
#'
#' @keywords internal
#'
#' @return The result of a call to [base::lapply()], [base::vapply()] or
#' [base::Map()].
#'
#' @export
chr_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, character(length), ..., USE.NAMES = use_names)
}

#' @rdname chr_ply
#' @export
lgl_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, logical(length), ..., USE.NAMES = use_names)
}

#' @rdname chr_ply
#' @export
int_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, integer(length), ..., USE.NAMES = use_names)
}

#' @rdname chr_ply
#' @export
dbl_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, double(length), ..., USE.NAMES = use_names)
}

#' @rdname chr_ply
#' @export
chr_mply <- function(..., length = 1L) {
  chr_ply(map(...), identity, length = length)
}

#' @rdname chr_ply
#' @export
lgl_mply <- function(..., length = 1L) {
  lgl_ply(map(...), identity, length = length)
}

#' @rdname chr_ply
#' @export
int_mply <- function(..., length = 1L) {
  int_ply(map(...), identity, length = length)
}

#' @rdname chr_ply
#' @export
dbl_mply <- function(..., length = 1L) {
  dbl_ply(map(...), identity, length = length)
}

#' @param i Index to extract
#' @rdname chr_ply
#' @export
chr_xtr <- function(x, i, ...) chr_ply(x, `[[`, i, ...)

#' @rdname chr_ply
#' @export
lgl_xtr <- function(x, i, ...) lgl_ply(x, `[[`, i, ...)

#' @rdname chr_ply
#' @export
int_xtr <- function(x, i, ...) int_ply(x, `[[`, i, ...)

#' @rdname chr_ply
#' @export
dbl_xtr <- function(x, i, ...) dbl_ply(x, `[[`, i, ...)

#' @rdname chr_ply
#' @export
lst_xtr <- function(x, ...) {

  for (i in c(...)) {
    x <- lapply(x, `[[`, i)
  }

  x
}

#' @rdname chr_ply
#' @export
map <- function(fun, ..., use_names = FALSE) {
  Map(fun, ..., USE.NAMES = use_names)
}

#' @rdname chr_ply
#' @export
filter <- function(fun, x, ...) {
  x[lgl_ply(x, match.fun(fun), ..., length = 1L, use_names = FALSE)]
}
