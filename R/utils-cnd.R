new_condition <- function(x, ..., as_list = TRUE) {

  id <- get_globals(...) + 1L
  set_globals(id, ...)

  if (inherits(x, "condition")) {
    x <- fmt_cnd_msg(x)
  }

  res <- structure(x, id = id, class = "block_cnd")

  if (!isTRUE(as_list)) {
    return(res)
  }

  list(res)
}

empty_block_condition <- list(
  error = character(),
  warning = character(),
  message = character()
)

fmt_cnd_msg <- function(x) {
  sub("\n$", "", conditionMessage(x))
}

show_condition <- function(x) {
  UseMethod("show_condition")
}

#' @noRd
#' @export
show_condition.list <- function(x) {
  invisible(lapply(x, show_condition))
}

#' @noRd
#' @export
show_condition.default <- function(x) {
  invisible(NULL)
}

#' @noRd
#' @export
show_condition.message <- function(x) {
  message(fmt_cnd_msg(x))
}

#' @noRd
#' @export
show_condition.warning <- function(x) {
  warning(fmt_cnd_msg(x))
}

#' @noRd
#' @export
show_condition.error <- function(x) {
  stop(fmt_cnd_msg(x))
}
