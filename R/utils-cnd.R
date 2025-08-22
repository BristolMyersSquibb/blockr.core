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

#' @importFrom evaluate replay
#' @export
replay.message <- function(x) {
  message(fmt_cnd_msg(x))
}

#' @export
replay.warning <- function(x) {
  warning(fmt_cnd_msg(x))
}

#' @export
replay.error <- function(x) {
  stop(fmt_cnd_msg(x))
}
