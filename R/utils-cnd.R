new_condition <- function(x, as_list = TRUE) {

  if (inherits(x, "condition")) {
    x <- fmt_cnd_msg(x)
  }

  res <- structure(
    x,
    id = digest::digest(x, "xxh3_64", serialize = FALSE),
    class = "block_cnd"
  )

  if (!isTRUE(as_list)) {
    return(res)
  }

  list(res)
}

empty_block_condition <- function() {
  list(
    error = list(),
    warning = list(),
    message = list()
  )
}

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

#' @importFrom evaluate replay
#' @export
replay.message <- function(x) {
  log_info(fmt_cnd_msg(x))
}

#' @export
replay.warning <- function(x) {
  log_warn(fmt_cnd_msg(x))
}

#' @export
replay.error <- function(x) {
  log_error(fmt_cnd_msg(x))
}

msg_handler <- function(cond) {
  stopifnot(is.environment(cond))
  function(m) {
    cond$message <- c(cond$message, new_condition(m))
    log_info(fmt_cnd_msg(m))
    tryInvokeRestart("muffleMessage")
  }
}

warn_handler <- function(cond) {
  stopifnot(is.environment(cond))
  function(w) {
    cond$warning <- c(cond$warning, new_condition(w))
    log_warn(fmt_cnd_msg(w))
    tryInvokeRestart("muffleWarning")
  }
}

err_handler <- function(cond, err_val = NULL) {
  stopifnot(is.environment(cond))
  function(e) {
    cond$error <- new_condition(e)
    log_error(fmt_cnd_msg(e))
    err_val
  }
}

capture_conditions <- function(expr, rv, slot, error_val = NULL) {

  stopifnot(
    is.reactivevalues(rv),
    is_string(slot), slot %in% names(rv)
  )

  cond <- list2env(empty_block_condition())

  res <- tryCatch(
    withCallingHandlers(
      expr,
      message = msg_handler(cond),
      warning = warn_handler(cond)
    ),
    error = err_handler(cond, error_val)
  )

  rv[[slot]] <- as.list(cond)

  res
}
