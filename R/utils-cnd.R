new_condition <- function(x, ..., as_list = TRUE) {

  id <- coal(get_globals(...), 0L) + 1L
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

msg_handler <- function(cond, sess) {
  function(m) {
    cond$message <- c(cond$message, new_condition(m, session = sess))
    log_info(fmt_cnd_msg(m))
    tryInvokeRestart("muffleMessage")
  }
}

warn_handler <- function(cond, sess) {
  function(w) {
    cond$warning <- c(cond$warning, new_condition(w, session = sess))
    log_warn(fmt_cnd_msg(w))
    tryInvokeRestart("muffleWarning")
  }
}

err_handler <- function(cond, sess, err_val = NULL) {
  function(e) {
    cond$error <- new_condition(e, session = sess)
    log_error(fmt_cnd_msg(e))
    err_val
  }
}

capture_conditions <- function(expr, cond, session = get_session(),
                               error_val = NULL) {
  tryCatch(
    withCallingHandlers(
      expr,
      message = msg_handler(cond, session),
      warning = warn_handler(cond, session)
    ),
    error = err_handler(cond, session, error_val)
  )
}
