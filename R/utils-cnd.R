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
  log_info(fmt_cnd_msg(x), use_glue = FALSE)
}

#' @export
replay.warning <- function(x) {
  log_warn(fmt_cnd_msg(x), use_glue = FALSE)
}

#' @export
replay.error <- function(x) {
  log_error(fmt_cnd_msg(x), use_glue = FALSE)
}

try_catch_continue <- function(expr, message, warning, error) {
  tryCatch(
    withCallingHandlers(
      expr,
      message = message,
      warning = warning
    ),
    error = error
  )
}

glue_plur <- function(..., envir = parent.frame()) {
  cli::pluralize(..., .envir = envir)
}

#' Blockr conditions
#'
#' Wrappers for [rlang::abort()], [rlang::warn()] and [rlang::inform()]. In
#' addition to `class`, conditions inherit from "blockr_error".
#'
#' @param ... Forwarded to [cli::pluralize()]
#' @param class Condition class
#' @param envir Forwarded to [cli::pluralize()]
#'
#' @return Called for side-effect of signaling conditions.
#'
#' @export
blockr_abort <- function(..., class = character(), envir = parent.frame()) {
  rlang::abort(glue_plur(..., envir = envir), c(class, "blockr_error"))
}

#' @param frequency,frequency_id Forwarded to [rlang::warn()]
#' @rdname blockr_abort
#' @export
blockr_warn <- function(..., class = character(), envir = parent.frame(),
                        frequency = "always", frequency_id = NULL) {

  rlang::warn(
    glue_plur(..., envir = envir),
    c(class, "blockr_warning"),
    .frequency = frequency,
    .frequency_id = frequency_id
  )
}

#' @rdname blockr_abort
#' @export
blockr_inform <- function(..., class = character(), envir = parent.frame(),
                          frequency = "always", frequency_id = NULL) {

  rlang::inform(
    glue_plur(..., envir = envir),
    c(class, "blockr_message"),
    .frequency = frequency,
    .frequency_id = frequency_id
  )
}
