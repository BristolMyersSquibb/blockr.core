new_condition <- function(x, as_list = TRUE) {

  if (inherits(x, "condition")) {
    x <- fmt_cnd_msg(x)
  }

  if (inherits(x, "block_cnd")) {

    res <- x

  } else {

    res <- structure(
      x,
      id = digest::digest(x, "xxh3_64", serialize = FALSE),
      class = "block_cnd"
    )
  }

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

msg_handler <- function(cond, conds) {
  stopifnot(is.environment(cond), is.character(conds))
  function(m) {
    if ("message" %in% conds) {
      cond$message <- c(cond$message, new_condition(m))
    }
    log_info(fmt_cnd_msg(m))
    tryInvokeRestart("muffleMessage")
  }
}

warn_handler <- function(cond, conds) {
  stopifnot(is.environment(cond), is.character(conds))
  function(w) {
    if ("warning" %in% conds) {
      cond$warning <- c(cond$warning, new_condition(w))
    }
    log_warn(fmt_cnd_msg(w))
    tryInvokeRestart("muffleWarning")
  }
}

err_handler <- function(cond, conds, err_val = NULL) {
  stopifnot(is.environment(cond), is.character(conds))
  function(e) {
    if ("error" %in% conds) {
      cond$error <- new_condition(e)
    }
    log_error(fmt_cnd_msg(e))
    err_val
  }
}

capture_conditions <- function(expr, rv, slot, error_val = NULL,
                               session = get_session()) {

  stopifnot(
    is.reactivevalues(rv),
    is_string(slot), slot %in% names(rv)
  )

  cond <- list2env(empty_block_condition())

  cnds <- coal(
    isolate(
      get_board_option_or_null("show_conditions", session)
    ),
    blockr_option("show_conditions", c("warning", "error"))
  )

  res <- tryCatch(
    withCallingHandlers(
      expr,
      message = msg_handler(cond, cnds),
      warning = warn_handler(cond, cnds)
    ),
    error = err_handler(cond, cnds, error_val)
  )

  cond <- as.list(cond)
  curr <- rv[[slot]]

  if (all(lengths(cond) == 0L) && !length(curr)) {
    return(res)
  }

  if (length(curr)) {

    chk <- lgl_mply(
      setequal,
      lapply(cond, chr_ply, attr, "id"),
      lapply(curr, chr_ply, attr, "id")
    )

    if (all(chk)) {
      return(res)
    }
  }

  rv[[slot]] <- cond

  res
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
