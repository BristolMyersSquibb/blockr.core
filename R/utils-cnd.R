new_blk_cnd <- function(x) {
  structure(
    x,
    id = digest::digest(x, "xxh3_64", serialize = FALSE),
    class = "blk_cnd"
  )
}

as_blk_cnd <- function(x) {
  UseMethod("as_blk_cnd")
}

#' @noRd
#' @export
as_blk_cnd.blk_cnd <- function(x) {
  x
}

#' @noRd
#' @export
as_blk_cnd.condition <- function(x) {
  new_blk_cnd(fmt_cnd_msg(x))
}

#' @noRd
#' @export
as_blk_cnd.character <- function(x) {
  new_blk_cnd(x)
}

blk_cnds <- function(x, block = NA_character_) {

  phases <- names(x)

  rows <- lapply(
    c("error", "warning", "message"),
    function(sev) {

      msgs <- lst_xtr(x[phases], sev)
      lens <- lengths(msgs)

      if (!any(lens)) {
        return(NULL)
      }

      flat <- unlst(msgs)

      data.frame(
        phase = rep(phases, lens),
        severity = sev,
        message = chr_ply(flat, cnd_message),
        id = chr_ply(flat, cnd_id),
        row.names = NULL
      )
    }
  )

  out <- do.call(rbind, rows)

  if (is.null(out)) {
    out <- data.frame(
      phase = character(),
      severity = character(),
      message = character(),
      id = character()
    )
  }

  res <- cbind(block = rep(block, nrow(out)), out)
  row.names(res) <- NULL

  res
}

is_blk_cnd <- function(x) {
  inherits(x, "blk_cnd")
}

cnd_id <- function(x) {
  attr(x, "id")
}

cnd_message <- function(x) {
  as.character(x)
}

empty_conditions_frame <- function() {
  blk_cnds(list())
}

is_list_of_blk_cnds <- function(x) {
  is.list(x) && all(lgl_ply(x, is_blk_cnd))
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

msg_handler <- function(cond, conds) {
  stopifnot(is.environment(cond), is.character(conds))
  function(m) {
    if ("message" %in% conds) {
      cond$message <- c(cond$message, list(as_blk_cnd(m)))
    }
    log_info(fmt_cnd_msg(m), use_glue = FALSE)
    tryInvokeRestart("muffleMessage")
  }
}

warn_handler <- function(cond, conds) {
  stopifnot(is.environment(cond), is.character(conds))
  function(w) {
    if ("warning" %in% conds) {
      cond$warning <- c(cond$warning, list(as_blk_cnd(w)))
    }
    log_warn(fmt_cnd_msg(w), use_glue = FALSE)
    tryInvokeRestart("muffleWarning")
  }
}

err_handler <- function(cond, conds, err_val = NULL) {
  stopifnot(is.environment(cond), is.character(conds))
  function(e) {

    msg <- fmt_cnd_msg(e)

    # Silent flow-control throws (req()) carry an empty message; recording one
    # as an error paints a text-less red band. Gate on emptiness, not class:
    # validate(need(x, "msg")) is also a shiny.silent.error and must surface.
    if (all(!nzchar(msg))) {
      return(err_val)
    }

    if ("error" %in% conds) {
      cond$error <- list(as_blk_cnd(e))
    }

    log_error(msg, use_glue = FALSE)

    err_val
  }
}

capture_conditions <- function(expr, rv, slot, error_val = NULL,
                               session = get_session()) {

  stopifnot(
    is.reactivevalues(rv),
    is_string(slot), slot %in% names(rv)
  )

  cnds <- coal(
    isolate(
      get_board_option_or_null("show_conditions", session)
    ),
    match.arg(
      blockr_option("show_conditions", c("warning", "error")),
      c("message", "warning", "error"),
      several.ok = TRUE
    )
  )

  cond <- list2env(empty_block_condition()[cnds])

  res <- try_catch_continue(
    expr,
    message = msg_handler(cond, cnds),
    warning = warn_handler(cond, cnds),
    error = err_handler(cond, cnds, error_val)
  )

  cond <- as.list(cond)
  curr <- set_names(
    coal(rv[[slot]], empty_block_condition())[cnds],
    cnds
  )

  if (any(lengths(cond))) {

    chk <- lgl_mply(
      Negate(setequal),
      lapply(cond, chr_ply, attr, "id"),
      lapply(curr, chr_ply, attr, "id")
    )

    if (any(chk)) {
      rv[[slot]] <- cond
    }

  } else if (any(lengths(curr) > 0L)) {
    rv[[slot]] <- empty_block_condition()[cnds]
  }

  res
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
