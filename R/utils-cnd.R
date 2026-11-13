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
#' addition to `class`, conditions inherit from "blockr_error",
#' "blockr_warning" or "blockr_message".
#'
#' Two more wrappers stop a block without failing it.
#' Each signals an error that also inherits from "blockr_invalid" or
#' "blockr_waiting", and R then runs nothing more of the block and reports it
#' with the status of that name.
#' R records neither error as a condition of the evaluation,
#' but keeps its message as the block's reason (see [evaluate_block()]).
#'
#' - The signal `blockr_invalid()` says that a block's state cannot run yet,
#'   such as a selection the user emptied.
#' - The signal `blockr_waiting()` says that a block's state can run, but that
#'   the block runs only once something else happens.
#'   An example is the write block of blockr.io in manual mode:
#'   it writes only once its save button passes `save` to its `block_eval()`
#'   method.
#'
#' A block class signals either from its `expr` or a [block_eval()] method,
#' while the check of a function given as its `inputs` signals an error with
#' `blockr_abort()` (see [new_block_class()]).
#'
#' @param ... Forwarded to [cli::pluralize()]
#' @param class Condition class
#' @param envir Forwarded to [cli::pluralize()]
#'
#' @return Called for side-effect of signaling conditions.
#'
#' @examples
#' merge_block <- new_block_class(
#'   "merge_block",
#'   inputs = function(x, y) {
#'     if (!is.data.frame(x) || !is.data.frame(y)) {
#'       blockr_abort("The merge block joins two data frames.",
#'                    class = "merge_block_inputs_invalid")
#'     }
#'   },
#'   expr = function(by) {
#'     if (!length(by)) {
#'       blockr_invalid("Choose at least one column to join by.")
#'     }
#'     bbquote(merge(.(x), .(y), by = .(by)))
#'   },
#'   ui = js_module("merge-block.js")
#' )
#'
#' evaluate_block(merge_block, list(by = character()), list(x = "a", y = "b"),
#'                list(a = datasets::BOD, b = datasets::BOD))
#'
#' evaluate_block(merge_block, list(by = "Time"), list(x = "a", y = "b"),
#'                list(a = datasets::BOD, b = 1:3))
#'
#' @export
blockr_abort <- function(..., class = character(), envir = parent.frame()) {
  rlang::abort(glue_plur(..., envir = envir), c(class, "blockr_error"))
}

#' @rdname blockr_abort
#' @export
blockr_invalid <- function(..., class = character(), envir = parent.frame()) {
  rlang::abort(
    glue_plur(..., envir = envir),
    c(class, "blockr_invalid", "blockr_error")
  )
}

#' @rdname blockr_abort
#' @export
blockr_waiting <- function(..., class = character(), envir = parent.frame()) {
  rlang::abort(
    glue_plur(..., envir = envir),
    c(class, "blockr_waiting", "blockr_error")
  )
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
