#' Locked boards
#'
#' A board can be deployed read-only by setting the `blockr.locked`
#' option (see [blockr_option()]). Locking has to be enforced
#' server-side, not by UI hiding: a forged `Shiny.setInputValue`
#' bypasses any `display: none`, so the refusal has to happen on the
#' server.
#'
#' The generic `is_board_locked()` reports whether a board is locked.
#' Its method for `board`, the class of every board, reads the
#' `blockr.locked` option, and `blockr.dock`'s `is_dock_locked()` delegates
#' here.
#'
#' @section Scope: Locking guards against forged *client input* -- a
#' browser that sets Shiny inputs but cannot run server-side R. It is
#' **not** a defense against code executing in the session: blockr
#' evaluates arbitrary R, which can flip the option, rebind this generic
#' or mutate board state directly, and no in-process flag can prevent
#' that. A hard boundary has to live where the user's code does not run
#' (a sandboxed evaluator, or an ephemeral read-only deployment). Treat
#' locking as a guard for non-malicious users.
#'
#' @param board A `board` object to dispatch on.
#' @param ... Generic consistency.
#'
#' @return A logical flag.
#'
#' @examples
#' is_board_locked(new_board())
#'
#' @name locked-board
#' @export
is_board_locked <- function(board, ...) {
  UseMethod("is_board_locked", board)
}

#' @rdname locked-board
#' @export
is_board_locked.board <- function(board, ...) {
  blockr_flag("locked", FALSE)
}
