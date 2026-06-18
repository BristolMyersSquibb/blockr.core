#' Locked boards
#'
#' A board can be deployed read-only by setting the `blockr.locked`
#' option (see [blockr_option()]). Locking is enforced server-side, not
#' by UI hiding: a forged `Shiny.setInputValue` bypasses any
#' `display: none` and fires the underlying observer, so the refusal has
#' to happen on the server. While locked, the two channels every
#' mutation funnels through -- the `board_update` lifecycle (see
#' [board_update()]) and [set_board_option_value()] -- refuse to apply
#' changes.
#'
#' `is_board_locked()` reports whether a board is locked and dispatches
#' on the board, so a subclass can source the state however it likes
#' (for example gating an unlock behind an authenticated request). The
#' default `board` method reads the `blockr.locked` option, and
#' `blockr.dock`'s `is_dock_locked()` delegates here.
#'
#' @section Scope: Locking guards against forged *client input* -- a
#' browser that sets Shiny inputs but cannot run server-side R. It is
#' **not** a defense against code executing in the session: blockr
#' evaluates arbitrary R, which can flip the option, rebind this generic
#' or mutate board state directly, and no in-process flag can prevent
#' that. A subclass method may raise the bar (e.g. an authenticated
#' unlock) but cannot close the gap on its own -- a hard boundary has to
#' live where the user's code does not run (a sandboxed evaluator, or an
#' ephemeral read-only deployment). Treat locking as a guard for
#' non-malicious users.
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
  isTRUE(blockr_option("locked", FALSE))
}
