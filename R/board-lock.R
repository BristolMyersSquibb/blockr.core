#' Locked boards
#'
#' An app sets three locks, each on its own.
#'
#' * The load lock, the blockr option `lock_load`, fixes the app to its opening
#'   board, the one that the app script built or that `serve()` was given.
#'   The client's store then opens no other board,
#'   and a frontend draws no control for opening one.
#' * The board lock, the blockr option `locked`, fixes what every board that
#'   the app shows holds: its blocks and links, its name, every block's name
#'   and lock, and its stacks.
#'   A reader may still change a board option, such as dark mode,
#'   the state of a block that the board lock leaves open,
#'   and what sits inside a plugin's state, which each plugin locks itself.
#' * A block's lock, its `locked`, fixes the block's state where the board lock
#'   is on.
#'   A lock of `TRUE` locks the block, `FALSE` leaves it open to readers,
#'   and `NA` follows the board lock, which then locks it.
#'   A block holds `NA` unless a script gives it another lock,
#'   and a JSON board holds `NA` as `null`.
#'
#' An app sets a blockr option as an R option, such as `blockr.locked`,
#' and a deployment as an environment variable, such as `BLOCKR_LOCKED`
#' (see [blockr_option()]).
#' Each lock is off unless one of them sets it.
#' So an author leaves a block open to readers by giving it
#' `locked = FALSE` in an app without the board lock,
#' saves the board and serves it with the board lock on.
#'
#' The client's store refuses what the locks fix.
#' Under the board lock, R refuses an evaluation that binds a block's data
#' inputs to other blocks or gives a locked block another state,
#' with an error that names the lock.
#' Where the load lock is on too, R holds every block to the opening board,
#' which it reads as the app starts.
#' It evaluates only that board's blocks, with their opening links,
#' and every locked block with the block expression that it built from the
#' block's opening state.
#' A locked block whose block expression fails to build from that state stops
#' the app as it starts, naming the block, unless the failure is a
#' [blockr_invalid()], which R replies with to every evaluation of the block.
#' Since no migration runs in R, it refuses a JSON board that `serve()` is
#' given with a block whose class the block registry lacks, or whose state is
#' at another version than its class gives, naming each.
#' Opening and saving the board once in an app of the same packages migrates
#' it in the client.
#' Without the load lock, R holds a block to its first evaluation since the
#' board opened.
#' That guards against a defect in the client,
#' but not against a client that sends another lock.
#'
#' R runs every request as on any board, an action among them,
#' with the board option values that the request carries,
#' since the locks fix the board and its blocks' state, which no request
#' changes.
#' A request that should not run on a locked board, such as one that evaluates
#' code that the client sends, is its package's to refuse,
#' by reading the blockr option `locked`.
#'
#' @section Scope: A lock is no sandbox.
#' A block left open may parse what a reader types into code,
#' and blockr evaluates arbitrary R.
#' So a hard boundary has to live where the user's code does not run,
#' such as a sandboxed R process.
#'
#' @examples
#' new_head_block(n = 10L, locked = FALSE)
#'
#' @name locked-board
NULL

# Whether the blockr option `name`, `locked` or `lock_load`, turns its lock on.
# A lock is off unless an app or a deployment sets it.
# An environment variable gives a string, which R reads as `as.logical()`
# does, as the client reads a blockr option that its code reads as a logical.
lock_option <- function(name) {

  x <- blockr_option(name, FALSE)

  if (is.character(x)) {
    x <- as.logical(trimws(x))
  }

  if (!is_bool(x)) {
    blockr_abort(
      "The blockr option `{name}` is TRUE or FALSE.",
      class = "lock_option_invalid"
    )
  }

  x
}

# Whether the board lock is on, which the blockr option `locked` sets.
board_lock_on <- function() {
  lock_option("locked")
}

# Whether the load lock is on, which the blockr option `lock_load` sets.
load_lock_on <- function() {
  lock_option("lock_load")
}

# The locks that the app sets, as R sends them with its reply to `hello`.
app_locks <- function() {
  list(board = board_lock_on(), load = load_lock_on())
}

# A block's lock as R reads it from JSON, by the schema of core's block,
# which gives it as a boolean or null.
# JSON's readers read `null` as `NULL`, which R reads as `NA`.
read_block_lock <- function(x, what) {

  if (is.null(x)) {
    return(NA)
  }

  if (!is_bool(x)) {
    blockr_abort(
      "{what} holds a lock that is neither true, false nor null.",
      class = "block_locked_invalid"
    )
  }

  x
}
