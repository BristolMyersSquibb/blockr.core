#' @param active Whether the callback gates on the accordion (defaulting to the
#' `gate_stacks` [blockr_option()])
#' @rdname board_server
#' @export
gate_stacks <- function(active = blockr_option("gate_stacks", FALSE)) {

  function(board, visibility, session = get_session(), ...) {

    if (!isTRUE(active)) {
      return(NULL)
    }

    observe(show_open_stacks(board, visibility, session))

    NULL
  }
}

show_open_stacks <- function(board, vis, session) {

  brd <- board$board

  shown <- shown_block_ids(brd, open_stack_ids(session, brd))

  # A collapsed stack's blocks are parked rather than dropped: `FALSE` keeps
  # them built and ready to show again, where an `NA` slot would leave them
  # unbuilt. Paint stays the accordion's to report -- until it has spoken core
  # knows what it asked to be shown but not what the client has put on screen.
  for (id in ls(vis$required)) {

    vis$required[[id]](id %in% shown)

    if (stacks_reported(session)) {
      vis$visible[[id]](id %in% shown)
    }
  }

  invisible()
}

open_stack_ids <- function(session, board) {

  # Read before the branch below can skip it: this is what wakes the observer
  # when the accordion first reports, including a report of nothing.
  open <- session$input[["stacks"]]

  if (!stacks_reported(session)) {
    return(default_open_stacks(board_stacks(board)))
  }

  ids <- board_stack_ids(board)

  ids[chr_ply(paste0("stack_", ids), session$ns) %in% open]
}

# The accordion input reads NULL both before it has bound and once the user has
# collapsed every stack; only the registered input name tells the two apart.
stacks_reported <- function(session) {
  "stacks" %in% names(session$input)
}

shown_block_ids <- function(board, open) {

  collapsed <- setdiff(board_stack_ids(board), open)

  available_stack_blocks(
    board,
    board_stacks(board)[collapsed],
    board_block_ids(board)
  )
}
