#' @rdname board_server
#' @export
gate_stacks <- function() {

  function(board, visibility, session = get_session(), ...) {

    brd <- isolate(board$board)

    # A stackless board renders an accordion that never binds as an input, so
    # nothing would ever arrive to refine a declaration made on its behalf and
    # it would stay parked for the session.
    if (has_length(board_stack_ids(brd))) {
      seed_open_stacks(brd, visibility)
    }

    observe(show_open_stacks(board, visibility, session))

    NULL
  }
}

# Declared before the first flush, because a board with no gate declared is one
# where every block is needed: a collapsed stack's blocks would otherwise
# evaluate once in the window before the accordion reports. Paint stays the
# client's to report -- claiming it here as well would let the construction
# backlog build against first paint.
seed_open_stacks <- function(board, vis) {

  shown <- shown_block_ids(board, default_open_stacks(board_stacks(board)))

  for (id in ls(vis$required)) {
    vis$required[[id]](id %in% shown)
  }

  invisible()
}

show_open_stacks <- function(board, vis, session) {

  open <- session$input[["stacks"]]

  # Read before this returns, so the observer wakes when the accordion first
  # reports. Until it does, what stands is the declaration seeded above -- and
  # for a board that renders its own UI and never binds the accordion, nothing
  # at all.
  if (!stacks_reported(session)) {
    return(invisible())
  }

  brd <- board$board

  shown <- shown_block_ids(brd, open_stack_ids(open, brd, session))

  # A collapsed stack's blocks are parked rather than dropped: `FALSE` keeps
  # them built and ready to show again, where an `NA` slot would leave them
  # unbuilt.
  for (id in ls(vis$required)) {
    vis$required[[id]](id %in% shown)
    vis$visible[[id]](id %in% shown)
  }

  invisible()
}

# The accordion input reads NULL both before it has bound and once the user has
# collapsed every stack; only the registered input name tells the two apart.
stacks_reported <- function(session) {
  "stacks" %in% names(session$input)
}

open_stack_ids <- function(open, board, session) {

  ids <- board_stack_ids(board)

  ids[chr_ply(paste0("stack_", ids), session$ns) %in% open]
}

shown_block_ids <- function(board, open) {

  collapsed <- setdiff(board_stack_ids(board), open)

  available_stack_blocks(
    board,
    board_stacks(board)[collapsed],
    board_block_ids(board)
  )
}
