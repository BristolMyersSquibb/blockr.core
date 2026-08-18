#' @rdname board_server
#' @export
gate_stacks <- function() {

  function(board, visibility, update, session = get_session(), ...) {

    brd <- isolate(board$board)

    # A stackless board renders an accordion that never binds as an input, so
    # nothing would ever arrive to refine a declaration made on its behalf and
    # it would stay parked for the session.
    if (has_length(board_stack_ids(brd))) {
      seed_open_stacks(brd, visibility, update, session)
    }

    observe(show_open_stacks(board, visibility, update, session))

    NULL
  }
}

# Declared before the first flush, because a board with no gate declared is one
# where every block is needed: a collapsed stack's blocks would otherwise
# evaluate once in the window before the accordion reports. The gate itself is
# written here rather than sent, since a payload only applies at the end of the
# flush it is written in, by which time that window has passed. Paint stays the
# client's to report -- claiming it here as well would let the construction
# backlog build against first paint.
seed_open_stacks <- function(board, vis, update, session) {

  shown <- shown_block_ids(board, default_open_stacks(board_stacks(board)))

  vis$gate(stack_gate_owner(session))

  claim_shown_blocks(board, shown, update, session)
}

show_open_stacks <- function(board, vis, update, session) {

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

  vis$gate(stack_gate_owner(session))

  claim_shown_blocks(brd, shown, update, session)

  for (id in ls(vis$visible)) {
    vis$visible[[id]](id %in% shown)
  }

  invisible()
}

# A collapsed stack's blocks are parked rather than dropped -- held out of the
# claim but still built, so re-expanding shows them without a rebuild. Core
# ignores a `construct` request naming a block it has already built.
claim_shown_blocks <- function(board, shown, update, session) {

  owner <- stack_gate_owner(session)

  update(
    list(
      sustain = set_names(list(list(set = shown)), owner),
      construct = setdiff(board_block_ids(board), shown)
    )
  )

  invisible()
}

stack_gate_owner <- function(session) {
  session$ns("gate_stacks")
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
