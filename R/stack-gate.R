#' @rdname board_server
#' @export
gate_stacks <- function() {

  function(board, visibility, update, session = get_session(), ...) {

    observe(show_open_stacks(board, visibility, update, session))

    NULL
  }
}

show_open_stacks <- function(board, vis, update, session) {

  open <- session$input[["stacks"]]

  # Read before this returns, so the observer wakes when the accordion first
  # reports. Until it does, core leaves every block alone: a board that renders
  # its own UI never binds the accordion, and then nothing here should gate it.
  if (!stacks_reported(session)) {
    return(invisible())
  }

  brd <- board$board
  owner <- stack_gate_owner(session)

  shown <- shown_block_ids(brd, open_stack_ids(open, brd, session))

  # Declared on the first report rather than as the callback is set up, for the
  # same reason: a board that never reports must not end up gated on a claim
  # this callback is not making.
  vis$gate(owner)

  # A collapsed stack's blocks are parked rather than dropped -- held out of the
  # claim but still built, so re-expanding shows them without a rebuild. Core
  # ignores a `construct` request naming a block it has already built.
  update(
    list(
      sustain = set_names(list(list(set = shown)), owner),
      construct = setdiff(board_block_ids(brd), shown)
    )
  )

  for (id in ls(vis$visible)) {
    vis$visible[[id]](id %in% shown)
  }

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
