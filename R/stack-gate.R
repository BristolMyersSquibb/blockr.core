#' @rdname board_server
#' @export
gate_stacks <- function() {

  function(board, visibility, update, session = get_session(), ...) {

    observe(show_open_stacks(board, visibility, update, session))

    open_stacks_claim(isolate(board$board), session)
  }
}

# A stackless board renders an accordion that never binds as an input, so
# nothing would ever arrive to refine a claim made on its behalf and it would
# stay parked for the session; it is left ungated instead.
open_stacks_claim <- function(board, session) {

  if (!has_length(board_stack_ids(board))) {
    return(NULL)
  }

  gate_claim(
    stack_gate_owner(session),
    shown_block_ids(board, default_open_stacks(board_stacks(board)))
  )
}

show_open_stacks <- function(board, vis, update, session) {

  open <- session$input[["stacks"]]

  # Read before this returns, so the observer wakes when the accordion first
  # reports. Until it does, what stands is the claim the callback declared --
  # and for a board that renders its own UI and never binds the accordion,
  # nothing at all.
  if (!stacks_reported(session)) {
    return(invisible())
  }

  brd <- board$board
  owner <- stack_gate_owner(session)

  shown <- shown_block_ids(brd, open_stack_ids(open, brd, session))

  update(list(sustain = set_names(list(list(set = shown)), owner)))

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
