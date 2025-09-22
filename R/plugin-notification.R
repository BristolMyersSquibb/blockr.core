#' User notification plugin module
#'
#' During the evaluation cycle of each block, user notifications may be
#' generated to inform in case of issues such as errors or warnings. These
#' notifications are provided in a way that display can be controlled and
#' adapted to specific needs. The default `notify_user` plugin simply displays
#' notifications via [shiny::showNotification()], with some ID management in
#' order to be able to clear no longer relevant notifications via
#' [shiny::removeNotification()].
#'
#' @param server,ui Server/UI for the plugin module
#'
#' @return A plugin container inheriting from `notify_user` is returned by
#' `notify_user()`, while the UI component (e.g. `notify_user_ui()`) is
#' expected to return shiny UI (i.e. [shiny::tagList()]; if available) and the
#' server component (i.e. `notify_user_server()`) is expected to return a
#' [shiny::reactiveVal()] or [shiny::reactive()] which evaluates to a list
#' containing notifications per block and notification type (i.e. "message",
#' "warning" or "error").
#'
#' @export
notify_user <- function(server = notify_user_server, ui = notify_user_ui) {
  new_plugin(server, ui, class = "notify_user")
}

#' @param id Namespace ID
#' @param board Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @rdname notify_user
#' @export
notify_user_server <- function(id, board, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      state <- reactiveVal(
        set_up_blocks_notif(isolate(board$blocks), session = session)
      )

      observeEvent(
        names(board$blocks),
        update_block_notif(board$blocks, state, session)
      )

      invisible()
    }
  )
}

#' @rdname notify_user
#' @export
notify_user_ui <- function(id, board) {
  tagList(
    if (requireNamespace("cli", quietly = TRUE)) {
      tags$style(HTML(paste(format(cli::ansi_html_style()), collapse = "\n")))
    }
  )
}

update_block_notif <- function(blocks, state, session) {

  do_add <- setdiff(names(blocks), names(state))

  if (length(do_add)) {
    tmp <- set_up_blocks_notif(blocks, do_add, session)
    state(c(state(), tmp))
  }

  to_rm <- setdiff(names(state()), names(blocks))

  if (length(to_rm)) {
    state(
      tear_down_blocks_notif(state(), to_rm, session)
    )
  }

  invisible()
}

set_up_blocks_notif  <- function(blocks, todo = names(blocks),
                                 session = get_session()) {

  res <- set_names(vector("list", length(todo)), todo)

  for (blk in todo) {
    res[[blk]] <- set_up_block_notif(blocks[[blk]]$server$cond, blk, session)
  }

  res
}

set_up_block_notif <- function(conds, blk, session) {
  lapply(set_names(nm = names(conds)), set_up_type_notif, conds, blk, session)
}

set_up_type_notif <- function(typ, conds, blk, session) {

  ids <- reactiveVal(character())

  obs <- observeEvent(
    conds[[typ]],
    {
      new_ids <- create_block_notif(conds[[typ]], blk, ids(),
                                    session)

      remove_block_notif(new_ids, ids(), session)

      ids(new_ids)
    }
  )

  list(ids = ids, obs = obs)
}

tear_down_blocks_notif <- function(state, blocks = names(state),
                                   session = get_session()) {

  for (blk in blocks) {
    for (typ in names(state[[blk]])) {
      remove_block_notif(character(), state[[blk]][[typ]]$ids(), session)
      state[[blk]][[typ]]$obs$destroy()
    }
  }

  state[setdiff(names(state), blocks)]
}

create_block_notif <- function(x, blk, prev, session = get_session()) {

  cur <- character()

  if (all(lengths(x) == 0L)) {
    return(cur)
  }

  cnds <- coal(
    isolate(
      get_board_option_or_null("show_conditions", session)
    ),
    blockr_option("show_conditions", c("warning", "error"))
  )

  for (cnd in intersect(names(x), cnds)) {
    for (msg in x[[cnd]]) {

      id <- paste(cnd, attr(msg, "id"), sep = "-")

      if (id %in% prev) {
        next
      }

      showNotification(
        HTML(paste0("Block ", blk, ": ", cli::ansi_html(msg))),
        duration = NULL,
        id = id,
        type = cnd,
        session = session
      )

      cur <- c(cur, id)
    }
  }

  cur
}

remove_block_notif <- function(new_ids, old_ids, session) {
  for (id in setdiff(old_ids, new_ids)) {
    removeNotification(id, session)
  }
}
