#' User notification plugin module
#'
#' During the evaluation cycle of each block, conditions (errors, warnings and
#' messages) may be raised. The default `notify_user` plugin surfaces these to
#' the user as [shiny::showNotification()] toasts, displaying newly active
#' conditions and clearing ones that are no longer active via
#' [shiny::removeNotification()]. Each block's conditions (see [board_server()])
#' are tracked individually, so that a single block's change touches only its
#' own notifications rather than re-processing the whole board.
#'
#' @param server,ui Server/UI for the plugin module
#'
#' @return A plugin container inheriting from `notify_user` is returned by
#' `notify_user()` and the UI component (e.g. `notify_user_ui()`) is expected
#' to return shiny UI (i.e. [shiny::tagList()]). The server component (i.e.
#' `notify_user_server()`) is called for the side effect of managing
#' notifications and returns `NULL`.
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

      tracked <- new.env(parent = emptyenv())

      observe({

        ids <- names(board$blocks)

        isolate(
          sync_block_notifs(ids, tracked, board, session)
        )
      })

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

sync_block_notifs <- function(ids, tracked, board, session) {

  for (id in setdiff(ids, ls(tracked))) {
    tracked[[id]] <- block_notif_observer(id, board, session)
  }

  for (id in setdiff(ls(tracked), ids)) {

    tracked[[id]]$destroy()
    rm(list = id, envir = tracked)
  }

  invisible()
}

block_notif_observer <- function(block_id, board, session) {

  conditions <- isolate(board$blocks[[block_id]]$server$conditions)

  shown <- character()

  obs <- observeEvent(
    conditions(),
    {
      frame <- notif_frame(conditions(), session)

      for (key in setdiff(shown, frame$key)) {
        notify_remove(key, session)
      }

      for (key in setdiff(frame$key, shown)) {
        notif_show(key, frame[frame$key == key, , drop = FALSE], session)
      }

      shown <<- frame$key
    },
    ignoreNULL = FALSE
  )

  list(
    destroy = function() {

      obs$destroy()

      for (key in shown) {
        notify_remove(key, session)
      }
    }
  )
}

notif_frame <- function(conditions, session = get_session()) {

  cnds <- coal(
    isolate(
      get_board_option_or_null("show_conditions", session)
    ),
    blockr_option("show_conditions", c("warning", "error"))
  )

  conditions <- conditions[conditions$severity %in% cnds, , drop = FALSE]

  conditions$key <- paste(
    conditions$block, conditions$severity, conditions$id,
    sep = "-"
  )

  conditions[!duplicated(conditions$key), , drop = FALSE]
}

notif_show <- function(key, row, session) {
  notify(
    "Block ", row$block, ": ", row$message,
    glue = FALSE,
    log = FALSE,
    duration = NULL,
    id = key,
    type = row$severity,
    session = session
  )
}
