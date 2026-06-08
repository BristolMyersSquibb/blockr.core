#' User notification plugin module
#'
#' During the evaluation cycle of each block, conditions (errors, warnings and
#' messages) may be raised. The default `notify_user` plugin surfaces these to
#' the user as [shiny::showNotification()] toasts, displaying newly active
#' conditions and clearing ones that are no longer active via
#' [shiny::removeNotification()]. It renders from `board$conditions()` (see
#' [board_server()]), the board-level reactive frame of active conditions, so
#' that toast display and any programmatic consumer share a single processed
#' view rather than each walking per-block condition state.
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

      shown <- reactiveVal(character())

      observeEvent(
        board$conditions(),
        shown(
          update_condition_notif(board$conditions(), shown(), session)
        ),
        ignoreNULL = FALSE
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

update_condition_notif <- function(conditions, shown,
                                   session = get_session()) {

  cnds <- coal(
    isolate(
      get_board_option_or_null("show_conditions", session)
    ),
    blockr_option("show_conditions", c("warning", "error"))
  )

  conditions <- conditions[conditions$severity %in% cnds, , drop = FALSE]

  ids <- paste(conditions$severity, conditions$id, sep = "-")
  keep <- which(!duplicated(ids))

  cur <- character()

  for (i in keep) {

    id <- ids[i]

    if (!id %in% shown) {
      showNotification(
        HTML(
          paste0(
            "Block ", conditions$block[i], ": ",
            cli::ansi_html(conditions$message[i])
          )
        ),
        duration = NULL,
        id = id,
        type = conditions$severity[i],
        session = session
      )
    }

    cur <- c(cur, id)
  }

  for (id in setdiff(shown, cur)) {
    removeNotification(id, session)
  }

  cur
}
