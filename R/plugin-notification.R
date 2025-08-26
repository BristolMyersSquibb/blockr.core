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
  new_plugin(server, ui, validator = check_block_notifications_val,
             class = "notify_user")
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

      onStop(
        function() set_globals(list(), session = session)
      )

      if (length(get_globals(session = session))) {
        warning("Existing notification IDs will be purged.")
      }

      set_globals(list(), session = session)

      cnd <- reactive(
        lst_xtr_reval(board$blocks, "server", "cond")
      )

      observeEvent(
        cnd(),
        {
          notf <- cnd()
          ids <- get_globals(session = session)

          for (blk in setdiff(names(ids), names(notf))) {

            for (id in ids[[blk]]) {
              removeNotification(id)
            }

            ids[[blk]] <- NULL
            set_globals(ids, session = session)
          }

          for (blk in names(notf)) {

            cur <- create_block_notifications(notf, blk)

            for (id in setdiff(ids[[blk]], cur)) {
              removeNotification(id)
            }

            ids[[blk]] <- cur
            set_globals(ids, session = session)
          }
        }
      )

      reactive(
        filter_all_zero_len(cnd())
      )
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

create_block_notifications <- function(notf, blk, session = get_session()) {

  cur <- c()

  for (typ in names(notf[[blk]])) {
    for (cnd in names(notf[[blk]][[typ]])) {
      for (msg in notf[[blk]][[typ]][[cnd]]) {

        id <- session_to_id(attr(msg, "id"), session)

        showNotification(
          HTML(paste0("Block ", blk, ": ", ansi_html(msg))),
          duration = NULL,
          id = id,
          type = cnd
        )

        cur <- c(cur, id)
      }
    }
  }

  cur
}

check_block_notifications_val <- function(val) {

  observeEvent(
    TRUE,
    {
      if (!is.reactive(val)) {
        abort(
          "Expecting `notify_user` to return a reactive value.",
          class = "notify_user_return_invalid"
        )
      }
    },
    once = TRUE
  )

  observeEvent(
    val(),
    {
      if (!is.list(val())) {
        abort(
          "Expecting the `notify_user` return value to evaluate to a list.",
          class = "notify_user_return_invalid"
        )
      }
    },
    once = TRUE
  )

  val
}
