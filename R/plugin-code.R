#' Code generation plugin module
#'
#' All code necessary for reproducing a data analysis as set up in blockr can
#' be made available to the user. Several ways of providing such a script or
#' code snippet are conceivable and currently implemented, we have a modal
#' with copy-to-clipboard functionality. This is readily extensible, for example
#' by offering a download button, by providing this functionality as a
#' `generate_code` module.
#'
#' @param server,ui Server/UI for the plugin module
#'
#' @return A plugin container inheriting from `generate_code` is returned by
#' `generate_code()`, while the UI component (e.g. `generate_code_ui()`) is
#' expected to return shiny UI (i.e. [shiny::tagList()]) and the server
#' component (i.e. `generate_code_server()`) is expected to return `NULL`.
#'
#' @export
generate_code <- function(server = generate_code_server,
                          ui = generate_code_ui) {

  new_plugin(server, ui, class = "generate_code")
}

#' @param id Namespace ID
#' @param board Reactive values object
#' @param visibility Visibility channel bundle (supplied by [board_server()]).
#' On a gated board, "Show code" marks every block `required`, so the exported
#' script covers the whole board and not only what is on screen; an off-screen
#' block that is not fully configured then holds the export back rather than
#' emitting broken code. `NULL` (the standalone default) leaves the board
#' untouched.
#' @param ... Extra arguments passed from parent scope
#'
#' @rdname generate_code
#' @export
generate_code_server <- function(id, board, visibility = NULL, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      output$code_out <- renderUI(
        {
          state <- code_export_state(board)

          script <- if (identical(state, "ready")) {
            export_wrapped_code(
              lst_xtr_reval(board$blocks, "server", "expr"),
              board$board
            )
          }

          code_modal_body(state, script)
        }
      )

      observeEvent(
        input$code_mod,
        {
          require_all_blocks(board, visibility)

          showModal(
            modalDialog(
              title = "Generated code",
              uiOutput(session$ns("code_out")),
              easyClose = TRUE,
              footer = NULL,
              size = "l"
            )
          )
        }
      )

      NULL
    }
  )
}

#' @rdname generate_code
#' @export
generate_code_ui <- function(id, board) {
  tagList(
    actionButton(
      NS(id, "code_mod"),
      "Show code",
      icon = icon("code")
    )
  )
}

code_export_state <- function(board) {

  ids <- board_block_ids(board$board)
  status <- reactiveValuesToList(board$eval)

  if (!setequal(names(board$blocks), ids) || !setequal(names(status), ids)) {
    return("pending")
  }

  if (all(chr_ply(status, reval_if) %in% c("ready", "dormant"))) {
    return("ready")
  }

  "blocked"
}

code_modal_body <- function(state, script = NULL) {

  if (identical(state, "ready")) {
    return(
      div(
        class = "text-decoration-none position-relative",
        pre(paste0(script, collapse = "\n"))
      )
    )
  }

  if (identical(state, "pending")) {
    return(
      div(class = "text-muted", "Preparing code...")
    )
  }

  div(
    class = "text-muted",
    paste(
      "The board is not ready. Finish configuring all blocks",
      "before exporting code."
    )
  )
}

require_all_blocks <- function(board, visibility) {

  if (is.null(visibility) || !gating_active(visibility$required)) {
    return(invisible())
  }

  for (id in board_block_ids(board$board)) {

    slot <- visibility$required[[id]]

    if (not_null(slot)) {
      slot(TRUE)
    }
  }

  invisible()
}
