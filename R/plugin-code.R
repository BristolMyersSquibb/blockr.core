#' Code generation plugin module
#'
#' All code necessary for reproducing a data analysis as set up in blockr can
#' be made available to the user. Several ways of providing such a script or
#' code snippet are conceivable and currently implemented, we have a modal
#' with copy-to-clipboard functionality. This is readily extensible, for example
#' by offering a download button, by providing this functionality as a
#' `generate_code` module.
#'
#' Opening the modal asks for every block on the board to be built, through the
#' `construct` board update component (see the "Evaluation requests" section of
#' [board_server()]), because the script is assembled from block expressions and
#' an unbuilt block carries none. Nothing is evaluated: a board that defers its
#' off-screen blocks stays deferred, and the front-end's gating is untouched.
#'
#' Export is held back while a block is not fully configured, or while one
#' reports an error from its last run — either would put code into the script
#' that does not reproduce the board. A block that has never run reports
#' neither, so the modal offers to evaluate the board, which is a one-off that
#' leaves the blocks dormant again but has them report what they found.
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
#' @param update Reactive value object to initiate board updates
#' @param ... Extra arguments passed from parent scope
#'
#' @rdname generate_code
#' @export
generate_code_server <- function(id, board, update, ...) {
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
          update(list(construct = board_block_ids(board$board)))

          showModal(code_modal(session$ns))
        }
      )

      observeEvent(
        input$code_eval,
        update(list(evaluate = board_block_ids(board$board)))
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

code_modal <- function(ns) {
  modalDialog(
    title = "Generated code",
    uiOutput(ns("code_out")),
    easyClose = TRUE,
    footer = tagList(
      actionButton(ns("code_eval"), "Evaluate blocks", class = "btn-secondary"),
      modalButton("Close")
    ),
    size = "l"
  )
}

code_export_state <- function(board) {

  ids <- board_block_ids(board$board)

  if (!setequal(names(board$blocks), ids)) {
    return("pending")
  }

  ready <- lgl_ply(board$blocks, block_state_ready)

  if (!all(ready) || nrow(export_block_errors(board)) > 0L) {
    return("blocked")
  }

  "ready"
}

block_state_ready <- function(blk) {
  isTRUE(reval_if(blk$server$state_ready))
}

export_block_errors <- function(board) {
  cnd <- reval_if(board$conditions)
  cnd[cnd$severity == "error", ]
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
      "The board is not ready. Finish configuring all blocks, and fix any",
      "block reporting an error, before exporting code."
    )
  )
}
