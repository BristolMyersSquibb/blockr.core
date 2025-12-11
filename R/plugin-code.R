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
#' @param ... Extra arguments passed from parent scope
#'
#' @rdname generate_code
#' @export
generate_code_server <- function(id, board, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      code <- reactive(
        export_wrapped_code(
          lst_xtr_reval(board$blocks, "server", "expr"),
          board$board
        )
      )

      observeEvent(
        input$code_mod,
        showModal(
          modalDialog(
            title = "Generated code",
            div(
              id = session$ns("code_out"),
              class = "text-decoration-none position-relative",
              pre(paste0(code(), collapse = "\n"))
            ),
            easyClose = TRUE,
            footer = NULL,
            size = "l"
          )
        )
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
