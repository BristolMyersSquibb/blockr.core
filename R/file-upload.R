#' @section Upload block:
#' In order to make user data available to blockr, this block provides file-
#' upload functionality via [shiny::fileInput()]. Given that data provided in
#' this way are only available for the life-time of the shiny session, exported
#' code is not self-contained and a script containing code from an upload block
#' is cannot be run in a new session. Also, serialization of upload blocks is
#' currently not allowed as the full data would have to be included during
#' serialization.
#'
#' @block data upload block
#' @blockDescr Upload data
#' @blockCategory input
#' @blockIcon upload
#' @blockGuidance Provides a data.frame from a file uploaded in the running app.
#'   Uploaded data lives only for the session, so serialization and reproducible
#'   export are unavailable. Has no configurable arguments.
#' @blockKeywords upload file import data session
#'
#' @rdname new_file_block
#' @export
new_upload_block <- function(...) {
  new_file_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(
              bquote(.(file), list(file = input$upload$datapath))
            ),
            state = list()
          )
        }
      )
    },
    function(id) {
      fileInput(
        NS(id, "upload"),
        "Upload data"
      )
    },
    class = "upload_block",
    ...
  )
}
