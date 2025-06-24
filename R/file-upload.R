#' @section Upload block:
#' In order to make user data available to blockr, this block provides file-
#' upload functionality via [shiny::fileInput()]. Given that data provided in
#' this way are only available for the life-time of the shiny session, exported
#' code is not self-contained and a script containing code from an upload block
#' is cannot be run in a new session. Also, serialization of upload blocks is
#' currently not allowed as the full data would have to be included during
#' serialization.
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
