#' @section Glue block:
#' Using [glue::glue()], this block allows evaluation of a text string in the
#' context of datasets to produce (markdown formatted) text as block result.
#'
#' @param text String evaluated using [glue::glue()]
#'
#' @rdname new_text_block
#' @export
new_glue_block <- function(text = character(), ...) {

  new_text_block(
    function(id, ...args) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(
              bquote(
                glue::glue(.(txt)),
                list(txt = input$text)
              )
            ),
            state = list(
              text = reactive(input$text)
            )
          )
        }
      )
    },
    function(id) {
      textAreaInput(
        inputId = NS(id, "text"),
        label = NULL,
        value = text,
        placeholder = "You may use markdown syntax to style the text."
      )
    },
    class = "glue_block",
    ...
  )
}
