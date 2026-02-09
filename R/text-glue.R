#' @section Glue block:
#' Using [glue::glue()], this block allows evaluation of a text string in the
#' context of datasets to produce (markdown formatted) text as block result.
#'
#' @param text String evaluated using [glue::glue()]
#'
#' @block Glue string block
#' @blockDescr String interpolation using glue
#' @blockCategory utility
#' @blockIcon braces
#'
#' @rdname new_text_block
#' @export
new_glue_block <- function(text = character(), ...) {

  new_text_block(
    function(id, ...args) {
      moduleServer(
        id,
        function(input, output, session) {

          arg_names <- reactive(
            set_names(names(...args), dot_args_names(...args))
          )

          list(
            expr = reactive(
              bquote(
                glue::glue(.(txt), .envir = .(env)),
                list(
                  txt = input$text,
                  env = bquote(
                    list2env(list(..(data)), parent = baseenv()),
                    list(data = lapply(arg_names(), as_dot_call)),
                    splice = TRUE
                  )
                )
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
    expr_type = "bquoted",
    class = "glue_block",
    ...
  )
}
