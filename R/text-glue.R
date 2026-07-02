#' @section Glue block:
#' Using [glue::glue()], this block allows evaluation of a text string in the
#' context of datasets to produce (markdown formatted) text as block result.
#'
#' @param text String evaluated using [glue::glue()]
#'
#' @block glue string block
#' @blockDescr String interpolation using glue
#' @blockCategory utility
#' @blockIcon braces
#' @blockGuidance Interpolates a template against the input data with
#'   `glue::glue()` to produce (markdown) text. Reference columns of the data in
#'   `text` with braces, e.g. "{nrow(data)} rows".
#' @blockKeywords glue template text string interpolate markdown
#' @blockArg text new_block_arg(
#'   "Template string evaluated with glue::glue().",
#'   example = "{nrow(data)} rows",
#'   type = arg_string()
#' )
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
            dot_arg_refs(...args)
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
