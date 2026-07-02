#' @section CSV block:
#' Files in CSV format provided for example by a block created via
#' [new_file_block()] may be parsed into `data.frame` by CSV blocks.
#'
#' @param sep,quote Forwarded to [utils::read.table()]
#'
#' @block csv parser block
#' @blockDescr Read CSV file
#' @blockCategory utility
#' @blockIcon filetype-csv
#' @blockGuidance Parses a delimited text file (typically from a file or upload
#'   block) into a data.frame with `utils::read.table()`. `sep` is the field
#'   separator and `quote` the quoting character.
#' @blockKeywords csv, parse, read, delimited, import
#' @blockArg sep Field separator character.
#'   [example] ","
#'   [type] arg_string()
#' @blockArg quote Quoting character.
#'   [type] arg_string()
#'
#' @rdname new_parser_block
#' @export
new_csv_block <- function(sep = ",", quote = "\"", ...) {
  new_parser_block(
    function(id, file) {
      moduleServer(
        id,
        function(input, output, session) {

          sp <- reactiveVal(sep)
          qo <- reactiveVal(quote)

          observeEvent(input$sep, sp(input$sep))
          observeEvent(input$quote, qo(input$quote))

          list(
            expr = reactive(
              bbquote(
                utils::read.table(.(file), header = TRUE, sep = .(sp),
                                  quote = .(qo), dec = ".", fill = TRUE,
                                  comment.char = ""),
                list(sp = sp(), qo = qo())
              )
            ),
            state = list(sep = sp, quote = qo)
          )
        }
      )
    },
    function(id) {
      tagList(
        textInput(
          inputId = NS(id, "sep"),
          label = "Field separator",
          value = sep
        ),
        textInput(
          inputId = NS(id, "quote"),
          label = "Quoting characters",
          value = quote
        )
      )
    },
    expr_type = "bquoted",
    class = "csv_block",
    ...
  )
}
