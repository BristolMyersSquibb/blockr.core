#' @section Scatter block:
#' Mainly for demonstration purposes, this block draws a scattter plot using
#' [base::plot()]. In its current simplistic implementation, apart from axis
#' labels (fixed to the corresponding column names), no further plotting
#' options are available and for any "production" application, a more
#' sophisticated (set of) block(s) for data visualization will most likely be
#' required.
#'
#' @param x,y Columns to place on respective axes
#'
#' @rdname new_plot_block
#' @export
new_scatter_block <- function(x = character(), y = character(), ...) {
  new_plot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          y_col <- reactiveVal(y)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$ycol, y_col(input$ycol))

          observeEvent(
            cols(),
            {
              updateSelectInput(
                session,
                inputId = "xcol",
                choices = cols(),
                selected = x_col()
              )
              updateSelectInput(
                session,
                inputId = "ycol",
                choices = cols(),
                selected = y_col()
              )
            }
          )

          list(
            expr = reactive(
              bbquote(
                plot(.(data)[[.(x)]], data[[.(y)]], xlab = .(x_col),
                     ylab = .(y_col)),
                list(x = x_col(), y = y_col(), x_col = x_col(),
                     y_col = y_col())
              )
            ),
            state = list(x = x_col, y = y_col)
          )
        }
      )
    },
    function(id) {
      tagList(
        selectInput(
          inputId = NS(id, "xcol"),
          label = "X-axis",
          choices = x,
          selected = x
        ),
        selectInput(
          inputId = NS(id, "ycol"),
          label = "Y-axis",
          choices = y,
          selected = y
        )
      )
    },
    dat_valid = function(data) {
      stopifnot(is.data.frame(data) || is.matrix(data))
    },
    expr_type = "bquoted",
    class = "scatter_block",
    ...
  )
}
