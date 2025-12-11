#' @section Head block:
#' Row-subsetting the first or last `n` rows of a `data.frame` (as provided by
#' [utils::head()] and [utils::tail()]) is implemented as `head_block`. This is
#' an example of a block that takes a single `data.frame` as input and produces
#' a single `data.frame` as output.
#'
#' @param n Number of rows
#' @param direction Either "head" or "tail"
#'
#' @rdname new_transform_block
#' @export
new_head_block <- function(n = 6L, direction = c("head", "tail"), ...) {

  direction <- match.arg(direction)

  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          nrw <- reactiveVal(n)
          til <- reactiveVal(isTRUE(direction == "tail"))

          observeEvent(input$n, nrw(input$n))
          observeEvent(input$tail, til(input$tail))

          observeEvent(
            nrow(data()),
            updateNumericInput(
              inputId = "n",
              value = nrw(),
              min = 1L,
              max = nrow(data())
            )
          )

          list(
            expr = reactive(
              if (isTRUE(til())) {
                bbquote(utils::tail(.(data), n = .(n)), list(n = nrw()))
              } else {
                bbquote(utils::head(.(data), n = .(n)), list(n = nrw()))
              }
            ),
            state = list(
              n = nrw,
              direction = reactive(if (isTRUE(til())) "tail" else "head")
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        numericInput(
          inputId = NS(id, "n"),
          label = "Number of rows",
          value = n,
          min = 1L
        ),
        bslib::input_switch(
          id = NS(id, "tail"),
          label = "Tail",
          value = isTRUE(direction == "tail")
        )
      )
    },
    dat_val = function(data) {
      stopifnot(is.data.frame(data))
    },
    expr_type = "bquoted",
    class = "head_block",
    ...
  )
}
