#' @section Subset block:
#' This block allows to perform row and column subsetting on `data.frame`
#' objects via [base::subset()]. Using non-standard evaluation, strings passed
#' as `subset`/`select` arguments or entered via shiny UI are turned into
#' `language` objects by [base::parse()].
#'
#' @param subset,select Expressions (passed as strings)
#'
#' @block subset block
#' @blockDescr Row and column subsetting
#' @blockCategory transform
#' @blockIcon funnel
#' @blockGuidance Row- and column-subsets the input with `base::subset()`.
#'   `subset` is a logical expression over the columns keeping rows where it is
#'   TRUE; `select` picks columns. Both are R expressions written as strings and
#'   evaluated with the data columns in scope.
#' @blockKeywords filter subset select rows columns where
#' @blockArg subset Logical row-filter expression, as a string.
#'   [example] "Sepal.Width > 3"
#'   [type] arg_string()
#' @blockArg select Column-selection expression, as a string.
#'   [example] "Species"
#'   [type] arg_string()
#'
#' @rdname new_transform_block
#' @export
new_subset_block <- function(subset = "", select = "", ...) {

  pasrse_first <- function(x) {
    parse(text = x)[[1L]]
  }

  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          sub <- reactiveVal(subset)
          sel <- reactiveVal(select)

          observeEvent(
            input$eval,
            {
              sub(input$subset)
              sel(input$select)
            }
          )

          observeEvent(
            req(sub()),
            {
              if (!identical(sub(), input$subset)) {
                updateTextInput(session, "subset", value = sub())
              }
            }
          )

          observeEvent(
            req(sel()),
            {
              if (!identical(sel(), input$select)) {
                updateTextInput(session, "select", value = sel())
              }
            }
          )

          list(
            expr = reactive(
              {
                su <- sub()
                se <- sel()

                if (nzchar(su) && nzchar(se)) {
                  bbquote(
                    subset(.(data), .(su), .(se)),
                    list(su = pasrse_first(su), se = pasrse_first(se))
                  )
                } else if (nzchar(se)) {
                  bbquote(
                    subset(.(data), select = .(se)),
                    list(se = pasrse_first(se))
                  )
                } else if (nzchar(su)) {
                  bbquote(
                    subset(.(data), .(su)),
                    list(su = pasrse_first(su))
                  )
                } else {
                  quote(subset(.(data)))
                }
              }
            ),
            state = list(subset = sub, select = sel)
          )
        }
      )
    },
    function(id) {
      tagList(
        textInput(
          inputId = NS(id, "subset"),
          label = "Subset",
          value = subset,
          placeholder = "Enter a row subsetting expression."
        ),
        textInput(
          inputId = NS(id, "select"),
          label = "Select",
          value = select,
          placeholder = "Enter a column selection expression."
        ),
        actionButton(
          inputId = NS(id, "eval"),
          label = "Evaluate",
        )
      )
    },
    dat_val = function(data) {
      stopifnot(is.data.frame(data))
    },
    allow_empty_state = TRUE,
    expr_type = "bquoted",
    external_ctrl = TRUE,
    class = "subset_block",
    ...
  )
}
