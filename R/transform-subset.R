#' @section Subset block:
#' This block allows to perform row and column subsetting on `data.frame`
#' objects via [base::subset()]. Using non-standard evaluation, strings passed
#' as `subset`/`select` arguments or entered via shiny UI are turned into
#' `language` objects by [base::parse()].
#'
#' @param subset,select Expressions (passed as strings)
#'
#' @rdname new_transform_block
#' @export
new_subset_block <- function(subset = "", select = "", ...) {
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

          list(
            expr = reactive(subset_expr(sub(), sel())),
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
    class = "subset_block",
    ...
  )
}

subset_expr <- function(sub, sel) {

  pasrse_first <- function(x) {
    parse(text = x)[[1L]]
  }

  if (nzchar(sub) && nzchar(sel)) {
    bquote(
      subset(data, .(sub), .(sel)),
      list(sub = pasrse_first(sub), sel = pasrse_first(sel))
    )
  } else if (nzchar(sel)) {
    bquote(
      subset(data, select = .(sel)),
      list(sel = pasrse_first(sel))
    )
  } else if (nzchar(sub)) {
    bquote(
      subset(data, .(sub)),
      list(sub = pasrse_first(sub))
    )
  } else {
    quote(subset(data))
  }
}
