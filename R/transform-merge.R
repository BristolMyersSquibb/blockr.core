#' @section Merge block:
#' Joining together two `data.frame`s, based on a set of index columns, using
#' [base::merge()] is available as `merge_block`. Depending on values passed
#' as `all_x`/`all_y` the result will correspond to an "inner", "outer", "left"
#' or "right" join. See [base::merge()] for details. This block class serves
#' as an example for a transform block that takes exactly two data inputs `x`
#' and `y` to produce a single `data.frame` as output.
#'
#' @param by Column(s) tp join on
#' @param all_x,all_y Join type, see [base::merge()]
#'
#' @block merge block
#' @blockDescr Joining or datasets
#' @blockCategory transform
#' @blockIcon union
#' @blockGuidance Joins the two inputs with `base::merge()`. `by` names the
#'   shared key column(s); omit it to join on all common columns. all_x and
#'   all_y set the join type: both FALSE for an inner join, both TRUE for a full
#'   outer join, all_x alone for a left join, all_y alone for a right join.
#' @blockKeywords merge join inner outer left right
#' @blockArg by Shared key column(s) to join on.
#'   [type] arg_array(arg_string())
#' @blockArg all_x Keep unmatched rows from the first input.
#'   [type] arg_boolean()
#' @blockArg all_y Keep unmatched rows from the second input.
#'   [type] arg_boolean()
#'
#' @rdname new_transform_block
#' @export
new_merge_block <- function(by = character(), all_x = FALSE, all_y = FALSE,
                            ...) {

  by_choices <- function(x, y) {
    intersect(colnames(x), colnames(y))
  }

  new_transform_block(
    function(id, x, y) {
      moduleServer(
        id,
        function(input, output, session) {

          sels <- reactiveVal(by)

          allx <- reactiveVal(all_x)
          ally <- reactiveVal(all_y)

          observeEvent(input$by, sels(input$by))

          observeEvent(
            input$type,
            {
              allx("all.x" %in% input$type)
              ally("all.y" %in% input$type)
            },
            ignoreNULL = FALSE,
            ignoreInit = TRUE
          )

          cols <- reactive(by_choices(x(), y()))

          observe(
            updateSelectInput(
              session,
              inputId = "by",
              choices = cols(),
              selected = sels()
            )
          )

          list(
            expr = reactive(
              bbquote(
                merge(.(x), .(y), by = .(cols), all.x = .(allx),
                      all.y = .(ally)),
                list(cols = sels(), allx = allx(), ally = ally())
              )
            ),
            state = list(
              by = sels,
              all_x = allx,
              all_y = ally
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        selectInput(
          inputId = NS(id, "by"),
          label = "By columns",
          choices = list(),
          multiple = TRUE
        ),
        checkboxGroupInput(
          inputId = NS(id, "type"),
          label = "Join type",
          choices = c("all.x", "all.y"),
          selected = c("all.x", "all.y")[c(all_x, all_y)]
        )
      )
    },
    dat_valid = function(x, y) {
      stopifnot(is.data.frame(x), is.data.frame(y))
    },
    expr_type = "bquoted",
    class = "merge_block",
    ...
  )
}
