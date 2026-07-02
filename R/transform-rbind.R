#' @section Row-bind block:
#' Row-wise concatenation of an arbitrary number of `data.frame`s, as performed
#' by [base::rbind()] is available as an `rbind_block`. This mainly serves as
#' an example for a variadic block via the "special" `...args` block data
#' argument.
#'
#' @block rbind block
#' @blockDescr Row-binding of datasets
#' @blockCategory transform
#' @blockIcon chevron-bar-expand
#' @blockGuidance Row-binds two or more inputs with `base::rbind()`; the inputs
#'   must share the same columns. Takes a variable number of data inputs and has
#'   no configurable arguments.
#' @blockKeywords rbind, bind, concatenate, rows, append, union
#'
#' @rdname new_transform_block
#' @export
new_rbind_block <- function(...) {
  new_transform_block(
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
                rbind(..(dat)),
                list(dat = lapply(arg_names(), as_dot_call)),
                splice = TRUE
              )
            ),
            state = list()
          )
        }
      )
    },
    expr_type = "bquoted",
    class = "rbind_block",
    ...
  )
}
