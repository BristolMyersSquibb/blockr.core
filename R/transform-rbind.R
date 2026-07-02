#' @section Row-bind block:
#' Row-wise concatenation of an arbitrary number of `data.frame`s, as performed
#' by [base::rbind()] is available as an `rbind_block`. This mainly serves as
#' an example for a variadic block via the "special" `...args` block data
#' argument.
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
