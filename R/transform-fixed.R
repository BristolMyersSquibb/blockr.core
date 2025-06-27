#' @section Fixed block:
#' Mainly useful for testing and examples, this block applies a fixed
#' transformation to its data input. No UI elements are exposed and the
#' transformation consequently cannot be parametrized. The quoted expression
#' passed as `expr` is expected to refer to the input data as `data`.
#'
#' @param expr Quoted expression
#'
#' @rdname new_transform_block
#' @export
new_fixed_block <- function(expr, ...) {

  if (!is.language(expr)) {
    abort(
      "Expecting `expr` to be of type \"language\".",
      class = "invalid_block_input"
    )
  }

  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(expr),
            state = list(expr = expr)
          )
        }
      )
    },
    class = "fixed_block",
    allow_empty_state = TRUE,
    ...
  )
}
