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

  if (is_string(expr)) {
    expr <- str2lang(expr)
  }

  if (!is.language(expr)) {
    blockr_abort(
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
    external_ctrl = FALSE,
    block_metadata = list(),
    ...
  )
}

#' @export
blockr_ser.fixed_block <- function(x, ...) {
  res <- NextMethod()
  res$payload$expr <- paste(deparse(res$payload$expr), collapse = "\n")
  res
}
