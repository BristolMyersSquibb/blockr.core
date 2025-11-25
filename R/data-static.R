#' @section Static block:
#' Mainly useful for testing and examples, this block simply returns the data
#' with which it was initialized. Serialization of static blocks is not allowed
#' and exported code will not be self-contained in the sense that it will not
#' be possible to reproduce results in a script that contains code from a
#' static block.
#'
#' @param data Data (used directly as block result)
#'
#' @rdname new_data_block
#' @export
new_static_block <- function(data, ...) {

  ctor_envir <- environment()

  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(
              bquote(get("data", envir = .(env)), list(env = ctor_envir))
            ),
            state = list(data = data)
          )
        }
      )
    },
    class = "static_block",
    ...
  )
}
