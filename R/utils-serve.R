#' Serve object
#'
#' Intended as entry point to start up an app, the generic function `serve()`
#' dispatches on the object to serve, such as an entire board or a single block
#' (for previewing it during block development). No methods are currently
#' provided.
#'
#' @param x Object
#' @param ... Generic consistency
#'
#' @return A `serve()` method is expected to return an app, such as the result
#' of a call to [shiny::shinyApp()].
#'
#' @export
serve <- function(x, ...) {
  UseMethod("serve")
}
