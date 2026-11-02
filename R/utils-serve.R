#' Serve object
#'
#' Intended as entry point to start up an app, the generic function `serve()`
#' dispatches on the object to serve, such as an entire board or a single block
#' (for previewing it during block development). No methods are currently
#' provided.
#'
#' The board options an app offers are collected by `blockr_app_options()`
#' from the board, its blocks and all registered blocks, keeping the first
#' option for each ID.
#'
#' @param x Object
#' @param ... Generic consistency
#'
#' @return A `serve()` method is expected to return an app, such as the result
#' of a call to [shiny::shinyApp()], while `blockr_app_options()` returns a
#' `board_options` object.
#'
#' @export
serve <- function(x, ...) {
  UseMethod("serve")
}

#' @rdname serve
#' @export
blockr_app_options <- function(x) {
  UseMethod("blockr_app_options")
}

#' @export
blockr_app_options.board <- function(x, ...) {
  combine_board_options(
    board_options(x),
    lapply(board_blocks(x), board_options),
    lapply(available_blocks(), board_options)
  )
}
