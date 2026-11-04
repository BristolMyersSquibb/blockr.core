#' Shiny utilities
#'
#' The function `get_session()` wraps [shiny::getDefaultReactiveDomain()].
#'
#' @return Either `NULL` or a shiny session object.
#'
#' @export
get_session <- function() {
  shiny::getDefaultReactiveDomain()
}
