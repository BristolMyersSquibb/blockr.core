#' Shiny utilities
#'
#' The function `get_session()` wraps [shiny::getDefaultReactiveDomain()].
#'
#' @return Either `NULL` or a shiny session object.
#'
#' @importFrom shiny getDefaultReactiveDomain
#' @export
get_session <- function() {
  getDefaultReactiveDomain()
}
