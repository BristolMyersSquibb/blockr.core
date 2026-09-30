make_read_only <- function(x) {

  stopifnot(is.reactivevalues(x))

  res <- unclass(x)
  res[["readonly"]] <- TRUE
  class(res) <- class(x)

  res
}

#' Shiny utilities
#'
#' Utility functions for shiny:
#' - `get_session`: See [shiny::getDefaultReactiveDomain()].
#' - `generate_plugin_args`: Meant for unit testing plugins.
#' - `notify`: Glue-capable wrapper for [shiny::showNotification()].
#'
#' @return Either `NULL` or a shiny session object for `get_session()`, a list
#' of arguments for plugin server functions in the case of
#' `generate_plugin_args()` and `notify()` is called for the side-effect of
#' displaying a browser notification (and returns `NULL` invisibly).
#'
#' @export
get_session <- function() {
  getDefaultReactiveDomain()
}

#' @param close_button Passed as `closeButton` to [shiny::showNotification()]
#' @param glue,log Whether to [glue::glue()]-interpolate `...` and whether to
#' emit a log message. Set both to `FALSE` to surface pre-formatted,
#' already-logged text (e.g. a captured condition message, which may contain
#' braces that would otherwise fail interpolation).
#'
#' @inheritParams write_log
#' @inheritParams shiny::showNotification
#'
#' @rdname get_session
#' @export
notify <- function(..., envir = parent.frame(), action = NULL, duration = 5,
                   close_button = TRUE, id = NULL,
                   type = c("message", "warning", "error"),
                   glue = TRUE, log = TRUE, session = get_session()) {

  type <- match.arg(type)

  msg <- if (glue) glue_plur(..., envir = envir) else paste0(...)
  msg <- HTML(cli::ansi_html(msg))

  showNotification(
    msg,
    action = action,
    duration = duration,
    closeButton = close_button,
    id = id,
    type = type,
    session = session
  )

  if (log) {
    switch(
      type,
      message = log_info(msg, envir = envir, use_glue = FALSE),
      warning = log_warn(msg, envir = envir, use_glue = FALSE),
      error = log_error(msg, envir = envir, use_glue = FALSE)
    )
  }

  invisible(NULL)
}

notify_remove <- function(id, session = get_session()) {
  removeNotification(id, session = session)
}
