#' Remove entries from a `reactiveValues` object
#'
#' shiny offers no public way to delete a key from a [shiny::reactiveValues()]
#' object -- assigning `NULL` stores a `NULL` value but leaves the key in
#' `names()`. `trim_rv()` removes the named entries outright and invalidates
#' the affected reactive dependencies, so a key can be truly dropped (and
#' later re-added) -- for instance when a variadic block argument is unlinked.
#'
#' @param x A `reactiveValues` object.
#' @param rm Character vector of keys to remove; all must be present in `x`.
#'
#' @return `x`, invisibly.
#'
#' @export
trim_rv <- function(x, rm) {

  stopifnot(is.reactivevalues(x))

  internals <- .subset2(x, "impl")

  stopifnot(all(rm %in% internals$.nameOrder))

  # No public removal exists, so mirror the internals ReactiveValues$set()
  # touches: drop the value and its name, then invalidate the same
  # dependencies an update would (the key's own, plus names / as-list).
  for (key in rm) {

    internals$.values$remove(key)

    if (internals$.dependents$containsKey(key)) {
      internals$.dependents$get(key)$invalidate()
    }
  }

  internals$.nameOrder <- setdiff(internals$.nameOrder, rm)

  if (isTRUE(internals$.hasRetrieved$names)) {
    internals$.namesDeps$invalidate()
  }

  if (isTRUE(internals$.hasRetrieved$asList)) {
    internals$.valuesDeps$invalidate()
  }

  if (isTRUE(internals$.hasRetrieved$asListAll)) {
    internals$.allValuesDeps$invalidate()
  }

  invisible(x)
}

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
