reorder_rv <- function(x, new) {

  stopifnot(
    is.reactivevalues(x), setequal(new, names(x)), anyDuplicated(new) == 0L
  )

  internals <- .subset2(x, "impl")
  internals$.nameOrder <- new

  invisible(x)
}

make_read_only <- function(x) {

  stopifnot(is.reactivevalues(x))

  res <- unclass(x)
  res[["readonly"]] <- TRUE
  class(res) <- class(x)

  res
}

destroy_outputs <- function(ns_prefix, session = get_session()) {

  for (id in starts_with(list_outputs(session), ns_prefix)) {
    destroy_output(id, session)
  }

  invisible()
}

list_outputs <- function(session = get_session()) {
  coal(
    union(
      names(session$.__enclos_env__$private$.outputs),
      names(session$.__enclos_env__$private$.outputOptions)
    ),
    character()
  )
}

destroy_output <- function(id, session = get_session()) {

  log_trace("destroying output {id}")

  session$defineOutput(id, NULL, NULL)
  session$.__enclos_env__$private$.outputs[[id]] <- NULL
  session$.__enclos_env__$private$.outputOptions[[id]] <- NULL

  invisible()
}

destroy_inputs <- function(ns_prefix, session = get_session()) {

  for (id in starts_with(names(session$input), ns_prefix)) {
    destroy_input(id, session)
  }

  invisible()
}

destroy_input <- function(id, session = get_session()) {

  log_trace("destroying input {id}")

  session$manageInputs(
    set_names(list(NULL), id),
    now = TRUE
  )

  input <- .subset2(session$input, "impl")

  input$.values$remove(id)
  input$.nameOrder <- setdiff(input$.nameOrder, id)

  invisible()
}

invalidate_inputs <- function(session = get_session()) {

  log_trace("invalidating inputs of domain {session$ns(NULL)}")

  input <- .subset2(session$input, "impl")

  input$.namesDeps$invalidate()
  input$.valuesDeps$invalidate()

  invisible()
}

destroy_observers <- function(ns_prefix, session = get_session()) {

  if (isFALSE(blockr_option("observe_hook_disabled", NULL))) {
    return(invisible())
  }

  obs <- get0("observers", envir = session$userData)

  if (is.null(obs)) {
    log_debug("cannot destroy uncaptured observers")
    return(invisible())
  }

  for (i in starts_with(names(obs), ns_prefix)) {

    for (x in obs[[i]]) {

      log_trace(
        "destroying observer {x$.reactId} of domain {x$.domain$ns(NULL)}"
      )

      x$destroy()
    }

    obs[[i]] <- NULL
  }

  assign("observers", obs, envir = session$userData)

  invisible()
}

destroy_module <- function(id, what = c("inputs", "outputs", "observers"),
                           session = get_session()) {

  what <- match.arg(what, several.ok = TRUE)

  log_debug("destroying module {id}, component{?s} {what}")

  ns <- session$ns(id)

  if ("inputs" %in% what) {
    destroy_inputs(id, session)
  }

  if ("outputs" %in% what) {
    destroy_outputs(ns, session)
  }

  if ("observers" %in% what) {
    destroy_observers(ns, session)
  }

  invisible(ns)
}

trace_observe <- function() {

  if (isFALSE(blockr_option("observe_hook_disabled", NULL))) {
    return(invisible())
  }

  if (is_observe_traced()) {
    return(invisible())
  }

  log_trace("hooking observer capturing")

  suppressMessages(
    trace(
      shiny::observe,
      exit = quote(
        {
          if (!is.null(domain)) { # nocov start

            obs <- get0(
              "observers",
              envir = domain$userData,
              inherits = FALSE
            )

            if (is.null(obs)) {
              obs <- list()
            }

            dom <- domain$ns(NULL)

            if (!length(dom)) {
              dom <- ""
            }

            cur <- returnValue()

            log_trace(
              "capturing observer {cur$.reactId} of domain {dom}"
            )

            obs[[dom]] <- c(obs[[dom]], cur)

            assign("observers", obs, envir = domain$userData)
          } # nocov end
        }
      ),
      print = FALSE
    )
  )

  invisible()
}

untrace_observe <- function() {

  if (isFALSE(blockr_option("observe_hook_disabled", NULL))) {
    return(invisible())
  }

  if (!is_observe_traced()) {
    return(invisible())
  }

  log_trace("removing observer capture hook")

  suppressMessages(untrace(shiny::observe))

  invisible()
}

is_observe_traced <- function() {
  inherits(shiny::observe, "functionWithTrace")
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

is_load_alled <- function(pkg = pkg_name()) {

  ns <- .getNamespace(pkg)

  if (is.null(ns)) {
    blockr_abort(
      "Namespace not found for package {pkg}.",
      class = "namespace_not_found"
    )
  }

  ".__DEVTOOLS__" %in% ls(envir = ns, all.names = TRUE)
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

#' @param board A board object
#' @param mode Edit plugins, such as `manage_blocks` get an additional argument
#' `update` over read plugins such as `preserve_board`.
#' @rdname get_session
#' @export
generate_plugin_args <- function(board, ..., mode = c("edit", "read")) {

  session <- edit_plugin_args <- read_plugin_args <- NULL

  mode <- match.arg(mode)

  if (!is_testing() && !is_load_alled()) {
    blockr_warn(
      "`generate_plugin_args()` is intended only for a unit-testing context.",
      class = "generate_plugin_args_not_testing"
    )
  }

  withr::local_envvar(BLOCKR_LOG_LEVEL = "")
  withr::local_options(blockr.log_level = "warn")

  res_plugin_args <- list()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()
      res_plugin_args <<- switch(
        mode,
        edit = edit_plugin_args,
        read = read_plugin_args
      )
    },
    args = list(x = board, ...)
  )

  res_plugin_args
}

#' @param close_button Passed as `closeButton` to [shiny::showNotification()]
#'
#' @inheritParams write_log
#' @inheritParams shiny::showNotification
#'
#' @rdname get_session
#' @export
notify <- function(..., envir = parent.frame(), action = NULL, duration = 5,
                   close_button = TRUE, id = NULL,
                   type = c("message", "warning", "error"),
                   session = get_session()) {

  type <- match.arg(type)

  msg <- glue_plur(..., envir = envir)

  showNotification(
    msg,
    action = action,
    duration = duration,
    closeButton = close_button,
    id = id,
    type = type,
    session = session
  )

  switch(
    type,
    message = log_info(msg, envir = envir, use_glue = FALSE),
    warning = log_warn(msg, envir = envir, use_glue = FALSE),
    error = log_error(msg, envir = envir, use_glue = FALSE)
  )

  invisible(NULL)
}
