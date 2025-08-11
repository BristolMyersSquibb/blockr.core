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

  input <- .subset2(session$input, "impl")

  input$.namesDeps$invalidate()
  input$.valuesDeps$invalidate()

  invisible()
}

destroy_observers <- function(ns_prefix, session = get_session()) {

  obs <- get("observers", envir = session$userData)

  for (i in starts_with(names(obs), ns_prefix)) {

    for (x in obs[[i]]) {
      x$destroy()
    }

    obs[[i]] <- NULL
  }

  assign("observers", obs, envir = session$userData)

  invisible()
}

#' Shiny utilities
#'
#' Utility functions for shiny:
#' - `get_session`: See [shiny::getDefaultReactiveDomain()].
#'
#' @return Either `NULL` or a shiny session object.
#'
#' @export
get_session <- function() {
  getDefaultReactiveDomain()
}
