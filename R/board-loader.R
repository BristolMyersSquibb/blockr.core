#' Board loader
#'
#' Which `board` to build for an incoming request -- and how a board is carried
#' across the `session$reload()` that a [preserve_board] restore triggers -- is
#' the job of an app-level **board loader**, passed to [serve()] as its
#' `loader` argument. A `board_loader()` pairs a `resolve(query, session)` --
#' returning the `board` to build for an incoming request, or `NULL` for the
#' [serve()] default -- with an optional `stage(board, session)`, which
#' persists a board and returns the URL query parameters that reference it (a
#' resolve-only loader, e.g. one not backing a restore, leaves `stage` `NULL`).
#' [serve()] uses that
#' one loader for both the request-phase resolution (at the GET, where
#' `session` is `NULL`, and at the WS connect) and the in-session staging when
#' a restore fires; **core** writes those parameters into the URL and drives
#' the reload, so the reload stays a guaranteed core mechanism that no loader
#' can opt out of.
#'
#' The default [local_loader()] keeps its handoff in a per-loader store (no
#' process global) and is therefore single-process; multi-user deployments pass
#' a loader resolving from a shared backend (as blockr.session does).
#'
#' @param resolve,stage Paired functions backing a `board_loader`: `resolve` is
#' `function(query, session)` returning the `board` to build or `NULL` (the
#' parsed URL `query`, and the `session` -- `NULL` at the GET, set at the WS
#' connect); `stage` is `function(board, session)` (or `NULL` for a loader that
#' does not stage, e.g. resolve-only) which persists `board` and returns the
#' URL query parameters referencing it (core writes them and reloads). They
#' share private state, so a `board_loader` is built as a unit, not supplied as
#' two loose functions.
#' @param x Object to test for `board_loader`-ness
#'
#' @return `board_loader()` and `local_loader()` return a `board_loader` object
#' and `is_board_loader()` a scalar logical.
#'
#' @export
board_loader <- function(resolve, stage = NULL) {
  validate_board_loader(
    structure(
      list(resolve = resolve, stage = stage),
      class = "board_loader"
    )
  )
}

validate_board_loader <- function(x) {

  if (!is_board_loader(x)) {
    blockr_abort(
      "Expecting a `board_loader` object.",
      class = "board_loader_invalid"
    )
  }

  resolve_ok <- is.function(x$resolve) &&
    formals_match(x$resolve, c("query", "session"))

  if (!resolve_ok) {
    blockr_abort(
      "A `board_loader` `resolve` must be a function of `query` and `session`.",
      class = "board_loader_resolve_invalid"
    )
  }

  stage_ok <- is.null(x$stage) ||
    (is.function(x$stage) && formals_match(x$stage, c("board", "session")))

  if (!stage_ok) {
    blockr_abort(
      "A `board_loader` `stage` must be `NULL` or a function of `board` and ",
      "`session`.",
      class = "board_loader_stage_invalid"
    )
  }

  x
}

formals_match <- function(f, args) {
  identical(names(formals(f)), args)
}

#' @rdname board_loader
#' @export
is_board_loader <- function(x) {
  inherits(x, "board_loader")
}

reload_param <- "__blockr_reload__"

#' @rdname board_loader
#' @export
local_loader <- function() {

  store <- new.env(parent = emptyenv())

  resolve <- function(query, session) {

    token <- query[[reload_param]]

    if (is.null(token)) {
      return(NULL)
    }

    board <- get0(token, envir = store, inherits = FALSE)

    if (not_null(session)) {

      if (not_null(board)) {
        rm(list = token, envir = store)
      }

      strip_reload_token(session)
    }

    board
  }

  stage <- function(board, session) {

    token <- rand_names(ls(envir = store))
    assign(token, board, envir = store)

    log_debug("staging board for reload handoff {token}")

    set_names(list(token), reload_param)
  }

  board_loader(resolve, stage)
}

strip_reload_token <- function(session) {

  query <- session_query(session)
  query[[reload_param]] <- NULL

  updateQueryString(query_to_string(query), mode = "replace", session = session)

  invisible()
}

resolve_board <- function(default, loader, query, session) {

  board <- loader$resolve(query, session)

  if (is.null(board)) {
    return(default)
  }

  if (!is_board(board)) {
    blockr_abort(
      "A board loader's `resolve` must return `NULL` or a `board`.",
      class = "invalid_board_loader"
    )
  }

  validate_board(board)
}

session_query <- function(session) {
  parseQueryString(coal(isolate(session$clientData$url_search), ""))
}

reload_board <- function(loader, board, session) {

  if (not_null(loader$stage)) {

    params <- loader$stage(board, session)

    if (length(params)) {
      query <- modifyList(session_query(session), as.list(params))
      updateQueryString(query_to_string(query), mode = "replace",
                        session = session)
    }
  }

  log_debug("reloading session")
  session$reload()

  invisible()
}

query_to_string <- function(query) {

  if (!length(query)) {
    return("?")
  }

  nms <- chr_ply(names(query), utils::URLencode, reserved = TRUE)
  vals <- chr_ply(
    unlist(query, use.names = FALSE), utils::URLencode, reserved = TRUE
  )

  paste0("?", paste(nms, vals, sep = "=", collapse = "&"))
}
