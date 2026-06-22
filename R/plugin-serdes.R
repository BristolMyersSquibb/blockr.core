#' Serialization plugin module
#'
#' Board state can be preserved by serializing all contained objects and
#' restored via de-serialization. This mechanism can be used to power features
#' such as save/restore (via download, as implemented in the default
#' `preserve_board` plugin), but more refined user experience is conceivable
#' in terms of undo/redo functionality and (automatic) saving of board state.
#' Such enhancements can be implemented in a third-party `preserve_board`
#' module.
#'
#' Unlike other plugins, `preserve_board` additionally carries a `loader`: a
#' function of a single request argument that core calls when building the
#' board UI (at the GET) and server (at the WS connect), returning the `board`
#' to build or `NULL` to fall back to the `serve()` default. The request
#' exposes the parsed URL `query` (normalized across both call sites) and the
#' raw `request`/`session`; note `session` is `NULL` at the GET (there is no
#' session yet), so a loader must tolerate its absence there. Core validates
#' the returned value, which must be `NULL` or a `board`.
#'
#' This is how a restored (or otherwise externally resolved) board is handed to
#' a freshly (re)loaded session. The default trio (`preserve_board_server()` /
#' `preserve_board_loader()`) implements save/restore: the server stages the
#' deserialized board and triggers a [shiny::reactiveVal()] reload, the loader
#' returns it on the next request. A third-party module may replace any of the
#' three to source the board from elsewhere (e.g. a database). The default
#' handoff is a single process-global slot and so is single-tab /
#' preview-grade: concurrent restores in one process can clobber one another.
#' Multi-user deployments should supply their own `loader` (e.g. keyed per
#' user/session), as blockr.session does.
#'
#' @param server,ui Server/UI for the plugin module
#' @param loader Request-phase board loader (see [new_plugin()])
#'
#' @return A plugin container inheriting from `preserve_board` is returned by
#' `preserve_board()`, while the UI component (e.g. `preserve_board_ui()`) is
#' expected to return shiny UI (i.e. [shiny::tagList()]) and the loader
#' component (i.e. `preserve_board_loader()`) is expected to return `NULL` or a
#' `board` object.
#'
#' @export
preserve_board <- function(server = preserve_board_server,
                           ui = preserve_board_ui,
                           loader = preserve_board_loader) {

  stopifnot(is.null(loader) || is.function(loader))

  new_plugin(server, ui, class = "preserve_board", loader = loader)
}

board_loader <- function(x) attr(x, "loader")

#' @param id Namespace ID
#' @param board Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @rdname preserve_board
#' @export
preserve_board_server <- function(id, board, ...) {

  dot_args <- list(...)

  moduleServer(
    id,
    function(input, output, session) {

      consume_reload_handoff(session)

      output$serialize <- downloadHandler(
        board_filename(board),
        do.call(
          write_board_to_disk,
          c(list(board), dot_args, list(session = session))
        )
      )

      res <- reactiveVal()

      observeEvent(
        input$restore,
        {
          board_ser <- read_json(input$restore$datapath)

          do.call(
            restore_board,
            c(
              list(board$board, board_ser, res),
              dot_args,
              list(session = session)
            )
          )
        }
      )

      observeEvent(
        res(),
        {
          val <- res()

          stage_reload_handoff(if (is_board(val)) val else val$board, session)
        }
      )

      invisible()
    }
  )
}

reload_param <- "__blockr_reload__"

reload_handoff <- new.env(parent = emptyenv())

stage_reload_handoff <- function(board, session) {

  token <- rand_names()
  assign(token, board, envir = reload_handoff)

  query <- session_query(session)
  query[[reload_param]] <- token

  log_debug("staging board for reload handoff {token}")

  updateQueryString(query_to_string(query), mode = "replace", session = session)
  session$reload()

  invisible(token)
}

consume_reload_handoff <- function(session) {

  query <- session_query(session)
  token <- query[[reload_param]]

  if (is.null(token)) {
    return(invisible())
  }

  if (exists(token, envir = reload_handoff, inherits = FALSE)) {
    rm(list = token, envir = reload_handoff)
  }

  query[[reload_param]] <- NULL
  updateQueryString(query_to_string(query), mode = "replace", session = session)

  invisible()
}

#' @param request Request passed by core at the UI (GET) and server (WS connect)
#' entry points: a list with the parsed URL `query`, the raw `request`, and
#' `session` (`NULL` at the GET, so a loader must tolerate its absence). The
#' default loader reads the one-time handoff token from `query` and returns the
#' board staged for it (or `NULL`).
#'
#' @rdname preserve_board
#' @export
preserve_board_loader <- function(request) {

  token <- request$query[[reload_param]]

  if (is.null(token)) {
    return(NULL)
  }

  get0(token, envir = reload_handoff, inherits = FALSE)
}

read_json <- function(x) {
  jsonlite::fromJSON(
    x,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
}

#' @param x The current `board` object
#' @param new Serialized (list-based) representation of the new board
#' @param result A [shiny::reactiveVal()] to hold the new board object
#' @param meta Optional named list of plugin metadata to persist across
#'   the reload triggered by board restoration
#' @param session Shiny session
#'
#' @rdname preserve_board
#' @export
restore_board <- function(x, new, result, ..., meta = NULL,
                          session = get_session()) {
  UseMethod("restore_board")
}

#' @export
restore_board.board <- function(x, new, result, ..., meta = NULL,
                                session = get_session()) {

  board <- blockr_deser(new)

  if (is.null(meta)) {
    result(board)
  } else {
    result(list(board = board, meta = meta))
  }
}

#' @param board The initial `board` object
#' @rdname preserve_board
#' @export
preserve_board_ui <- function(id, board) {
  tagList(
    downloadButton(
      NS(id, "serialize"),
      "Save"
    ),
    htmltools::tagQuery(
      fileInput(
        NS(id, "restore"),
        "",
        buttonLabel = tagList(icon("upload"), "Restore")
      )
    )$addAttrs(
      style = "margin-bottom: 8px;"
    )$allTags()
  )
}

board_filename <- function(rv) {
  function() {
    paste0(
      rv$board_id,
      "_",
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
      ".json"
    )
  }
}

#' @param blocks Block state reactive values
#' @rdname preserve_board
#' @export
serialize_board <- function(x, blocks, id = NULL, ...,
                            session = get_session()) {

  UseMethod("serialize_board")
}

#' @export
serialize_board.board <- function(x, blocks, id = NULL, ...,
                                  session = get_session()) {

  blocks <- lapply(
    lst_xtr(blocks, "server", "state"),
    lapply,
    reval_if
  )

  opts <- lapply(
    set_names(nm = names(as_board_options(x))),
    get_board_option_or_null,
    session
  )

  blockr_ser(x, board_id = id, blocks = blocks, options = opts)
}

write_board_to_disk <- function(rv, ..., session = get_session()) {

  dot_args <- list(...)

  function(con) {

    json <- write_json(
      do.call(
        serialize_board,
        c(
          list(rv$board, rv$blocks, rv$board_id),
          dot_args,
          list(session = session)
        )
      )
    )

    writeLines(json, con)
  }
}

write_json <- function(x) {
  jsonlite::toJSON(x, null = "null")
}
