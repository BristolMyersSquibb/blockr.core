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
#' a freshly (re)loaded session. The loader is a `board_loader` object, pairing
#' a `resolve(request)` (read) with a `stage(board, session)` (persist and
#' trigger the reload that `resolve` later picks up) over shared private state.
#' [serve()] pulls the served board's loader once and uses it for both the
#' request-phase resolution and the in-session staging, so the board crossing a
#' reload does not depend on which `preserve_board` the resolved board itself
#' carries. The default loader keeps its state in the app object rather than a
#' process global, so it is per-[serve()] and survives the reload without a
#' shared slot; it is single-process, though, so multi-user deployments supply
#' their own `loader` resolving from a shared backend (as blockr.session does).
#'
#' @param server,ui Server/UI for the plugin module
#' @param loader A `board_loader()` object, or `NULL`
#'
#' @return A plugin container inheriting from `preserve_board` is returned by
#' `preserve_board()`, while the UI component (e.g. `preserve_board_ui()`) is
#' expected to return shiny UI (i.e. [shiny::tagList()]). `board_loader()`
#' returns a `board_loader` object and `is_board_loader()` a scalar logical.
#'
#' @export
preserve_board <- function(server = preserve_board_server,
                           ui = preserve_board_ui,
                           loader = preserve_board_loader()) {

  stopifnot(is.null(loader) || is_board_loader(loader))

  new_plugin(server, ui, class = "preserve_board", loader = loader)
}

#' @param resolve,stage Paired functions backing a `board_loader`: `resolve` is
#' `function(request)` returning a `board` or `NULL`; `stage` is
#' `function(board, session)` which persists `board` and triggers the reload
#' that `resolve` later picks up. They share private state, so a `board_loader`
#' is built as a unit rather than supplied as two loose functions.
#'
#' @rdname preserve_board
#' @export
board_loader <- function(resolve, stage) {

  stopifnot(is.function(resolve), is.function(stage))

  structure(
    list(resolve = resolve, stage = stage),
    class = "board_loader"
  )
}

#' @rdname preserve_board
#' @export
is_board_loader <- function(x) {
  inherits(x, "board_loader")
}

#' @param id Namespace ID
#' @param board Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @rdname preserve_board
#' @export
preserve_board_server <- function(id, board, ..., loader = NULL) {

  dot_args <- list(...)

  moduleServer(
    id,
    function(input, output, session) {

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
          new <- if (is_board(val)) val else val$board

          if (not_null(loader)) {
            loader$stage(new, session)
          }
        }
      )

      invisible()
    }
  )
}

reload_param <- "__blockr_reload__"

#' @rdname preserve_board
#' @export
preserve_board_loader <- function() {

  store <- new.env(parent = emptyenv())

  resolve <- function(request) {

    token <- request$query[[reload_param]]

    if (is.null(token)) {
      return(NULL)
    }

    board <- get0(token, envir = store, inherits = FALSE)

    if (not_null(request$session)) {

      if (not_null(board)) {
        rm(list = token, envir = store)
      }

      strip_reload_token(request$session)
    }

    board
  }

  stage <- function(board, session) {

    token <- rand_names()
    assign(token, board, envir = store)

    query <- session_query(session)
    query[[reload_param]] <- token

    log_debug("staging board for reload handoff {token}")

    updateQueryString(
      query_to_string(query), mode = "replace", session = session
    )
    session$reload()

    invisible(token)
  }

  board_loader(resolve, stage)
}

strip_reload_token <- function(session) {

  query <- session_query(session)
  query[[reload_param]] <- NULL

  updateQueryString(query_to_string(query), mode = "replace", session = session)

  invisible()
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
