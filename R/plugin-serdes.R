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
#' How a board is carried across the `session$reload()` that a restore
#' triggers is the job of a separate, app-level **board loader**, passed to
#' [serve()] as its `loader` argument rather than attached to the plugin. A
#' [board_loader()] pairs a `resolve(query, session)` -- returning the `board`
#' to build for an incoming request, or `NULL` for the [serve()] default --
#' with a `stage(board, session)`, which persists a board and returns the URL
#' query parameters that reference it. [serve()] uses that one loader for both
#' the request-phase resolution (at the GET, where `session` is `NULL`, and at
#' the WS connect) and the in-session staging when a restore fires; **core**
#' writes those parameters into the URL and drives the reload. The plugin
#' server never touches the loader and never reloads, so the reload stays a
#' guaranteed core mechanism. The default [local_loader()] keeps its handoff
#' in a per-loader store (no process global) and is single-process; multi-user
#' deployments pass a loader resolving from a shared backend (as blockr.session
#' does).
#'
#' @param server,ui Server/UI for the plugin module
#'
#' @return A plugin container inheriting from `preserve_board` is returned by
#' `preserve_board()`, while the UI component (e.g. `preserve_board_ui()`) is
#' expected to return shiny UI (i.e. [shiny::tagList()]) and the server
#' component a [shiny::reactiveVal()] evaluating to `NULL` or the `board` to
#' restore. `board_loader()` and `local_loader()` return a `board_loader`
#' object and `is_board_loader()` a scalar logical.
#'
#' @export
preserve_board <- function(server = preserve_board_server,
                           ui = preserve_board_ui) {

  new_plugin(server, ui, validator = check_ser_deser_val,
             class = "preserve_board")
}

#' @param resolve,stage Paired functions backing a `board_loader`: `resolve` is
#' `function(query, session)` returning the `board` to build or `NULL` (the
#' parsed URL `query`, and the `session` -- `NULL` at the GET, set at the WS
#' connect); `stage` is `function(board, session)` which persists `board` and
#' returns the URL query parameters referencing it (core writes them and
#' reloads). They share private state, so a `board_loader` is built as a unit,
#' not supplied as two loose functions.
#'
#' @rdname preserve_board
#' @export
board_loader <- function(resolve, stage) {
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
    has_formals(x$resolve, c("query", "session"))

  if (!resolve_ok) {
    blockr_abort(
      "A `board_loader` `resolve` must be a function of `query` and `session`.",
      class = "board_loader_resolve_invalid"
    )
  }

  stage_ok <- is.function(x$stage) &&
    has_formals(x$stage, c("board", "session"))

  if (!stage_ok) {
    blockr_abort(
      "A `board_loader` `stage` must be a function of `board` and `session`.",
      class = "board_loader_stage_invalid"
    )
  }

  x
}

has_formals <- function(f, args) {
  all(args %in% names(formals(f)))
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
preserve_board_server <- function(id, board, ...) {

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

      res
    }
  )
}

reload_param <- "__blockr_reload__"

#' @rdname preserve_board
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

    token <- rand_names()
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
#' @param session Shiny session
#'
#' @rdname preserve_board
#' @export
restore_board <- function(x, new, result, ..., session = get_session()) {
  UseMethod("restore_board")
}

#' @export
restore_board.board <- function(x, new, result, ..., session = get_session()) {
  result(blockr_deser(new))
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

check_ser_deser_val <- function(val) {
  observeEvent(
    TRUE,
    {
      if (!is.reactive(val)) {
        blockr_abort(
          "Expecting `preserve_board` to return a reactive value.",
          class = "preserve_board_return_invalid"
        )
      }
    },
    once = TRUE
  )

  observeEvent(
    val(),
    {
      board <- val()

      if (!is_board(board)) {
        blockr_abort(
          "Expecting the `preserve_board` return value to evaluate to a ",
          "`board` object.",
          class = "preserve_board_return_invalid"
        )
      }

      validate_board(board)
    },
    once = TRUE
  )

  val
}
