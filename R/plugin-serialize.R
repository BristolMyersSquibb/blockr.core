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
#' @param server,ui Server/UI for the plugin module
#'
#' @return A plugin container inheriting from `preserve_board` is returned by
#' `preserve_board()`, while the UI component (e.g. `preserve_board_ui()`) is
#' expected to return shiny UI (i.e. [shiny::tagList()]) and the server
#' component (i.e. `preserve_board_server()`) is expected to return a
#' [shiny::reactiveVal()] or [shiny::reactive()] which evaluates to `NULL` or a
#' `board` object.
#'
#' @export
preserve_board <- function(server = preserve_board_server,
                           ui = preserve_board_ui) {

  new_plugin(server, ui, validator = check_ser_deser_val,
             class = "preserve_board")
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
        do.call(
          restore_board,
          c(
            list(board$board, input$restore$datapath, res),
            dot_args,
            list(session = session)
          )
        )
      )

      res
    }
  )
}

#' @param x The current `board` object
#' @param json JSON serialized board to be restored
#' @param result A [shiny::reactiveVal()] to hold the new board object
#' @param session Shiny session
#'
#' @rdname preserve_board
#' @export
restore_board <- function(x, json, result, ..., session = get_session()) {
  UseMethod("restore_board")
}

#' @export
restore_board.board <- function(x, json, result, ..., session = get_session()) {
  result(from_json(json))
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
serialize_board <- function(x, blocks, ..., session = get_session()) {
  UseMethod("serialize_board")
}

#' @export
serialize_board.board <- function(x, blocks, ..., session = get_session()) {

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

  to_json(x, blocks = blocks, options = opts)
}

write_board_to_disk <- function(rv, ..., session = get_session()) {

  dot_args <- list(...)

  function(con) {

    json <- jsonlite::prettify(
      do.call(
        serialize_board,
        c(
          list(rv$board, rv$blocks),
          dot_args,
          list(session = session)
        )
      )
    )

    writeLines(json, con)
  }
}

check_ser_deser_val <- function(val) {
  observeEvent(
    TRUE,
    {
      if (!is.reactive(val)) {
        abort(
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
      if (!is_board(val())) {
        abort(
          "Expecting the `preserve_board` return value to evaluate to a ",
          "`board` object.",
          class = "preserve_board_return_invalid"
        )
      }

      validate_board(val())
    },
    once = TRUE
  )

  val
}
