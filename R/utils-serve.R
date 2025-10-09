#' Serve object
#'
#' Intended as entry point to start up a shiny app, the generic function
#' `serve()` can be dispatched either on a single block (mainly for previewing
#' purposes during block development) or an entire board
#'
#' @param x Object
#' @param ... Generic consistency
#'
#' @examplesShinylive
#' webr::install("blockr.core", repos = "https://cynkra.github.io/blockr.webR/")
#' library(blockr.core)
#' serve(
#'   new_merge_block("Time"),
#'   data = list(
#'     x = datasets::BOD,
#'     y = datasets::ChickWeight
#'   )
#' )
#'
#' @return The generic `serve()` is expected to return the result of a call to
#' [shiny::shinyApp()].
#'
#' @export
serve <- function(x, ...) {
  update_serve_obj(x)
  UseMethod("serve")
}

#' @param id Block ID
#' @param data Data inputs
#' @rdname serve
#' @export
serve.block <- function(x, id = "block", ..., data = list()) {

  init_data <- function(x, is_variadic) {
    if (is_variadic) do.call(reactiveValues, x) else reactiveVal(x)
  }

  if (...length() && !length(data)) {
    data <- list(...)
  }

  dot_args <- !names(data) %in% block_inputs(x)

  if (!is.na(block_arity(x)) && any(dot_args)) {
    blockr_abort(
      "Unexpected arguments {names(data)[dot_args]}.",
      class = "unexpected_var_args"
    )
  }

  if (any(dot_args)) {
    data <- c(data[!dot_args], list(...args = data[dot_args]))
  }

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    title = id,
    expr_ui(id, x),
    block_ui(id, x)
  )

  server <- function(input, output, session) {

    res <- block_server(id, x, Map(init_data, data, names(data) == "...args"))

    exportTestValues(
      result = safely_export(res$result())()
    )

    invisible()
  }

  shinyApp(ui, server)
}

#' @param id Board namespace ID
#' @param plugins Board plugins
#'
#' @rdname serve
#'
#' @examplesShinylive
#' webr::install("blockr.core", repos = "https://cynkra.github.io/blockr.webR/")
#' library(blockr.core)
#' serve(
#'   new_board(
#'     blocks = c(
#'       a = new_dataset_block("BOD"),
#'       b = new_dataset_block("ChickWeight"),
#'       c = new_merge_block("Time")
#'     ),
#'     links = c(
#'       ac = new_link("a", "c", "x"),
#'       bc = new_link("b", "c", "y")
#'     ),
#'     stacks = list(ac = c("a", "c"))
#'   )
#' )
#'
#' @export
serve.board <- function(x, id = rand_names(), plugins = board_plugins(x),
                        ...) {

  args <- list(...)

  ui <- function() {

    log_debug("building ui for board {id}")

    bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      board_ui(id, get_serve_obj(), plugins)
    )
  }

  server <- function(input, output, session) {

    trace_observe()
    onStop(untrace_observe, session)

    res <- do.call(
      board_server,
      c(
        list(id, get_serve_obj(), plugins),
        args
      )
    )

    exportTestValues(
      result = lapply(
        lapply(
          lapply(lst_xtr(res[[1L]]$blocks, "server", "result"), safely_export),
          reval
        ),
        reval
      )
    )

    invisible()
  }

  shinyApp(ui, server)
}

serve_obj <- new.env()

update_serve_obj <- function(x) {
  assign("x", x, envir = serve_obj)
  invisible(x)
}

#' @rdname serve
#' @export
get_serve_obj <- function() {
  get("x", envir = serve_obj, inherits = FALSE)
}
