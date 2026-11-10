#' Serve a board or a block
#'
#' The generic `serve()` starts an app, as an app script does at its top level.
#' Its methods take a board, the path of a JSON board, or a single block, and
#' return a Shiny app.
#' The app's page holds core's client, which draws the board with the board's
#' frontend, or with core's basic frontend where the board holds none.
#' The app's server function hands what the client sends to the evaluator of
#' the session, which evaluates the blocks.
#'
#' For a board, R writes the board's JSON once, as `serve()` is called, and
#' sends it to the client as each session starts.
#' So a board that R cannot write as JSON fails as the script runs (see
#' [blockr_ser()]).
#' For the path of a JSON board, R reads only the file's format, and sends the
#' file's text as it is, for the client to open.
#' It loads no package for the file.
#' The board's ID, its plugins and its board options come from the board (see
#' [new_board()]), and the client reads the query of the page's URL itself.
#' Where the board lock and the load lock are both on, R reads the board's
#' blocks and links once, as `serve()` is called, and holds every evaluation
#' to them.
#'
#' A single block is served alone, with fixed data and no board around it, so
#' that its developer can try it out.
#' The page shows the block in a card of its own: its name, its UI, its output
#' and a line of its status.
#' R keeps the data for the process, and binds each entry to the block's input
#' of that name.
#' For a variadic block, an entry whose name is none of the block's inputs
#' goes into the block's `...`, under that name.
#' So each entry needs a name, as in
#' `serve(new_rbind_block(), a = iris, b = mtcars)`.
#'
#' @param x A board, the path of a JSON board, or a block
#' @param ... For a block, its data, where `data` is empty
#'
#' @examples
#' if (interactive()) {
#'   serve(
#'     new_board(
#'       blocks = c(
#'         a = new_dataset_block("BOD"),
#'         b = new_dataset_block("ChickWeight"),
#'         c = new_merge_block("Time")
#'       ),
#'       links = c(
#'         ac = new_link("a", "c", "x"),
#'         bc = new_link("b", "c", "y")
#'       )
#'     )
#'   )
#'
#'   serve(
#'     new_merge_block("Time"),
#'     data = list(x = datasets::BOD, y = datasets::ChickWeight)
#'   )
#' }
#'
#' @return A Shiny app, as [shiny::shinyApp()] returns it.
#'
#' @export
serve <- function(x, ...) {
  UseMethod("serve")
}

#' @rdname serve
#' @export
serve.board <- function(x, ...) {
  check_board_args(substitute(list(...)), "a board")
  serve_app(encode_message(blockr_ser(x)))
}

#' @rdname serve
#' @export
serve.character <- function(x, ...) {
  check_board_args(substitute(list(...)), "the path of a JSON board")
  serve_app(read_board_json(x))
}

#' @param data The block's data, a list by input
#' @rdname serve
#' @export
serve.block <- function(x, ..., data = list()) {

  if (...length() && length(data)) {
    blockr_abort(
      "R takes a block's data from `data`, or from `...` where `data` is ",
      "empty, and it was given both.",
      class = "serve_data_invalid"
    )
  }

  if (...length()) {
    data <- list(...)
  }

  check_block_data(x, data)

  serve_app(encode_message(blockr_ser(new_board(x))), data, alone = TRUE)
}

# The app that `serve()` returns, for the opening board given as JSON, and
# for a block that `serve()` serves alone, with the data that R binds to the
# block's inputs.
# R writes or reads the opening board here, as `serve()` is called, rather than
# as a session starts, and so reads it for the locks.
# Every session's evaluator holds its evaluations to what R read.
serve_app <- function(board, data = list(), alone = FALSE) {

  force(board)

  lock <- lock_board(board, data)

  shiny::shinyApp(
    ui = function(req) serve_page(alone),
    server = function(input, output, session) {

      ev <- new_evaluator(board, data, lock)

      shiny_transport(
        function(message, send) receive_requests(ev, message, send),
        session
      )
    }
  )
}

# The page of the app that `serve()` returns, which holds no Bootstrap.
# A browser takes an import map only ahead of the page's first module script,
# so the page names the map's dependency first, and Shiny puts it in the head
# after its own scripts, which are no modules.
# The page then holds core's stylesheet, the element that the client draws
# in, and the module script that starts the client with `startPage()`.
# R makes the page for each request, so that its URLs name the files that R
# serves now.
serve_page <- function(alone = FALSE) {

  runtime <- served_module(core_runtime())

  shiny::tagList(
    import_map_dependency(),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = runtime[["css"]])
    ),
    shiny::tags$div(id = "blockr-page"),
    shiny::tags$script(
      type = "module",
      shiny::HTML(page_script(runtime[["module"]], alone))
    )
  )
}

# The import map as an HTML dependency, which puts it in the page's head.
import_map_dependency <- function() {
  htmltools::htmlDependency(
    "blockr-import-map",
    as.character(utils::packageVersion("blockr.core")),
    src = c(href = ""),
    head = as.character(import_map())
  )
}

# The module script that imports `startPage()` from core's runtime, at the URL
# given, and starts the client in the page's element.
# The client draws a block that `serve()` serves alone where `alone` is `TRUE`.
page_script <- function(runtime, alone = FALSE) {
  paste0(
    "import { startPage } from \"./", runtime, "\";\n",
    "startPage({ el: document.getElementById(\"blockr-page\")",
    if (alone) ", alone: true" else "",
    " });\n"
  )
}

# Refuses every argument but `x` of the method of `serve()` for a board or for
# the path of a JSON board.
# The arguments come as the call `list(...)`, unevaluated, so that one that
# names something gone, such as `local_loader()`, fails here too.
# The error says where each argument of today's `serve()` went.
check_board_args <- function(args, what) {

  args <- as.list(args)[-1L]

  if (!length(args)) {
    return(invisible())
  }

  nms <- coal(names(args), character(length(args)), fail_all = FALSE)

  moved <- c(
    id = "A board holds its ID, which `new_board()` takes as `id`.",
    plugins = paste(
      "A board holds its plugins, which `new_board()` takes as",
      "`plugins`."
    ),
    options = paste(
      "A board holds the values of its board options, which `new_board()`",
      "takes as `options`."
    ),
    loader = "The client opens and saves boards itself, with no loader."
  )

  moved <- moved[intersect(names(moved), nms)]

  # The arguments as the error names them.
  nms <- ifelse(nzchar(nms), paste0("`", nms, "`"), "an unnamed argument")

  blockr_abort(
    "For {what}, `serve()` takes no argument but `x`, and it was given ",
    "{nms}.",
    if (length(moved)) paste0(" ", paste(moved, collapse = " ")) else "",
    class = "serve_args_invalid"
  )
}

# Checks the data that R binds to the inputs of a block that `serve()` serves
# alone: a list in which each entry has a name of its own.
# Each name is an input of the block, or any name for a variadic block, whose
# `...` takes the entries that name none of its inputs.
check_block_data <- function(x, data) {

  if (!is.list(data) || is.object(data)) {
    blockr_abort(
      "The data of a block that `serve()` serves alone is a list by input, as ",
      "in `data = list(data = iris)`.",
      class = "serve_data_invalid"
    )
  }

  nms <- coal(names(data), character(length(data)), fail_all = FALSE)
  unnamed <- sum(is.na(nms) | !nzchar(nms))
  inputs <- block_data_inputs(x)
  variadic <- "..." %in% inputs

  if (unnamed) {
    blockr_abort(
      "R binds each entry of a block's data to the block's input of that ",
      "name, and {unnamed} entr{?y/ies} {?has/have} no name.",
      if (variadic) {
        paste0(
          " A variadic block takes its data under any names, as in ",
          "`serve(new_rbind_block(), a = iris, b = mtcars)`."
        )
      } else {
        ""
      },
      class = "serve_data_invalid"
    )
  }

  twice <- unique(nms[duplicated(nms)])

  if (length(twice)) {
    blockr_abort(
      "R binds each entry of a block's data to the block's input of that ",
      "name, and the data names {twice} more than once.",
      class = "serve_data_invalid"
    )
  }

  unknown <- setdiff(nms, inputs)

  if (variadic || !length(unknown)) {
    return(invisible(data))
  }

  if (!length(inputs)) {
    blockr_abort(
      "The block has no inputs, so it takes no data, and it was given ",
      "{unknown}.",
      class = "serve_data_invalid"
    )
  }

  blockr_abort(
    "The block has no {qty(unknown)}input{?s} {unknown}. ",
    "Its {qty(inputs)}input{?s} {?is/are} {inputs}.",
    class = "serve_data_invalid"
  )
}
