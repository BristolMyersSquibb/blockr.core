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
#' Before it writes the board, R registers the class of each block on the
#' board that the block registry lacks, under the registry ID that the class
#' gives (see [register_block()]).
#' So every block on the served board runs, and so does a block that `serve()`
#' serves alone.
#' A class from a package keeps its package's ID, so that a board saved from
#' the app opens in an app that offers the package.
#' A class from outside a package, whose registry ID is its id alone,
#' replaces another class that the registry holds under that ID, so that a
#' script that redefines a class and serves again runs the new definition.
#' A board with blocks of two such classes of one id fails, since the client
#' and the evaluator know a block's class by its registry ID alone.
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
#' @section End-to-end tests:
#' In Shiny's test mode, as the package shinytest2 starts an app, the app gives
#' end-to-end tests what they check.
#' R exports `result` through [shiny::exportTestValues()], which a test reads
#' with `AppDriver$get_values(export = TRUE)`.
#' For a board, `result` holds the latest result of each block, by block ID.
#' For a block that `serve()` serves alone, `result` is the block's result.
#' R holds no result of a block whose latest evaluation failed, so `result`
#' leaves that block out.
#' The client holds the board and each block's status.
#' So the page puts the object `blockrTest` on `window`, which a test reads
#' with `AppDriver$get_js()`.
#' Its function `board()` gives a copy of the board that the client holds, and
#' its function `statuses()` gives each block's status by block ID, as in
#' `app$get_js("blockrTest.statuses()")`.
#'
#' @param x A board, the path of a JSON board, or a block
#' @param ... For a block, its data where `data` is empty.
#'   R ignores it otherwise, with a warning.
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
  register_board_classes(x)
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

  dots <- dots_names(substitute(list(...)))

  if (length(dots) && length(data)) {
    dots <- arg_labels(dots)
    blockr_warn(
      "R takes the block's data from `data`, and ignores {dots} in `...`. ",
      "It takes data from `...` only where `data` is empty.",
      class = "serve_dots_ignored"
    )
  } else if (length(dots)) {
    data <- list(...)
  }

  check_block_data(x, data)

  board <- new_board(x)
  register_board_classes(board)

  serve_app(encode_message(blockr_ser(board)), data, alone = TRUE)
}

# Registers the class of each block on a board that the block registry lacks,
# under the registry id that the class gives, so that every block on the
# served board runs. A class from a package keeps its package's id. Core's
# static and fixed blocks get theirs this way, since core does not register
# them as it loads.
# A class from outside a package has no package that checks the ids of its
# classes, so R checks it here. It replaces another class that the registry
# holds under its id, as a class does that the script has redefined since an
# earlier `serve()`, and two such classes of one id on a board fail.
register_board_classes <- function(board) {

  classes <- lapply(as.list(board_blocks(board)), block_class)

  check_outside_classes(classes)

  for (cls in classes) {

    id <- class_registry_id(cls)
    old <- if (id %in% list_blocks()) get_registry_entry(id)[["block_class"]]

    if (is.null(old) || (is.null(cls[["package"]]) && !identical(old, cls))) {
      register_block(
        block_constructor(cls),
        default_block_name(cls[["class"]]),
        "",
        overwrite = TRUE
      )
    }
  }

  invisible()
}

# Two classes from outside a package with one id give one registry id, the
# class id, under which the client and the evaluator know only one class. The
# classes are given by block id.
check_outside_classes <- function(classes) {

  outside <- lgl_ply(classes, function(cls) is.null(cls[["package"]]))
  ids <- chr_ply(classes, class_registry_id)

  for (id in unique(ids[outside])) {

    same <- classes[outside & ids == id]
    other <- names(same)[!lgl_ply(same, identical, same[[1L]])]

    if (length(other)) {
      blockr_abort(
        "Blocks {names(same)[1L]} and {other[1L]} are of two classes with the ",
        "id {id}, defined outside a package, which give one registry id.",
        class = "board_blocks_invalid"
      )
    }
  }

  invisible()
}

# The app that `serve()` returns, for the opening board given as JSON, and
# for a block that `serve()` serves alone, with the data that R binds to the
# block's inputs.
# R writes or reads the opening board here, as `serve()` is called, rather than
# as a session starts, and so reads it for the locks.
# Every session's evaluator holds its evaluations to what R read.
# The server function exports the latest results of the session's evaluator
# for end-to-end tests.
# Shiny answers for exported values only in test mode.
serve_app <- function(board, data = list(), alone = FALSE) {

  force(board)

  lock <- lock_board(board, data)

  shiny::shinyApp(
    ui = function(req) serve_page(alone),
    server = function(input, output, session) {

      ev <- new_evaluator(board, data, lock)

      shiny::exportTestValues(result = latest_results(ev, alone))

      shiny_transport(
        function(message, send) receive_requests(ev, message, send),
        session
      )
    }
  )
}

# The latest result of each block that the evaluator holds one for, by block
# ID, as R exports them for end-to-end tests.
# For a block that `serve()` serves alone, it is that block's result,
# or `NULL` before R holds one.
# R holds no result of a block whose latest evaluation failed.
latest_results <- function(ev, alone = FALSE) {

  ids <- ls(ev$results, all.names = TRUE)
  res <- lst_xtr(mget(ids, envir = ev$results), "result")

  if (alone) {
    return(if (length(res)) res[[1L]])
  }

  res
}

# The page of the app that `serve()` returns, which holds no Bootstrap.
# A browser takes an import map only ahead of the page's first module script,
# so the page names the map's dependency first, and Shiny puts it in the head
# after its own scripts, which are no modules.
# The page then holds core's stylesheet, the element that the client draws
# in, and the module script that starts the client with `startPage()`.
# R makes the page for each request, so that its URLs name the files that R
# serves now.
# In Shiny's test mode, as shinytest2 starts an app, the module script also
# imports `blockrTest()` from core's module for end-to-end tests, and puts what
# it returns on `window`, for the tests to read.
# A page outside test mode loads none of that module.
serve_page <- function(alone = FALSE,
                       test = isTRUE(shiny::getShinyOption("testmode"))) {

  runtime <- served_module(core_runtime())

  script <- page_script(
    runtime[["module"]],
    alone,
    if (test) {
      served_url(js_module("js/blockr-test.js", package = "blockr.core"))
    }
  )

  shiny::tagList(
    import_map_dependency(),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = runtime[["css"]])
    ),
    shiny::tags$div(id = "blockr-page"),
    shiny::tags$script(type = "module", shiny::HTML(script))
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
# Where `test` gives the URL of core's module for end-to-end tests, the script
# also imports `blockrTest()` from it, and puts what that returns on `window`
# as `blockrTest`.
page_script <- function(runtime, alone = FALSE, test = NULL) {

  start <- paste0(
    "startPage({ el: document.getElementById(\"blockr-page\")",
    if (alone) ", alone: true" else "",
    " })"
  )

  paste0(
    "import { startPage } from \"./", runtime, "\";\n",
    if (is.null(test)) {
      paste0(start, ";\n")
    } else {
      paste0(
        "import { blockrTest } from \"./", test, "\";\n",
        "const started = ", start, ";\n",
        "window.blockrTest = blockrTest(() => started.core());\n"
      )
    }
  )
}

# Refuses every argument but `x` of the method of `serve()` for a board or for
# the path of a JSON board.
# The arguments come as the call `list(...)`, unevaluated, so that one that
# names something gone, such as `local_loader()`, fails here too.
# The error says where each argument of today's `serve()` went.
check_board_args <- function(args, what) {

  nms <- dots_names(args)

  if (!length(nms)) {
    return(invisible())
  }

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
  nms <- arg_labels(nms)

  blockr_abort(
    "For {what}, `serve()` takes no argument but `x`, and it was given ",
    "{nms}.",
    if (length(moved)) paste0(" ", paste(moved, collapse = " ")) else "",
    class = "serve_args_invalid"
  )
}

# The names of the arguments that the call `list(...)` holds, unevaluated,
# with "" for an argument that has none.
dots_names <- function(call) {
  args <- as.list(call)[-1L]
  coal(names(args), character(length(args)), fail_all = FALSE)
}

# The arguments of the names given, as a message names them: each name in
# backticks, or "an unnamed argument" for an argument that has none.
arg_labels <- function(nms) {
  ifelse(nzchar(nms), paste0("`", nms, "`"), "an unnamed argument")
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
