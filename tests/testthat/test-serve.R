# The apps that serve() returns, for a board, the path of a JSON board and a
# block that serve() serves alone: what R reads as serve() is called, the page,
# and the server function, which a test session runs as the Shiny transport
# carries the client's messages to it.

served_board <- function() {
  new_board(
    c(a = new_dataset_block("BOD"), b = new_head_block(n = 3L)),
    list(from = "a", to = "b"),
    id = "served_board",
    name = "Served"
  )
}

test_that("serve() serves a board, whose JSON R writes as serve() is called", {

  board <- served_board()
  app <- serve(board)

  expect_s3_class(app, "shiny.appobj")

  reply <- served_replies(app, hello_message())[[1L]]

  expect_identical(reply$status, "ok")
  expect_identical(
    reply$board,
    decode_message(encode_message(blockr_ser(board)))
  )
  expect_null(reply$bound)
  expect_identical(reply$lock, list(board = FALSE, load = FALSE))

  # So a board that R cannot write as JSON fails as the script runs.
  expect_error(
    serve(new_board(options = new_n_rows_option(as.Date("2024-01-01")))),
    class = "board_value_invalid"
  )
})

test_that("serve() registers the board's classes that the registry lacks", {

  cls <- test_block_class(
    "note_block",
    expr = function(text = "") bbquote(.(text)),
    ui = js_module("note-block.js", package = NULL)
  )

  board <- new_board(
    c(
      a = block_constructor(cls)("Hi"),
      b = new_static_block("datasets::BOD"),
      c = new_head_block()
    )
  )

  ids <- c("note_block", "blockr.core-static_block")
  head <- get_registry_entry("blockr.core-head_block")

  registry <- served_registry(serve(board))

  # Each under the id that its class gives, so that a class from a package
  # keeps its package's id, in the app's block registry alone
  expect_contains(ls(registry), ids)
  expect_false(any(ids %in% list_blocks()))
  expect_identical(get_registry_entry("note_block", registry)$name, "Note")
  expect_identical(
    get_registry_entry("blockr.core-static_block", registry)$package,
    "blockr.core"
  )

  # A class that the block registry holds keeps its entry
  expect_identical(get_registry_entry("blockr.core-head_block", registry), head)
})

test_that("serve() builds the app's block registry of the classes it offers", {

  local_declared(block_registry)
  unregister_blocks()

  # Every available class by default, beside the classes of the board
  expect_contains(
    ls(served_registry(serve(served_board()))),
    c("blockr.core-merge_block", "blockr.core-csv_block")
  )

  # The methods for a board and for a path take the classes that the app
  # offers, and refuse one that no available package lists as they are called
  path <- withr::local_tempfile(fileext = ".json")
  writeLines(unclass(encode_message(proto_served_file())), path)

  rows <- core_block_rows()
  absent <- data.frame(package = "blockr.absent", id = "absent_block")

  for (x in list(served_board(), path)) {

    registry <- served_registry(
      serve(x, blocks = rows[rows[["id"]] == "csv_block", ])
    )

    expect_contains(ls(registry), "blockr.core-csv_block")
    expect_false("blockr.core-merge_block" %in% ls(registry))

    expect_error(
      serve(x, blocks = absent),
      "Package blockr.absent, whose block classes the app offers, is not ",
      class = "block_classes_unavailable"
    )
  }

  # The block registry of the process holds none of the apps' classes
  expect_length(list_blocks(), 0L)
})

test_that("an app's sessions read the block registry that serve() built", {

  local_declared(block_registry)
  unregister_blocks()

  rows <- core_block_rows()
  app <- serve(served_board(), blocks = rows)

  # A later register_block() or serve() leaves the app as it was, while the
  # later app holds the class that the script registered
  dir <- withr::local_tempdir()
  module <- file.path(dir, "note-block.js")
  writeLines("export default function mount() {}", module)

  note <- outside_class("note_block", function(text = "") bbquote(.(text)), dir)
  register_block(block_constructor(note), "Note", "A note")

  later <- serve(
    served_board(),
    blocks = rows[rows[["id"]] %in% c("dataset_block", "head_block"), ]
  )

  expect_setequal(
    names(served_replies(app, hello_message())[[1L]]$block_registry),
    paste0("blockr.core-", rows[["id"]])
  )
  expect_setequal(
    names(served_replies(later, hello_message())[[1L]]$block_registry),
    c("blockr.core-dataset_block", "blockr.core-head_block", "note_block")
  )

  # The app evaluates its blocks, answers their requests and makes their
  # outputs by its registry, whose classes the block registry of the process
  # lacks
  replies <- served_replies(
    app,
    hello_message(),
    list(
      evaluate_request("2", "a", 1L, sample_recipe("a")),
      list(
        id = "3", kind = "request", type = "block_class",
        registry_id = "blockr.core-head_block", block = "b", name = "n_max",
        args = obj(), reads = list(read_of("a", 1L, "data"))
      ),
      output_request("4", "a", 1L, params = list(page_size = 5L))
    )
  )

  expect_identical(chr_xtr(replies[-1L], "status"), c("ok", "ok", "ok"))
  expect_identical(replies[[3L]]$value, 150L)

  # So does the board lock, which R reads from the opening board as serve() is
  # called where the load lock is on too, and otherwise from each block's first
  # evaluation
  withr::local_options(blockr.locked = TRUE, blockr.lock_load = TRUE)

  expect_no_error(serve(served_board(), blocks = rows))

  withr::local_options(blockr.lock_load = FALSE)

  replies <- served_replies(
    serve(served_board(), blocks = rows),
    hello_message(),
    list(evaluate_request("2", "a", 1L, sample_recipe("a")))
  )

  expect_identical(replies[[2L]]$status, "ok")
})

test_that("blocks and plugins read their rows alike, and plugins no file", {

  # A YAML file of block classes gives rows in its order, with the id `NA` for
  # all of a package's classes
  file <- withr::local_tempfile(fileext = ".yml")
  writeLines(
    c("- blockr.dag", "- blockr.core:", "  - head_block", "  - glue_block"),
    file
  )

  rows <- yaml_block_rows(file)

  expect_identical(rows[["package"]], c("blockr.dag", rep("blockr.core", 2L)))
  expect_identical(rows[["id"]], c(NA, "head_block", "glue_block"))

  # Each argument's errors keep its own class and words, and only `blocks`
  # takes the path of a YAML file
  expect_error(
    serve(served_board(), blocks = 1L),
    paste(
      "The block classes that an app offers are expected as a data frame",
      "with the character columns `package` and `id`, as `available_blocks()`",
      "gives, or as the path of a YAML file."
    ),
    fixed = TRUE,
    class = "block_classes_invalid"
  )
  expect_error(
    serve(served_board(), plugins = file),
    paste(
      "The plugins that an app runs are expected as a data frame with the",
      "character columns `package` and `id`, as `available_plugins()` gives."
    ),
    fixed = TRUE,
    class = "app_plugins_invalid"
  )
})

test_that("serve() registers the class of a block that it serves alone", {

  local_declared(block_registry)
  register_block(new_class_head_block, "Head", "First rows")

  registry <- served_registry(serve(new_static_block("datasets::BOD")))

  # The block's class alone, whatever the script registered, and in the block
  # registry of the process none
  expect_identical(ls(registry), "blockr.core-static_block")
  expect_false("blockr.core-static_block" %in% list_blocks())

  # R reads no package's file for it, so a file that R cannot read stops none
  # of these apps
  file <- file.path(local_load_all_package(), "inst", "registry", "blocks.yml")
  writeLines("new_probe_block: [", file)

  expect_error(available_blocks(), class = "package_blocks_yaml_invalid")
  expect_no_error(serve(new_static_block("datasets::BOD")))

  # A block takes no `blocks`, which it reads as data for an input of that name
  expect_error(
    serve(
      new_static_block("datasets::BOD"),
      blocks = data.frame(package = "blockr.core", id = "head_block")
    ),
    class = "serve_data_invalid"
  )
})

test_that("serve() replaces a class from outside a package that differs", {

  local_declared(block_registry)

  old <- note_class()
  new <- note_class(n = TRUE)

  # As when a script redefines a class after it registered it
  register_block(block_constructor(old), "Note", "A note")

  registry <- served_registry(serve(new_board(c(a = block_constructor(new)()))))

  expect_identical(get_registry_entry("note_block", registry)$block_class, new)
  expect_identical(get_registry_entry("note_block")$block_class, old)

  # A class from a package keeps the entry under its id, which the script
  # registered
  held <- note_class(package = "blockr.core")
  register_block(block_constructor(held), "Note", "A note")

  other <- block_constructor(note_class(TRUE, "blockr.core"))
  registry <- served_registry(serve(new_board(c(a = other()))))

  expect_identical(
    get_registry_entry("blockr.core-note_block", registry)$block_class,
    held
  )

  # And one that its package lists among the classes that the app offers
  local_load_all_package()

  rows <- available_blocks()
  other <- test_block_class(
    "probe_block",
    expr = function(n = 1L) bbquote(seq_len(.(n))),
    ui = js_module("probe-block.js", package = NULL),
    package = "regprobe"
  )

  registry <- served_registry(
    serve(
      new_board(c(a = block_constructor(other)())),
      blocks = rows[rows[["package"]] == "regprobe", ]
    )
  )

  expect_identical(
    get_registry_entry("regprobe-probe_block", registry)$block_class,
    get("probe_block", envir = asNamespace("regprobe"))
  )
})

test_that("serve() refuses two classes of one id from outside a package", {

  one <- block_constructor(note_class())
  two <- block_constructor(note_class(n = TRUE))

  board <- new_board(c(a = one(), b = one(), c = two()))

  expect_error(serve(board), "Blocks a and c", class = "board_blocks_invalid")
})

test_that("serve() takes nothing but the board, and says where the rest went", {

  board <- served_board()

  expect_error(
    serve(board, id = "my_board"),
    paste0(
      "For a board, `serve()` takes no argument but `x`, `blocks` and ",
      "`plugins`, and it was given `id`. A board holds its ID, which ",
      "`new_board()` takes as `id`."
    ),
    fixed = TRUE,
    class = "serve_args_invalid"
  )

  # R evaluates none of them, so that one naming a function that is gone fails
  # with the same error.
  expect_error(
    serve(board, loader = local_loader(), new_board()),
    paste0(
      "it was given `loader` and an unnamed argument. The client opens and ",
      "saves boards itself, with no loader."
    ),
    fixed = TRUE,
    class = "serve_args_invalid"
  )

  expect_error(
    serve(board, options = list(), navbar = "x"),
    "`new_board()` takes as `options`.",
    fixed = TRUE,
    class = "serve_args_invalid"
  )
})

test_that("serve() sends the app's plugins in its reply to hello", {

  core <- list(
    `blockr.core-basic_frontend_plugin` = list(
      name = "Basic frontend",
      version = 1L
    ),
    `blockr.core-file_storage_plugin` = list(
      name = "File storage",
      version = 1L
    )
  )

  # By default, the app runs every plugin class that an available package
  # lists
  reply <- served_replies(serve(served_board()), hello_message())[[1L]]

  expect_identical(reply$plugins, core)
  expect_false("plugin_versions" %in% names(reply))

  # Rows that narrow them leave out what they do not name, as an app without
  # storage
  rows <- subset(available_plugins(), id != "file_storage_plugin")
  reply <- served_replies(serve(served_board(), plugins = rows),
                          hello_message())[[1L]]

  expect_identical(reply$plugins, core[1L])

  # So does the path of a JSON board
  path <- withr::local_tempfile(fileext = ".json")
  writeLines(unclass(encode_message(proto_served_file())), path)

  reply <- served_replies(serve(path), hello_message())[[1L]]
  expect_identical(reply$plugins, core)

  reply <- served_replies(serve(path, plugins = rows), hello_message())[[1L]]
  expect_identical(reply$plugins, core[1L])

  # A block that serve() serves alone runs no plugin
  reply <- served_replies(
    serve(new_head_block(), data = list(data = datasets::BOD)),
    hello_message()
  )[[1L]]

  expect_identical(reply$plugins, set_names(list(), character()))

  # Rows that R refuses stop the app as serve() is called
  expect_error(
    serve(served_board(), plugins = rows[0L, ]),
    class = "app_plugins_invalid"
  )
  expect_error(serve(path, plugins = "none.yml"), class = "app_plugins_invalid")
})

test_that("serve() refuses a board with a plugin that the app does not run", {

  local_plugin_classes(
    Notes = plugin_ctor(function(text = "") NULL, "notes_plugin")
  )
  local_versions(blockr.test = "0.1.0")

  board <- new_board(
    plugins = plugin_ctor(function(text = "") NULL, "notes_plugin")("To do")
  )

  # The app runs the plugin by default, as the available packages list it
  reply <- served_replies(serve(board), hello_message())[[1L]]

  expect_named(
    reply$plugins,
    c(
      "blockr.core-basic_frontend_plugin",
      "blockr.core-file_storage_plugin",
      "blockr.test-notes_plugin"
    )
  )
  expect_identical(
    reply$board$board$plugins,
    list(`blockr.test-notes_plugin` = list(text = "To do"))
  )

  expect_error(
    serve(
      board,
      plugins = subset(available_plugins(), package != "blockr.test")
    ),
    "blockr.test-notes_plugin, which the app does not run",
    class = "board_plugins_invalid"
  )
})

test_that("serve() serves the path of a JSON board, whose text R sends", {

  path <- withr::local_tempfile(fileext = ".json")
  text <- unclass(encode_message(proto_served_file()))
  writeLines(text, path)

  reply <- served_replies(serve(path), hello_message())[[1L]]

  expect_identical(reply$board, decode_message(text))
  expect_null(reply$bound)

  expect_error(
    serve(path, id = "cars"),
    "For the path of a JSON board, `serve()` takes no argument but `x`",
    fixed = TRUE,
    class = "serve_args_invalid"
  )

  # R reads the file as serve() is called, so a missing file fails at once.
  expect_error(serve(tempfile()), class = "board_json_missing")
})

test_that("serve() reads the opening board for the locks as it is called", {

  withr::local_options(blockr.locked = TRUE, blockr.lock_load = TRUE)

  board <- proto_json_board(
    obj(
      id = "older",
      name = "Older",
      blocks = obj(a = proto_block("blockr.foo-bar_block", obj(), "Bar")),
      links = obj(),
      stacks = obj(),
      options = obj(),
      plugins = obj()
    )
  )

  path <- withr::local_tempfile(fileext = ".json")
  writeLines(unclass(encode_message(board)), path)

  expect_error(serve(path), class = "locked_board_unreadable")

  # Every session holds its evaluations to the lock that R read, and R logs
  # the error that it replies.
  app <- serve(served_board())

  withr::local_options(
    blockr.logger = cnd_logger,
    blockr.log_level = "error",
    width = 500L
  )
  local_reset_logging()

  expect_warning(
    replies <- served_replies(
      app,
      hello_message(),
      list(
        evaluate_request(
          "2",
          "a",
          1L,
          list(
            registry_id = "blockr.core-dataset_block",
            state = list(dataset = "iris", package = "datasets"),
            inputs = list()
          )
        )
      )
    ),
    "Block a at revision 1: The board lock fixes the state of block \"a\""
  )

  expect_identical(replies[[1L]]$lock, list(board = TRUE, load = TRUE))
  expect_identical(replies[[2L]]$status, "error")
  expect_identical(condition_class(replies[[2L]]), "lock_refused")
})

test_that("serve() stops where the logger that the app names cannot work", {

  withr::local_envvar(BLOCKR_LOGGER = "")
  withr::local_options(blockr.logger = "otel_logger")
  local_reset_logging()

  expect_error(serve(served_board()), class = "otel_logger_unavailable")
})

test_that("serve() serves a block alone, binding its data by input", {

  app <- serve(new_head_block(n = 3L), data = list(data = datasets::BOD))

  replies <- served_replies(app, hello_message())
  reply <- replies[[1L]]
  blocks <- reply$board$board$blocks

  expect_length(blocks, 1L)
  expect_identical(blocks[[1L]]$registry_id, "blockr.core-head_block")
  expect_identical(blocks[[1L]]$name, "Head")
  expect_identical(reply$bound, "data")

  # R evaluates the block with the data it holds, which the evaluation names
  # by the input alone.
  replies <- served_replies(
    app,
    hello_message(),
    list(
      evaluate_request(
        "2",
        names(blocks),
        1L,
        list(
          registry_id = "blockr.core-head_block",
          state = list(n = 3L, direction = "head"),
          inputs = list(list(input = "data"))
        )
      )
    )
  )

  expect_identical(replies[[2L]]$summary, "<data.frame> [3 × 2]")

  reply <- served_replies(
    serve(
      new_merge_block("Time"),
      data = list(x = datasets::BOD, y = datasets::BOD)
    ),
    hello_message()
  )[[1L]]
  expect_identical(reply$bound, c("x", "y"))

  # The data comes in `...` where `data` is empty, and a variadic block takes
  # it under any names.
  reply <- served_replies(
    serve(new_rbind_block(), a = datasets::BOD, b = datasets::BOD),
    hello_message()
  )[[1L]]
  expect_identical(reply$bound, c("a", "b"))
})

test_that("serve() refuses data that R cannot bind to the block's inputs", {

  expect_error(
    serve(new_rbind_block(), datasets::BOD, datasets::BOD),
    paste0(
      "and 2 entries have no name. A variadic block takes its data under any ",
      "names, as in `serve(new_rbind_block(), a = iris, b = mtcars)`."
    ),
    fixed = TRUE,
    class = "serve_data_invalid"
  )

  expect_error(
    serve(new_head_block(), datasets::BOD),
    "and 1 entry has no name.",
    fixed = TRUE,
    class = "serve_data_invalid"
  )

  expect_error(
    serve(new_head_block(), data = datasets::BOD),
    "is a list by input",
    class = "serve_data_invalid"
  )

  expect_error(
    serve(new_rbind_block(), a = datasets::BOD, a = datasets::BOD),
    "the data names a more than once",
    class = "serve_data_invalid"
  )

  expect_error(
    serve(
      new_merge_block(),
      data = list(x = datasets::BOD, z = datasets::BOD)
    ),
    "The block has no input z. Its inputs are x and y.",
    fixed = TRUE,
    class = "serve_data_invalid"
  )

  expect_error(
    serve(new_dataset_block(), data = list(data = datasets::BOD)),
    "The block has no inputs, so it takes no data",
    class = "serve_data_invalid"
  )
})

test_that("serve() ignores `...` where `data` is given, with a warning", {

  expect_warning(
    app <- serve(
      new_head_block(),
      n = 1,
      datasets::BOD,
      data = list(data = datasets::BOD)
    ),
    paste0(
      "R takes the block's data from `data`, and ignores `n` and an unnamed ",
      "argument in `...`."
    ),
    fixed = TRUE,
    class = "serve_dots_ignored"
  )

  reply <- served_replies(app, hello_message())[[1L]]

  expect_identical(reply$bound, "data")
})

test_that("the page names the import map first and starts the client", {

  page <- htmltools::renderTags(serve_page())

  expect_identical(page$dependencies[[1L]]$name, "blockr-import-map")
  expect_match(
    page$dependencies[[1L]]$head,
    "<script type=\"importmap\">",
    fixed = TRUE
  )
  expect_match(page$head, "blockr.core/js/blockr-core.css?v=", fixed = TRUE)
  expect_match(
    page$html,
    "import { startPage } from \"./blockr.core/js/blockr-core.js?v=",
    fixed = TRUE
  )
  expect_match(
    page$html,
    "startPage({ el: document.getElementById(\"blockr-page\") });",
    fixed = TRUE
  )

  page <- htmltools::renderTags(serve_page(alone = TRUE))

  expect_match(page$html, "\"blockr-page\"), alone: true });", fixed = TRUE)
})

test_that("in Shiny's test mode, the page puts out blockrTest", {

  # Outside test mode, the page loads nothing for end-to-end tests.
  page <- htmltools::renderTags(serve_page())$html

  expect_no_match(page, "blockrTest", fixed = TRUE)

  old <- shiny::getShinyOption("testmode")
  shiny::shinyOptions(testmode = TRUE)
  withr::defer(shiny::shinyOptions(testmode = old))

  page <- htmltools::renderTags(serve_page())$html

  expect_match(
    page,
    "import { blockrTest } from \"./blockr.core/js/blockr-test.js?v=",
    fixed = TRUE
  )
  expect_match(
    page,
    paste0(
      "const started = startPage({ el: ",
      "document.getElementById(\"blockr-page\") });\n",
      "window.blockrTest = blockrTest(() => started.core());\n"
    ),
    fixed = TRUE
  )
  expect_match(
    htmltools::renderTags(serve_page(alone = TRUE))$html,
    "\"blockr-page\"), alone: true });\nwindow.blockrTest = ",
    fixed = TRUE
  )
})

test_that("the app exports the latest result of each block for e2e tests", {

  # The recipe of the sample board's dataset block a with the dataset given.
  dataset_recipe <- function(dataset) {
    utils::modifyList(sample_recipe("a"), list(state = list(dataset = dataset)))
  }

  app <- serve(served_board())

  exports <- served_exports(
    app,
    hello_message(),
    list(
      evaluate_request("2", "a", 1L, sample_recipe("a")),
      evaluate_request("3", "a", 2L, dataset_recipe("mtcars")),
      evaluate_request(
        "4",
        "b",
        1L,
        sample_recipe("b", list(read_of("a", 2L, "data")))
      )
    )
  )

  # The result of the latest evaluation of a, its second.
  expect_identical(
    exports$result,
    list(a = datasets::mtcars, b = utils::head(datasets::mtcars))
  )

  # R holds no result of a block whose latest evaluation failed, so the
  # export leaves the block out.
  withr::local_options(
    blockr.logger = cnd_logger,
    blockr.log_level = "error",
    width = 500L
  )
  local_reset_logging()

  expect_warning(
    exports <- served_exports(
      app,
      hello_message(),
      list(
        evaluate_request("2", "a", 1L, sample_recipe("a")),
        evaluate_request("3", "a", 2L, dataset_recipe("no_such_dataset"))
      )
    ),
    "Block a at revision 2: 'no_such_dataset' is not an exported object"
  )

  expect_named(exports$result, character())
})

test_that("the app of a block served alone exports the block's result", {

  app <- serve(new_head_block(), data = list(data = datasets::mtcars))
  block <- names(served_replies(app, hello_message())[[1L]]$board$board$blocks)

  # Before R holds a result of the block, there is none to export.
  expect_null(served_exports(app, hello_message())$result)

  exports <- served_exports(
    app,
    hello_message(),
    list(
      evaluate_request(
        "2",
        block,
        1L,
        sample_recipe("b", list(list(input = "data")))
      )
    )
  )

  expect_identical(exports$result, utils::head(datasets::mtcars))
})
