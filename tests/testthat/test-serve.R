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

test_that("serve() takes nothing but the board, and says where the rest went", {

  board <- served_board()

  expect_error(
    serve(board, id = "my_board"),
    paste0(
      "For a board, `serve()` takes no argument but `x`, and it was given ",
      "`id`. A board holds its ID, which `new_board()` takes as `id`."
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
    serve(board, plugins = list(), options = list(), navbar = "x"),
    "`new_board()` takes as `plugins`.",
    fixed = TRUE,
    class = "serve_args_invalid"
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

  # Every session holds its evaluations to the lock that R read.
  app <- serve(served_board())

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
  )

  expect_identical(replies[[1L]]$lock, list(board = TRUE, load = TRUE))
  expect_identical(replies[[2L]]$status, "error")
  expect_identical(condition_class(replies[[2L]]), "lock_refused")
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

  expect_error(
    serve(new_head_block(), n = 1, data = list(data = datasets::BOD)),
    "and it was given both",
    class = "serve_data_invalid"
  )
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
