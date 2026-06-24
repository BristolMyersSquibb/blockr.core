test_that("the default server produces the deserialized board", {

  test_board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(from = "a", to = "b")
  )

  temp <- withr::local_tempfile(fileext = ".json")

  testServer(
    preserve_board_server,
    file.copy(output$serialize, temp),
    args = list(board = reactiveValues(board = test_board))
  )

  testServer(
    preserve_board_server,
    {
      session$setInputs(restore = list(datapath = temp))

      restored <- res()

      expect_s3_class(restored, class(test_board))
      expect_length(board_blocks(restored), length(board_blocks(test_board)))
      expect_setequal(board_block_ids(restored), board_block_ids(test_board))
      expect_setequal(board_link_ids(restored), board_link_ids(test_board))
    },
    args = list(board = reactiveValues(board = new_board()))
  )
})

test_that("board_server exposes the restored board through board_refresh", {

  test_board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("BOD"),
      c = new_merge_block(by = "Time")
    ),
    links = links(
      from = c("a", "b"),
      to = c("c", "c"),
      input = c("x", "y")
    )
  )

  temp <- withr::local_tempfile(fileext = ".json")

  testServer(
    get_s3_method("board_server", test_board),
    {
      ser_deser <- session$makeScope("preserve_board")
      file.copy(ser_deser$output$serialize, temp)
    },
    args = list(x = test_board, plugins = preserve_board())
  )

  testServer(
    get_s3_method("board_server", test_board),
    {
      ser_deser <- session$makeScope("preserve_board")
      ser_deser$setInputs(restore = list(datapath = temp))

      restored <- board_refresh()

      expect_length(board_blocks(restored), length(board_blocks(test_board)))
      expect_setequal(board_block_ids(restored), board_block_ids(test_board))
      expect_setequal(board_link_ids(restored), board_link_ids(test_board))
    },
    args = list(x = new_board(), plugins = preserve_board())
  )
})

test_that("serve_board_srv drives the reload when board_refresh fires", {

  refresh <- reactiveVal()
  reloaded <- NULL

  local_mocked_bindings(
    board_server = function(id, x, ...) list(board_refresh = refresh),
    reload_board = function(loader, board, session) reloaded <<- board,
    blockr_test_exports = function(x, rv, ...) invisible()
  )

  board <- new_board(blocks = c(a = new_dataset_block("iris")))

  srv <- serve_board_srv(
    "app", board, local_loader(), blockr_app_plugins, blockr_app_options
  )

  testServer(
    srv,
    {
      refresh(board)
      session$flushReact()

      expect_identical(reloaded, board)
    }
  )
})

test_that("preserve_board is the save/restore plugin (no loader on it)", {

  plug <- preserve_board()

  expect_true(is_plugin(plug))
  expect_s3_class(plug, "preserve_board")
  expect_setequal(names(plug), c("server", "ui"))
  expect_null(attr(plug, "loader"))
})

test_that("board_loader bundles a resolve and an optional validated stage", {

  ld <- board_loader(
    resolve = function(query, session) NULL,
    stage = function(board, session) NULL
  )

  expect_true(is_board_loader(ld))
  expect_true(is.function(ld$resolve))
  expect_true(is.function(ld$stage))

  stageless <- board_loader(function(query, session) NULL)
  expect_true(is_board_loader(stageless))
  expect_null(stageless$stage)

  expect_error(
    board_loader(function(x) NULL),
    class = "board_loader_resolve_invalid"
  )
  expect_error(
    board_loader(function(query, session) NULL, function(b, s) NULL),
    class = "board_loader_stage_invalid"
  )

  # formals must match exactly -- extra args (or `...`) are rejected
  expect_error(
    board_loader(function(query, session, x) NULL),
    class = "board_loader_resolve_invalid"
  )
  expect_error(
    board_loader(function(...) NULL),
    class = "board_loader_resolve_invalid"
  )
  expect_error(
    board_loader(
      function(query, session) NULL,
      function(board, session, x) NULL
    ),
    class = "board_loader_stage_invalid"
  )
})

test_that("the default loader stages by token and resolves it back", {

  ld <- local_loader()
  board <- new_board(blocks = c(a = new_dataset_block("iris")))

  params <- ld$stage(board, shiny::MockShinySession$new())
  expect_named(params, reload_param)

  query <- set_names(list(params[[reload_param]]), reload_param)

  expect_identical(ld$resolve(query, NULL), board)
  expect_null(ld$resolve(list(), NULL))

  # a separate loader instance has its own store
  expect_null(local_loader()$resolve(query, NULL))
})

test_that("the default loader consumes the token at the WS read", {

  ld <- local_loader()
  board <- new_board(blocks = c(a = new_dataset_block("iris")))

  session <- shiny::MockShinySession$new()
  token <- ld$stage(board, session)[[reload_param]]
  query <- set_names(list(token), reload_param)

  expect_identical(ld$resolve(query, session), board)
  expect_null(ld$resolve(query, NULL))
})

test_that("resolve_board prefers the loader board, validates, falls back", {

  default <- new_board()
  staged <- new_board(blocks = c(a = new_dataset_block("iris")))

  ld <- local_loader()
  token <- ld$stage(staged, shiny::MockShinySession$new())[[reload_param]]
  query <- set_names(list(token), reload_param)

  expect_identical(resolve_board(default, ld, query, NULL), staged)
  expect_identical(resolve_board(default, ld, list(), NULL), default)

  bad <- board_loader(resolve = function(query, session) "nope")
  expect_error(
    resolve_board(default, bad, list(), NULL),
    class = "invalid_board_loader"
  )
})

test_that("reload_board stages the board through the loader", {

  staged <- NULL
  spy <- board_loader(
    resolve = function(query, session) NULL,
    stage = function(board, session) {
      staged <<- board
      set_names(list("tok"), reload_param)
    }
  )

  board <- new_board(blocks = c(a = new_dataset_block("iris")))
  reload_board(spy, board, shiny::MockShinySession$new())

  expect_identical(staged, board)

  # a loader without a stage simply reloads, with nothing to persist
  no_stage <- board_loader(function(query, session) NULL)
  expect_no_error(reload_board(no_stage, board, shiny::MockShinySession$new()))
})

test_that("preserve_board return validation", {

  with_mock_session(
    {
      check_ser_deser_val(list(a = 1))
      sink_msg(
        expect_warning(
          session$flushReact(),
          "Expecting `preserve_board` to return a reactive value"
        )
      )
    }
  )

  with_mock_session(
    {
      check_ser_deser_val(reactiveVal(1))
      sink_msg(
        expect_warning(
          session$flushReact(),
          "Expecting the `preserve_board` return value to evaluate to a"
        )
      )
    }
  )
})

test_that("restore_board returns the deserialized board", {

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  ser <- blockr_ser(test_board)
  result <- reactiveVal()

  with_mock_session(
    {
      restore_board(test_board, ser, result, session = session)

      val <- result()
      expect_true(is_board(val))
      expect_length(board_blocks(val), length(board_blocks(test_board)))
    }
  )
})

test_that("dummy ser/deser ui test", {
  expect_s3_class(preserve_board_ui("ser_deser", new_board()), "shiny.tag.list")
})
