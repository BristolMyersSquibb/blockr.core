test_that("serialize/restore module", {

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
    {
      file.copy(output$serialize, temp)
    },
    args = list(board = reactiveValues(board = test_board))
  )

  withr::defer(clear_reload_handoff())

  testServer(
    preserve_board_server,
    {
      session$setInputs(restore = list(datapath = temp))

      restored <- preserve_board_loader(NULL)

      expect_s3_class(restored, class(test_board))

      expect_length(board_blocks(restored), length(board_blocks(test_board)))
      expect_length(board_links(restored), length(board_links(test_board)))

      expect_setequal(board_block_ids(restored), board_block_ids(test_board))
      expect_setequal(board_link_ids(restored), board_link_ids(test_board))
    },
    args = list(board = reactiveValues(board = new_board()))
  )
})

test_that("restore through board server stages the reload handoff", {

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
    args = list(
      x = test_board,
      plugins = preserve_board()
    )
  )

  withr::defer(clear_reload_handoff())

  testServer(
    get_s3_method("board_server", test_board),
    {
      ser_deser <- session$makeScope("preserve_board")
      ser_deser$setInputs(restore = list(datapath = temp))

      brd <- preserve_board_loader(NULL)

      expect_length(board_blocks(brd), length(board_blocks(test_board)))
      expect_length(board_links(brd), length(board_links(test_board)))

      expect_setequal(board_block_ids(brd), board_block_ids(test_board))
      expect_setequal(board_link_ids(brd), board_link_ids(test_board))
    },
    args = list(
      x = new_board(),
      plugins = preserve_board()
    )
  )
})

test_that("preserve_board ships a default loader", {

  plug <- preserve_board()

  expect_true(is_plugin(plug))
  expect_s3_class(plug, "preserve_board")
  expect_identical(plugin_loader(plug), preserve_board_loader)
})

test_that("preserve_board_loader reads the reload handoff", {

  withr::defer(clear_reload_handoff())

  clear_reload_handoff()
  expect_null(preserve_board_loader(NULL))

  board <- new_board(blocks = c(a = new_dataset_block("iris")))
  stage_reload_handoff(board)
  expect_identical(preserve_board_loader(NULL), board)

  clear_reload_handoff()
  expect_null(preserve_board_loader(NULL))
})

test_that("resolve_board prefers the loader board over the default", {

  withr::defer(clear_reload_handoff())

  default <- new_board()
  staged <- new_board(blocks = c(a = new_dataset_block("iris")))

  clear_reload_handoff()
  expect_identical(
    resolve_board(default, blockr_app_plugins, NULL),
    default
  )

  stage_reload_handoff(staged)
  expect_identical(
    resolve_board(default, blockr_app_plugins, NULL),
    staged
  )
})

test_that("resolve_board falls back when the plugin has no loader", {

  withr::defer(clear_reload_handoff())

  default <- new_board()
  no_loader <- function(x) plugins(preserve_board(loader = NULL))

  stage_reload_handoff(new_board(blocks = c(a = new_dataset_block("iris"))))

  expect_identical(
    resolve_board(default, no_loader, NULL),
    default
  )
})

test_that("resolve_board rejects a non-board loader result", {

  default <- new_board()
  bad <- function(x) plugins(preserve_board(loader = function(request) "nope"))

  expect_error(
    resolve_board(default, bad, NULL),
    class = "invalid_board_loader"
  )
})

test_that("board requests expose the parsed query at both entry points", {

  get_req <- new.env()
  get_req$QUERY_STRING <- "board_name=foo&user=bar"

  get <- board_request_get(get_req)

  expect_s3_class(get, "board_request")
  expect_identical(get$query, list(board_name = "foo", user = "bar"))
  expect_identical(get$request, get_req)
  expect_null(get$session)

  ws_sess <- list(
    clientData = list(url_search = "?board_name=foo&user=bar"),
    request = get_req
  )

  ws <- board_request_ws(ws_sess)

  expect_s3_class(ws, "board_request")
  expect_identical(ws$query, list(board_name = "foo", user = "bar"))
  expect_identical(ws$session, ws_sess)
  expect_identical(ws$request, get_req)
})

test_that("restore_board with meta wraps result", {

  test_board <- new_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  ser <- blockr_ser(test_board)
  result <- reactiveVal()
  meta <- list(url = "/foo", flag = TRUE)

  with_mock_session(
    {
      restore_board(test_board, ser, result, meta = meta, session = session)

      val <- result()
      expect_true(is.list(val))
      expect_true(is_board(val$board))
      expect_identical(val$meta, meta)
      expect_length(board_blocks(val$board), length(board_blocks(test_board)))
    }
  )
})

test_that("restore_board without meta returns naked board", {

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
