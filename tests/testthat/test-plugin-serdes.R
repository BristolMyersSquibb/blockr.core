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

test_that("the board server reloads a restore through the supplied loader", {

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

  staged <- NULL
  spy <- board_loader(
    resolve = function(request) NULL,
    stage = function(board, session) {
      staged <<- board
      set_names(list("tok"), reload_param)
    }
  )

  testServer(
    get_s3_method("board_server", test_board),
    {
      ser_deser <- session$makeScope("preserve_board")
      ser_deser$setInputs(restore = list(datapath = temp))
    },
    args = list(x = new_board(), plugins = preserve_board(), loader = spy)
  )

  expect_length(board_blocks(staged), length(board_blocks(test_board)))
  expect_setequal(board_block_ids(staged), board_block_ids(test_board))
  expect_setequal(board_link_ids(staged), board_link_ids(test_board))
})

test_that("preserve_board is the save/restore plugin (no loader on it)", {

  plug <- preserve_board()

  expect_true(is_plugin(plug))
  expect_s3_class(plug, "preserve_board")
  expect_setequal(names(plug), c("server", "ui"))
  expect_null(attr(plug, "loader"))
})

test_that("board_loader bundles a resolve and a stage with validated formals", {

  ld <- board_loader(
    resolve = function(request) NULL,
    stage = function(board, session) NULL
  )

  expect_true(is_board_loader(ld))
  expect_true(is.function(ld$resolve))
  expect_true(is.function(ld$stage))

  expect_error(
    board_loader(function(x) NULL, function(board, session) NULL),
    class = "board_loader_resolve_invalid"
  )
  expect_error(
    board_loader(function(request) NULL, function(b, s) NULL),
    class = "board_loader_stage_invalid"
  )
  expect_true(
    is_board_loader(board_loader(function(...) NULL, function(...) NULL))
  )
})

test_that("the default loader stages by token and resolves it back", {

  ld <- preserve_board_loader()
  board <- new_board(blocks = c(a = new_dataset_block("iris")))

  params <- ld$stage(board, shiny::MockShinySession$new())
  expect_named(params, reload_param)

  token <- params[[reload_param]]
  with_token <- list(
    query = set_names(list(token), reload_param), session = NULL
  )

  expect_identical(ld$resolve(with_token), board)
  expect_null(ld$resolve(list(query = list(), session = NULL)))

  # a separate loader instance has its own store
  expect_null(preserve_board_loader()$resolve(with_token))
})

test_that("the default loader consumes the token at the WS read", {

  ld <- preserve_board_loader()
  board <- new_board(blocks = c(a = new_dataset_block("iris")))

  session <- shiny::MockShinySession$new()
  token <- ld$stage(board, session)[[reload_param]]

  ws <- list(query = set_names(list(token), reload_param), session = session)
  get <- list(query = set_names(list(token), reload_param), session = NULL)

  expect_identical(ld$resolve(ws), board)
  expect_null(ld$resolve(get))
})

test_that("resolve_board prefers the loader board, validates, falls back", {

  default <- new_board()
  staged <- new_board(blocks = c(a = new_dataset_block("iris")))

  ld <- preserve_board_loader()
  token <- ld$stage(staged, shiny::MockShinySession$new())[[reload_param]]
  with_token <- list(
    query = set_names(list(token), reload_param), session = NULL
  )

  expect_identical(resolve_board(default, ld, with_token), staged)
  expect_identical(
    resolve_board(default, ld, list(query = list(), session = NULL)),
    default
  )
  expect_identical(resolve_board(default, NULL, list(query = list())), default)

  bad <- board_loader(
    resolve = function(request) "nope",
    stage = function(board, session) NULL
  )
  expect_error(
    resolve_board(default, bad, list(query = list())),
    class = "invalid_board_loader"
  )
})

test_that("reload_board stages the board through the loader", {

  staged <- NULL
  spy <- board_loader(
    resolve = function(request) NULL,
    stage = function(board, session) {
      staged <<- board
      set_names(list("tok"), reload_param)
    }
  )

  board <- new_board(blocks = c(a = new_dataset_block("iris")))
  reload_board(spy, board, shiny::MockShinySession$new())

  expect_identical(staged, board)
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
          paste(
            "Expecting the `preserve_board` return value to evaluate to a",
            "`board` object or a list with a `board` element."
          )
        )
      )
    }
  )
})

test_that("board requests carry the parsed query at both entry points", {

  get_req <- new.env()
  get_req$QUERY_STRING <- "board_name=foo&user=bar"

  get <- board_request_get(get_req)

  expect_type(get, "list")
  expect_identical(get$query, list(board_name = "foo", user = "bar"))
  expect_identical(get$request, get_req)
  expect_null(get$session)

  ws_sess <- list(
    clientData = list(url_search = "?board_name=foo&user=bar"),
    request = get_req
  )

  ws <- board_request_ws(ws_sess)

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
