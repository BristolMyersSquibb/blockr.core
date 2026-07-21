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

test_that("serve_board_srv stages and reloads when board_refresh fires", {

  refresh <- reactiveVal()
  staged <- NULL

  spy <- board_loader(
    resolve = function(request, session, default) NULL,
    stage = function(board, session) staged <<- board
  )

  local_mocked_bindings(
    board_server = function(id, x, ...) list(board_refresh = refresh),
    blockr_test_exports = function(x, rv, ...) invisible()
  )

  board <- new_board(blocks = c(a = new_dataset_block("iris")))

  srv <- serve_board_srv(
    "app", board, spy, blockr_app_plugins, blockr_app_options
  )

  testServer(
    srv,
    {
      refresh(board)
      session$flushReact()

      expect_identical(staged, board)
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
    resolve = function(request, session, default) NULL,
    stage = function(board, session) NULL
  )

  expect_true(is_board_loader(ld))
  expect_true(is.function(ld$resolve))
  expect_true(is.function(ld$stage))

  stageless <- board_loader(function(request, session, default) NULL)
  expect_true(is_board_loader(stageless))
  expect_null(stageless$stage)

  expect_error(
    board_loader(function(x) NULL),
    class = "board_loader_resolve_invalid"
  )
  expect_error(
    board_loader(
      function(request, session, default) NULL,
      function(b, s) NULL
    ),
    class = "board_loader_stage_invalid"
  )

  # formals must match exactly -- missing/extra args (or `...`) are rejected
  expect_error(
    board_loader(function(request, session) NULL),
    class = "board_loader_resolve_invalid"
  )
  expect_error(
    board_loader(function(...) NULL),
    class = "board_loader_resolve_invalid"
  )
  expect_error(
    board_loader(
      function(request, session, default) NULL,
      function(board, session, x) NULL
    ),
    class = "board_loader_stage_invalid"
  )
})

test_that("the default loader stages a board and resolves it back", {

  ld <- local_loader()
  board <- new_board(blocks = c(a = new_dataset_block("iris")))

  written <- NULL
  session <- list(
    clientData = list(url_search = ""),
    updateQueryString = function(url, mode) written <<- url
  )

  ld$stage(board, session)
  expect_true(grepl(reload_param, written, fixed = TRUE))

  # the staged board resolves back from the URL the stage wrote
  get_req <- list(QUERY_STRING = sub("^[?]", "", written))
  expect_identical(ld$resolve(get_req, NULL, board), board)
  expect_null(ld$resolve(list(QUERY_STRING = ""), NULL, board))

  # a separate loader instance has its own store
  expect_null(local_loader()$resolve(get_req, NULL, board))
})

test_that("the default loader consumes the token at the WS read", {

  ld <- local_loader()
  board <- new_board(blocks = c(a = new_dataset_block("iris")))

  written <- NULL
  staging <- list(
    clientData = list(url_search = ""),
    updateQueryString = function(url, mode) written <<- url
  )
  ld$stage(board, staging)

  # WS read off clientData consumes the token from the store
  ws <- list(
    clientData = list(url_search = written),
    updateQueryString = function(url, mode) NULL
  )
  expect_identical(ld$resolve(NULL, ws, board), board)

  # consumed: a GET read by the same query no longer finds it
  get_req <- list(QUERY_STRING = sub("^[?]", "", written))
  expect_null(ld$resolve(get_req, NULL, board))
})

test_that("resolve_board prefers the loader board, validates, falls back", {

  default <- new_board()
  staged <- new_board(blocks = c(a = new_dataset_block("iris")))

  # resolve_board augments the returned board's options (covered below), so the
  # picked board is no longer identical to the input -- compare by blocks.
  pref <- board_loader(function(request, session, default) staged)
  expect_identical(
    board_blocks(resolve_board(default, pref, list(), NULL)),
    board_blocks(staged)
  )

  none <- board_loader(function(request, session, default) NULL)
  expect_identical(
    board_blocks(resolve_board(default, none, list(), NULL)),
    board_blocks(default)
  )

  bad <- board_loader(function(request, session, default) "nope")
  expect_error(
    resolve_board(default, bad, list(), NULL),
    class = "invalid_board_loader"
  )
})

test_that("resolve_board augments the board with the managed option set", {

  # A board declares only its own options, but the settings UI manages the
  # wider blockr_app_options() set -- options contributed by registered blocks
  # (e.g. the preview-row count `n_rows`). resolve_board() bakes that set onto
  # the board it returns so the server and serialization see the full set.
  brd <- new_board()

  expect_false("n_rows" %in% names(board_options(brd)))
  expect_true("n_rows" %in% names(blockr_app_options(brd)))

  null_loader <- board_loader(function(request, session, default) NULL)
  via_default <- resolve_board(
    brd, null_loader, request = list(), session = NULL
  )
  expect_true("n_rows" %in% names(board_options(via_default)))

  load_loader <- board_loader(function(request, session, default) new_board())
  via_loaded <- resolve_board(
    brd, load_loader, request = list(), session = NULL
  )
  expect_true("n_rows" %in% names(board_options(via_loaded)))
})

test_that("a user-changed block-backed board option survives save/restore", {

  # End to end of the augmentation: `n_rows` is contributed by blocks, not the
  # board itself, so before resolve_board() widened the board it was dropped on
  # save and reverted to its registry default on reload.
  x <- resolve_board(
    new_board(),
    board_loader(function(request, session, default) NULL),
    request = list(),
    session = NULL
  )

  ser <- NULL

  testServer(
    get_s3_method("board_server", x),
    {
      session$flushReact()

      session$setInputs(n_rows = 100L)
      session$flushReact()

      ser <<- serialize_board(rv$board, rv$blocks, id = "b", session = session)
    },
    args = list(
      x = x,
      plugins = list(manage_blocks()),
      options = blockr_app_options(x)
    )
  )

  loaded <- blockr_deser(ser)

  expect_true("n_rows" %in% names(board_options(loaded)))
  expect_identical(
    board_option_value(blockr_app_options(loaded)[["n_rows"]]),
    100L
  )
})

test_that("resolve_query reads the query at both request phases", {

  get <- resolve_query(list(QUERY_STRING = "view=sales&tab=2"), NULL)
  expect_identical(get[["view"]], "sales")
  expect_identical(get[["tab"]], "2")

  ws <- resolve_query(NULL, list(clientData = list(url_search = "?view=sales")))
  expect_identical(ws[["view"]], "sales")

  expect_length(resolve_query(list(), NULL), 0L)
  expect_length(
    resolve_query(NULL, list(clientData = list(url_search = ""))),
    0L
  )
})

test_that("serve_board_ui threads the request query to blockr_app_ui", {

  seen <- NULL

  local_mocked_bindings(
    blockr_app_ui = function(id, x, ..., query = list()) {
      seen <<- query
      NULL
    }
  )

  ui <- serve_board_ui(
    "app", new_board(), local_loader(), blockr_app_plugins, blockr_app_options
  )
  ui(list(QUERY_STRING = "view=sales&tab=2"))

  expect_identical(seen[["view"]], "sales")
  expect_identical(seen[["tab"]], "2")
})

test_that("serve_board_srv threads the session query to blockr_app_server", {

  seen <- NULL

  local_mocked_bindings(
    resolve_query = function(request, session) {
      if (is.null(session)) list() else list(view = "ops")
    },
    blockr_app_server = function(id, x, ..., query = list()) {
      seen <<- query
      list()
    },
    blockr_test_exports = function(x, rv, ...) invisible()
  )

  srv <- serve_board_srv(
    "app", new_board(), local_loader(), blockr_app_plugins, blockr_app_options
  )

  testServer(srv, session$flushReact())

  expect_identical(seen[["view"]], "ops")
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

test_that("block-contributed board options survive save/restore", {

  brd <- new_board(blocks = c(a = new_dataset_block("iris")))

  temp <- withr::local_tempfile(fileext = ".json")

  testServer(
    get_s3_method("board_server", brd),
    {
      session$flushReact()
      session$setInputs(n_rows = "25")
      session$flushReact()

      ser_deser <- session$makeScope("preserve_board")
      file.copy(ser_deser$output$serialize, temp)
    },
    args = list(
      x = brd,
      plugins = preserve_board(),
      options = blockr_app_options(brd)
    )
  )

  restored <- blockr_deser(read_json(temp))

  expect_true("n_rows" %in% board_option_ids(restored))
  expect_identical(
    board_option_value(blockr_app_options(restored)[["n_rows"]]),
    25L
  )
})

test_that("serialize_board uses constructor state for a not-yet-built block", {

  board <- new_board(blocks = c(a = new_dataset_block("iris")))

  # `blocks` empty: block `a` has not been constructed (still building in the
  # background), so it serializes from its constructor defaults.
  ser <- serialize_board(board, list(), id = "board")

  expect_identical(
    ser$payload$blocks$payload$a$payload$dataset,
    "iris"
  )
})

test_that("dummy ser/deser ui test", {
  expect_s3_class(preserve_board_ui("ser_deser", new_board()), "shiny.tag.list")
})
