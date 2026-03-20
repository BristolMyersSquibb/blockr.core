test_that("ser/deser module", {

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

  testServer(
    preserve_board_server,
    {
      session$setInputs(restore = list(datapath = temp))

      expect_s3_class(res(), class(test_board))

      expect_length(board_blocks(res()), length(board_blocks(test_board)))
      expect_length(board_links(res()), length(board_links(test_board)))

      expect_setequal(board_block_ids(res()), board_block_ids(test_board))
      expect_setequal(board_link_ids(res()), board_link_ids(test_board))
    },
    args = list(board = reactiveValues(board = new_board()))
  )
})

test_that("ser/deser board", {

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

  testServer(
    get_s3_method("board_server", test_board),
    {
      ser_deser <- session$makeScope("preserve_board")
      ser_deser$setInputs(restore = list(datapath = temp))

      brd <- board_refresh()

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

test_that("gen_code return validation", {

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

test_that("meta round-trip through serve_obj", {

  id <- "test_meta_roundtrip"
  on.exit(if (is_reloading(id)) finalize_reload(id), add = TRUE)

  board <- new_board()
  meta <- list(url = "/bar", n = 42L)

  update_serve_obj(id, board, meta = meta)
  expect_true(is_reloading(id))

  result <- finalize_reload(id)
  expect_identical(result, meta)
  expect_false(is_reloading(id))
})

test_that("finalize_reload returns NULL when not reloading", {
  expect_null(finalize_reload("nonexistent_test_id"))
})

test_that("update_serve_obj without meta stores NULL meta", {

  id <- "test_no_meta"
  on.exit(if (is_reloading(id)) finalize_reload(id), add = TRUE)

  update_serve_obj(id, new_board())

  result <- finalize_reload(id)
  expect_null(result)
})

test_that("check_ser_deser_val accepts list with board and meta", {

  board <- new_board()

  with_mock_session(
    {
      val <- reactiveVal(list(board = board, meta = list(url = "/test")))
      res <- check_ser_deser_val(val)
      sink_msg(session$flushReact())

      expect_identical(res, val)
    }
  )
})

test_that("dummy ser/deser ui test", {
  expect_s3_class(preserve_board_ui("ser_deser", new_board()), "shiny.tag.list")
})
