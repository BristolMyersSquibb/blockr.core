test_that("ser/deser module", {

  test_board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(from = "a", to = "b")
  )

  temp <- tempfile(fileext = ".json")

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

  temp <- tempfile(fileext = ".json")

  testServer(
    get_s3_method("board_server", test_board),
    {
      ser_deser <- session$makeScope("preserve_board")
      file.copy(ser_deser$output$serialize, temp)
    },
    args = list(
      x = test_board,
      plugins = list(preserve_board())
    )
  )

  testServer(
    get_s3_method("board_server", test_board),
    {
      ser_deser <- session$makeScope("preserve_board")
      ser_deser$setInputs(restore = list(datapath = temp))

      brd <- rv$board

      expect_length(board_blocks(brd), length(board_blocks(test_board)))
      expect_length(board_links(brd), length(board_links(test_board)))

      expect_setequal(board_block_ids(brd), board_block_ids(test_board))
      expect_setequal(board_link_ids(brd), board_link_ids(test_board))
    },
    args = list(
      x = new_board(),
      plugins = list(preserve_board())
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
            "`board` object."
          )
        )
      )
    }
  )
})

test_that("dummy ser/deser ui test", {
  expect_s3_class(preserve_board_ui("ser_deser", new_board()), "shiny.tag.list")
})
