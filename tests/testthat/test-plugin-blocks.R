test_that("add/rm blocks", {

  testServer(
    manage_blocks_server,
    {
      expect_null(update())

      session$setInputs(
        registry_select = "dataset_block",
        block_id = "a",
        confirm_add = 1
      )

      session$flushReact()

      res <- update()$blocks

      expect_s3_class(res$add, "blocks")
      expect_length(res$add, 1L)
      expect_null(res$rm)

      expect_null(session$returned)
    },
    args = list(
      board = reactiveValues(board = new_board()),
      update = reactiveVal()
    )
  )

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(from = "a", to = "b")
  )

  testServer(
    manage_blocks_server,
    {
      expect_null(update())

      session$setInputs(block_select = "a", confirm_rm = 1)

      session$flushReact()

      res <- update()$blocks

      expect_type(res$rm, "character")
      expect_length(res$rm, 1L)
      expect_null(res$add)

      expect_null(session$returned)
    },
    args = list(board = reactiveValues(board = board), update = reactiveVal())
  )

  testServer(
    manage_blocks_server,
    {
      expect_null(update())

      session$setInputs(confirm_add = 1)
      session$setInputs(block_id = "", confirm_add = 2)
      session$setInputs(block_id = "a", confirm_add = 3)
      session$setInputs(block_id = "c", registry_select = "abc",
                        confirm_add = 4)
      session$setInputs(cancel_add = 1)

      session$setInputs(confirm_rm = 1)
      session$setInputs(block_select = "x", confirm_rm = 2)
      session$setInputs(cancel_rm = 1)

      res <- update()$blocks

      expect_null(res$add)
      expect_null(res$rm)

      expect_null(session$returned)
    },
    args = list(board = reactiveValues(board = board), update = reactiveVal())
  )
})

test_that("add/rm blocks return validation", {

  with_mock_session(
    {
      expect_error(
        validate_board_update(
          reactiveVal(list(blocks = "a")),
          new_board()
        ),
        class = "board_update_component_type_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(list(blocks = list(abc = NULL))),
          new_board()
        ),
        class = "board_update_component_components_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(list(blocks = list(add = "a"))),
          new_board()
        ),
        class = "board_update_blocks_add_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(
            list(blocks = list(add = blocks(a = new_dataset_block())))
          ),
          new_board(blocks = blocks(a = new_dataset_block()))
        ),
        class = "board_update_blocks_add_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(list(blocks = list(rm = 1))),
          new_board()
        ),
        class = "board_update_blocks_rm_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(list(blocks = list(rm = "a"))),
          new_board()
        ),
        class = "board_update_blocks_rm_invalid"
      )
    }
  )
})

test_that("dummy add/rm block ui test", {
  expect_s3_class(manage_blocks_ui("add_rm", new_board()), "shiny.tag.list")
})
