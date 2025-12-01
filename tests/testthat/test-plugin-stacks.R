test_that("add/rm stacks", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    )
  )

  testServer(
    manage_stacks_server,
    {
      expect_null(session$returned)
      expect_null(update())

      expect_identical(upd$add, stacks())
      expect_identical(upd$rm, character())
      expect_null(upd$edit)

      expect_null(update())

      session$setInputs(add_stack = 1)

      session$flushReact()

      expect_s3_class(upd$add, "stacks")
      expect_length(upd$add, 1L)

      stk <- names(upd$add)
      new <- as_stacks(
        set_names(list(new_stack(name = stack_name(upd$add[[1L]]))), stk)
      )

      expect_identical(upd$add, new)

      upd$edit <- list(row = names(upd$add), col = "blocks", val = "a")
      stack_blocks(new[[1]]) <- "a"

      session$flushReact()
      expect_identical(upd$add, new)

      upd$edit <- list(row = names(upd$add), col = "name", val = "my stack")
      stack_name(new[[1]]) <- "my stack"

      session$flushReact()
      expect_identical(upd$add, new)

      session$setInputs(modify_stacks = 1)

      expect_identical(
        update()$stacks,
        list(add = new, rm = NULL, mod = NULL)
      )

      expect_identical(upd$add, stacks())
      expect_identical(upd$rm, character())
      expect_identical(upd$mod, stacks())

      expect_null(session$returned)
    },
    args = list(board = reactiveValues(board = board), update = reactiveVal())
  )

  testServer(
    manage_stacks_server,
    {
      expect_null(upd$edit)
      expect_length(upd$add, 0L)
      expect_length(upd$mod, 0L)

      session$flushReact()

      expect_length(upd$obs, 1L)
      expect_named(upd$obs, "ac")
      expect_type(upd$obs, "list")

      expect_length(upd$obs[["ac"]], 2L)
      expect_named(upd$obs[["ac"]], c("name", "blocks"))
      expect_type(upd$obs[["ac"]], "list")

      for (i in c("name", "blocks")) {
        expect_s3_class(upd$obs[["ac"]][[i]], "Observer")
      }

      session$setInputs(ac_name = "some stack")
      expect_identical(
        upd$edit,
        list(row = "ac", col = "name", val = "some stack")
      )
      expect_length(upd$add, 0L)
      expect_length(upd$mod, 1L)

      session$setInputs(ac_blocks = c("a", "b"))
      expect_identical(
        upd$edit,
        list(row = "ac", col = "blocks", val = c("a", "b"))
      )
      expect_length(upd$add, 0L)
      expect_length(upd$mod, 1L)
    },
    args = list(
      board = list(
        board = new_board(
          blocks = c(
            a = new_dataset_block("iris"),
            b = new_dataset_block("mtcars"),
            c = new_subset_block(),
            d = new_subset_block()
          ),
          stacks = stacks(ac = c("a", "c"))
        )
      ),
      update = reactiveVal()
    )
  )

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    stacks = stacks(ab = c("a", "b"))
  )

  testServer(
    manage_stacks_server,
    {
      expect_null(session$returned)
      expect_null(update())

      expect_s3_class(upd$curr, "stacks")
      expect_length(upd$curr, 1L)
      expect_named(upd$curr, "ab")

      expect_identical(upd$rm, character())

      session$setInputs(stacks_dt_rows_selected = 1, rm_stack = 1)

      expect_identical(upd$rm, "ab")

      session$setInputs(modify_stacks = 1)

      expect_identical(
        update()$stacks,
        list(add = NULL, rm = "ab", mod = NULL)
      )

      expect_identical(upd$add, stacks())
      expect_identical(upd$rm, character())
      expect_identical(upd$mod, stacks())
      expect_identical(upd$curr, stacks())

      expect_null(session$returned)
    },
    args = list(board = reactiveValues(board = board), update = reactiveVal())
  )
})

test_that("add/rm stacks return validation", {

  with_mock_session(
    {
      val <- reactiveVal(
        list(stacks = list(add = stacks(a = "a"), rm = "ab"))
      )

      brd <- new_board(
        blocks = c(
          a = new_dataset_block("iris"),
          b = new_subset_block()
        ),
        stacks = stacks(ab = c("a", "b"))
      )

      expect_null(
        validate_board_update(val, brd)
      )
    }
  )

  with_mock_session(
    {
      expect_error(
        validate_board_update(
          reactiveVal(list(stacks = "a")),
          new_board()
        ),
        class = "board_update_component_type_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(list(stacks = list(abc = NULL))),
          new_board()
        ),
        class = "board_update_component_components_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(list(stacks = list(add = "a"))),
          new_board()
        ),
        class = "board_update_add_component_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(
            list(stacks = list(add = stacks(a = new_stack())))
          ),
          new_board(
            blocks = c(a = new_dataset_block()),
            stacks = stacks(a = "a")
          )
        ),
        class = "board_update_stacks_add_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(list(stacks = list(rm = 1))),
          new_board()
        ),
        class = "board_update_rm_component_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(list(stacks = list(rm = "a"))),
          new_board()
        ),
        class = "board_update_stacks_rm_invalid"
      )
    }
  )
})

test_that("dummy ad/rm stack ui test", {
  expect_s3_class(manage_stacks_ui("stack", new_board()), "shiny.tag.list")
  expect_s3_class(stacks_modal(NS("stacks")), "shiny.tag")
})
