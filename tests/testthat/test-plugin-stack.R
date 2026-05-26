test_that("edit blocks", {

  testServer(
    edit_stack_server,
    {
      expect_null(update())

      session$setInputs(stack_name_in = "data")

      session$flushReact()

      res <- update()$stacks

      expect_length(res$add, 0L)
      expect_length(res$rm, 0L)
      expect_length(res$mod, 1L)

      expect_named(res$mod, "a")
      expect_identical(res$mod[["a"]], list(name = "data"))

      expect_null(session$returned)
    },
    args = list(
      stack_id = "a",
      board = reactiveValues(
        board = new_board(
          blocks = blocks(a = new_dataset_block("iris")),
          stacks = stacks(a = "a")
        )
      ),
      update = reactiveVal()
    )
  )

  testServer(
    edit_stack_server,
    {
      expect_null(update())

      session$setInputs(rm_stack = 1)

      session$flushReact()

      res <- update()$stacks

      expect_length(res$add, 0L)
      expect_length(res$mod, 0L)

      expect_identical(res$rm, "a")

      expect_null(session$returned)
    },
    args = list(
      stack_id = "a",
      board = reactiveValues(
        board = new_board(
          blocks = blocks(a = new_dataset_block("iris")),
          stacks = stacks(a = "a")
        )
      ),
      update = reactiveVal()
    )
  )
})

test_that("dummy edit block ui test", {
  expect_s3_class(
    edit_stack_ui("edit_stack", new_stack()),
    "shiny.tag.list"
  )
})
