test_that("edit blocks", {

  testServer(
    edit_block_server,
    {
      expect_null(update())

      session$setInputs(block_name_in = "iris")

      session$flushReact()

      res <- update()$blocks

      expect_length(res$add, 0L)
      expect_length(res$rm, 0L)
      expect_length(res$mod, 1L)

      expect_named(res$mod, "a")
      expect_identical(block_name(res$mod[["a"]]), "iris")

      expect_null(session$returned)
    },
    args = list(
      block_id = "a",
      board = reactiveValues(
        board = new_board(blocks = list(a = new_dataset_block("iris")))
      ),
      update = reactiveVal()
    )
  )

  testServer(
    edit_block_server,
    {
      expect_null(update())

      session$setInputs(
        add_block_after = 1,
        registry_select = "subset_block",
        block_id = "c",
        input_select = "data",
        link_id = "bc",
        confirm_insert = 1
      )

      session$flushReact()

      blk <- update()$blocks

      expect_length(blk$add, 1L)
      expect_length(blk$mod, 0L)
      expect_length(blk$rm, 0L)

      lnk <- update()$links

      expect_length(lnk$add, 1L)
      expect_length(lnk$mod, 0L)
      expect_length(lnk$rm, 0L)

      expect_null(session$returned)
    },
    args = list(
      block_id = "b",
      board = reactiveValues(
        board = new_board(
          blocks = c(
            a = new_dataset_block("iris"),
            b = new_merge_block()
          ),
          links = list(from = "a", to = "b", input = "x"),
          stacks = list(ab = c("a", "b"))
        )
      ),
      update = reactiveVal()
    )
  )

  testServer(
    edit_block_server,
    {
      expect_null(update())

      session$setInputs(
        add_block_before = 1,
        registry_select = "dataset_block",
        block_id = "c",
        input_select = "y",
        link_id = "cb",
        confirm_insert = 1
      )

      session$flushReact()

      blk <- update()$blocks

      expect_length(blk$add, 1L)
      expect_length(blk$mod, 0L)
      expect_length(blk$rm, 0L)

      lnk <- update()$links

      expect_length(lnk$add, 1L)
      expect_length(lnk$mod, 0L)
      expect_length(lnk$rm, 0L)

      expect_null(session$returned)
    },
    args = list(
      block_id = "b",
      board = reactiveValues(
        board = new_board(
          blocks = c(
            a = new_dataset_block("iris"),
            b = new_merge_block()
          ),
          links = list(from = "a", to = "b", input = "x"),
          stacks = list(ab = c("a", "b"))
        )
      ),
      update = reactiveVal()
    )
  )

  testServer(
    edit_block_server,
    {
      expect_null(update())

      session$setInputs(
        add_block_before = 1,
        registry_select = "dataset_block",
        block_id = "c",
        input_select = "y",
        link_id = "cb",
        add_to_stack = TRUE,
        confirm_insert = 1
      )

      session$flushReact()

      blk <- update()$blocks

      expect_length(blk$add, 1L)
      expect_length(blk$mod, 0L)
      expect_length(blk$rm, 0L)

      lnk <- update()$links

      expect_length(lnk$add, 1L)
      expect_length(lnk$mod, 0L)
      expect_length(lnk$rm, 0L)

      stk <- update()$stacks

      expect_length(stk$add, 1L)
      expect_length(stk$mod, 0L)
      expect_length(stk$rm, 1L)

      expect_null(session$returned)
    },
    args = list(
      block_id = "b",
      board = reactiveValues(
        board = new_board(
          blocks = c(
            a = new_dataset_block("iris"),
            b = new_merge_block()
          ),
          links = list(from = "a", to = "b", input = "x"),
          stacks = list(ab = c("a", "b"))
        )
      ),
      update = reactiveVal()
    )
  )
})

test_that("dummy edit block ui test", {
  expect_s3_class(
    edit_block_ui(new_dataset_block(), "edit_block"),
    "shiny.tag.list"
  )
})
