test_that("the frozen write-channel records hidden-input blocks", {

  board <- new_board(blocks = c(a = new_dataset_block("iris")))

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$frozen, "a")
    },
    args = list(
      x = board,
      plugins = list(),
      callbacks = function(frozen, ...) {
        frozen("a")
        NULL
      }
    )
  )
})

test_that("a frozen block ignores a forged input", {

  blk <- new_dataset_block("iris")

  board <- reactiveValues(board = new_board(), frozen = "data")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      expect_equal(session$returned$result(), iris)

      session$makeScope("expr")$setInputs(dataset = "mtcars")
      session$flushReact()

      expect_equal(session$returned$result(), iris)
    },
    args = list(
      x = blk,
      data = list(),
      block_id = "data",
      board = board
    )
  )
})

test_that("freezing and unfreezing toggles a block's input handling", {

  blk <- new_dataset_block("iris")

  board <- reactiveValues(board = new_board(), frozen = character())

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      session$makeScope("expr")$setInputs(dataset = "mtcars")
      session$flushReact()

      expect_equal(session$returned$result(), mtcars)

      board$frozen <- "data"
      session$flushReact()

      session$makeScope("expr")$setInputs(dataset = "iris")
      session$flushReact()

      expect_equal(session$returned$result(), mtcars)

      board$frozen <- character()
      session$flushReact()

      session$makeScope("expr")$setInputs(dataset = "iris")
      session$flushReact()

      expect_equal(session$returned$result(), iris)
    },
    args = list(
      x = blk,
      data = list(),
      block_id = "data",
      board = board
    )
  )
})

test_that("a frozen block still re-evaluates on upstream data changes", {

  blk <- new_subset_block("Sepal.Width > 3")

  data <- reactiveVal(iris)

  board <- reactiveValues(board = new_board(), frozen = "sub")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      expect_equal(
        session$returned$result(),
        subset(iris, Sepal.Width > 3)
      )

      session$makeScope("expr")$setInputs(
        subset = "Sepal.Width > 99",
        select = "",
        eval = 1L
      )
      session$flushReact()

      expect_equal(
        session$returned$result(),
        subset(iris, Sepal.Width > 3)
      )

      data(head(iris, 20L))
      session$flushReact()

      expect_equal(
        session$returned$result(),
        subset(head(iris, 20L), Sepal.Width > 3)
      )
    },
    args = list(
      x = blk,
      data = list(data = data),
      block_id = "sub",
      board = board
    )
  )
})
