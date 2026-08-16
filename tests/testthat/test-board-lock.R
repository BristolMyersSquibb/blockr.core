test_that("is_board_locked reflects the blockr.locked option", {

  withr::local_options(blockr.locked = NULL)
  expect_false(is_board_locked(new_board()))

  withr::local_options(blockr.locked = TRUE)
  expect_true(is_board_locked(new_board()))
})

test_that("is_board_locked dispatches on board subclass", {

  withr::local_options(blockr.locked = TRUE)

  registerS3method(
    "is_board_locked", "test_unlockable_board",
    function(board, ...) FALSE,
    envir = asNamespace("blockr.core")
  )

  expect_false(is_board_locked(new_board(class = "test_unlockable_board")))
})

test_that("a locked board drops structural board updates", {

  withr::local_options(blockr.locked = TRUE)

  board <- new_board()

  testServer(
    get_s3_method("board_server", board),
    {
      board_update(
        list(blocks = list(add = as_blocks(new_dataset_block())))
      )

      session$flushReact()

      expect_length(board_blocks(rv$board), 0L)
      expect_null(board_update())
    },
    args = list(x = board, plugins = list(manage_blocks()))
  )
})

test_that("a locked board records the outcome of what it dropped", {

  withr::local_options(blockr.locked = TRUE)

  board <- new_board()

  testServer(
    get_s3_method("board_server", board),
    {
      board_update(
        list(blocks = list(add = as_blocks(new_dataset_block())))
      )

      session$flushReact()

      expect_false(rv$last_update$ok)
      expect_identical(rv$last_update$phase, "validate")
    },
    args = list(x = board, plugins = list(manage_blocks()))
  )
})

test_that("a locked board still accepts block requests", {

  withr::local_options(blockr.locked = TRUE)

  board <- new_board(blocks = c(a = new_dataset_block("BOD")))

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      # A request carries no state change, so the lock does not apply to it.
      board_update(list(require = list(consumer = list(set = "a"))))
      session$flushReact()

      expect_identical(rv$required_blocks(), list(consumer = "a"))
      expect_true(rv$last_update$ok)

      board_update(list(construct = "a"))
      session$flushReact()

      expect_true(rv$last_update$ok)

      # Mixed with one that does, the payload is dropped whole.
      board_update(
        list(
          blocks = list(add = as_blocks(list(b = new_dataset_block("BOD")))),
          require = list(consumer = list(set = character()))
        )
      )
      session$flushReact()

      expect_false(rv$last_update$ok)
      expect_length(board_blocks(rv$board), 1L)
      expect_identical(rv$required_blocks(), list(consumer = "a"))
    },
    args = list(x = board, plugins = list())
  )
})

test_that("an unlocked board applies structural board updates", {

  board <- new_board()

  testServer(
    get_s3_method("board_server", board),
    {
      board_update(
        list(blocks = list(add = as_blocks(new_dataset_block())))
      )

      session$flushReact()

      expect_length(board_blocks(rv$board), 1L)
    },
    args = list(x = board, plugins = list(manage_blocks()))
  )
})

test_that("the gate consults is_board_locked, not the raw option", {

  withr::local_options(blockr.locked = TRUE)

  registerS3method(
    "is_board_locked", "test_unlockable_board",
    function(board, ...) FALSE,
    envir = asNamespace("blockr.core")
  )

  board <- new_board(class = "test_unlockable_board")

  testServer(
    get_s3_method("board_server", board),
    {
      board_update(
        list(blocks = list(add = as_blocks(new_dataset_block())))
      )

      session$flushReact()

      expect_length(board_blocks(rv$board), 1L)
    },
    args = list(x = board, plugins = list(manage_blocks()))
  )
})

test_that("set_board_option_value is ignored on a locked board", {

  withr::local_options(blockr.locked = TRUE)

  board <- new_board()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      set_board_option_value("n_rows", 7L, board, session)
      expect_equal(get_board_option_value("n_rows", session), 50L)
    },
    args = list(
      x = board,
      options = as_board_options(new_n_rows_option())
    )
  )
})

test_that("a forged option input is ignored on a locked board", {

  withr::local_options(blockr.locked = TRUE)

  board <- new_board()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      session$setInputs(n_rows = 7L)
      session$flushReact()

      expect_equal(get_board_option_value("n_rows", session), 50L)
    },
    args = list(
      x = board,
      options = as_board_options(new_n_rows_option())
    )
  )
})

test_that("an option input updates an unlocked board", {

  board <- new_board()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      session$setInputs(n_rows = 8L)
      session$flushReact()

      expect_equal(get_board_option_value("n_rows", session), 8L)
    },
    args = list(
      x = board,
      options = as_board_options(new_n_rows_option())
    )
  )
})
