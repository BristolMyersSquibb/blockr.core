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
