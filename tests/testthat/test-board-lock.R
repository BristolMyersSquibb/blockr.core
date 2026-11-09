test_that("is_board_locked reflects the blockr.locked option", {

  withr::local_options(blockr.locked = NULL)
  expect_false(is_board_locked(new_board()))

  withr::local_options(blockr.locked = TRUE)
  expect_true(is_board_locked(new_board()))
})

test_that("is_board_locked reads BLOCKR_LOCKED as as.logical() reads it", {

  withr::local_options(blockr.locked = NULL)

  withr::local_envvar(BLOCKR_LOCKED = "TRUE")
  expect_true(is_board_locked(new_board()))

  withr::local_envvar(BLOCKR_LOCKED = "false")
  expect_false(is_board_locked(new_board()))

  withr::local_envvar(BLOCKR_LOCKED = "yes")
  expect_error(
    is_board_locked(new_board()),
    "The blockr option `locked` is TRUE or FALSE.",
    class = "blockr_option_invalid",
    fixed = TRUE
  )
})
