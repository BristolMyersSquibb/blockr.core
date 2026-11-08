test_that("is_board_locked reflects the blockr.locked option", {

  withr::local_options(blockr.locked = NULL)
  expect_false(is_board_locked(new_board()))

  withr::local_options(blockr.locked = TRUE)
  expect_true(is_board_locked(new_board()))
})
