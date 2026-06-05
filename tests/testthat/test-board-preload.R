test_that("validate_preload_result accepts NULL and well-formed results", {

  expect_null(validate_preload_result(NULL))

  board <- new_board(blocks = c(a = new_dataset_block("BOD")))

  res <- validate_preload_result(list(board = board))
  expect_true(is_board(res$board))
  expect_null(res$meta)

  res <- validate_preload_result(list(board = board, meta = list(url = "?x")))
  expect_identical(res$meta, list(url = "?x"))
})

test_that("validate_preload_result rejects malformed results", {

  board <- new_board(blocks = c(a = new_dataset_block("BOD")))

  expect_error(
    validate_preload_result(list(meta = list())),
    class = "invalid_board_preload"
  )

  expect_error(
    validate_preload_result(board),
    class = "invalid_board_preload"
  )

  expect_error(
    validate_preload_result(list(board = board, meta = "nope")),
    class = "invalid_board_preload"
  )
})

test_that("register_board_preload warns only when replacing a live callback", {

  withr::defer(register_board_preload(NULL))

  register_board_preload(NULL)

  expect_silent(register_board_preload(function(query, req) NULL))

  expect_warning(
    register_board_preload(function(query, req) NULL),
    class = "board_preload_replaced"
  )

  expect_silent(register_board_preload(NULL))
})
