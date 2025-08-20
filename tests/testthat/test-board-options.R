test_that("board opts", {

  opts1 <- new_board_options(
    new_board_name_option(),
    new_n_rows_option(),
    new_page_size_option()
  )

  expect_s3_class(opts1, "board_options")
  expect_length(opts1, 3L)
  expect_named(opts1, c("board_name", "n_rows", "page_size"))

  expect_s3_class(opts1[[1]], "board_option")
  expect_s3_class(opts1[["board_name"]], "board_option")

  expect_error(
    opts1[[1]] <- opts1[[2]],
    class = "board_options_duplicated_ids"
  )

  expect_error(
    opts1[["board_name"]] <- opts1[["n_rows"]],
    class = "board_options_duplicated_ids"
  )

  opts2 <- c(new_filter_rows_option(), opts1)

  expect_s3_class(opts2, "board_options")
  expect_length(opts2, 4L)
  expect_named(opts2, c("filter_rows", "board_name", "n_rows", "page_size"))

  opts3 <- c(opts1, new_filter_rows_option())

  expect_s3_class(opts3, "board_options")
  expect_length(opts3, 4L)
  expect_named(opts3, c("board_name", "n_rows", "page_size", "filter_rows"))
})
