test_that("board opts", {

  opts1 <- new_board_options(
    new_n_rows_option(),
    new_dark_mode_option()
  )

  expect_s3_class(opts1, "board_options")
  expect_length(opts1, 2L)
  expect_named(opts1, c("n_rows", "dark_mode"))

  expect_s3_class(opts1[[1]], "board_option")
  expect_s3_class(opts1[["n_rows"]], "board_option")

  expect_error(
    new_board_options(new_n_rows_option(), new_n_rows_option()),
    class = "board_options_duplicated_ids"
  )

  opts2 <- c(new_show_conditions_option(), opts1)

  expect_s3_class(opts2, "board_options")
  expect_length(opts2, 3L)
  expect_named(opts2, c("show_conditions", "n_rows", "dark_mode"))

  opts3 <- c(opts1, new_show_conditions_option())

  expect_s3_class(opts3, "board_options")
  expect_length(opts3, 3L)
  expect_named(opts3, c("n_rows", "dark_mode", "show_conditions"))
})

test_that("opt ser/deser", {

  for (opt in default_board_options()) {
    expect_identical(
      opt,
      blockr_deser(blockr_ser(opt)),
      ignore_function_env = TRUE
    )
  }

  reg_opts <- combine_board_options(lapply(available_blocks(), board_options))

  for (opt in reg_opts) {
    expect_identical(
      opt,
      blockr_deser(blockr_ser(opt)),
      ignore_function_env = TRUE
    )
  }

  opts <- list(
    new_dark_mode_option("dark"),
    new_show_conditions_option(character()),
    new_show_conditions_option("error"),
    new_show_conditions_option(c("warning", "error", "message"))
  )

  for (opt in opts) {
    expect_identical(
      opt,
      blockr_deser(blockr_ser(opt)),
      ignore_function_env = TRUE
    )
  }
})

test_that("a board options container has a compact str_value()", {

  opts <- as_board_options(new_board())

  res <- str_value(opts)
  lines <- strsplit(res, "\n")[[1L]]

  expect_length(res, 1L)
  expect_identical(lines[1L], "<board_options[2]>")
  expect_length(lines, 3L)
  expect_true(any(startsWith(lines[-1L], "  show_conditions: ")))

  expect_identical(str_value(new_board_options()), "<board_options[0]>")

  expect_identical(capture.output(str(opts))[1L], " <board_options[2]>")
})
