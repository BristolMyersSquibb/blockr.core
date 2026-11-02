test_that("a board option holds its ID and the value a script gives", {

  opt <- new_board_option("test", "test")

  expect_true(is_board_option(opt))
  expect_s3_class(opt, c("test_option", "board_option"), exact = TRUE)
  expect_identical(validate_board_option(opt), opt)
  expect_identical(as_board_option(opt), opt)
  expect_identical(board_option_id(opt), "test")
  expect_identical(board_option_value(opt), "test")

  expect_snapshot(opt)

  expect_error(
    new_board_option(1L, "test"),
    class = "board_option_component_id_invalid"
  )

  expect_error(
    new_board_option("", "test"),
    class = "board_option_component_id_invalid"
  )

  expect_error(
    validate_board_option(list()),
    class = "board_option_inheritance_invalid"
  )

  expect_error(
    validate_board_option(structure(list(), id = 1L, class = "board_option")),
    class = "board_option_component_id_invalid"
  )
})

test_that("a board option made without a value has none", {

  opt <- new_board_option("test")

  expect_false(has_board_option_value(opt))
  expect_null(board_option_value(opt))
  expect_identical(format(opt), "test: <unset>")

  # A value of `NULL` is a value, which JSON writes as null
  opt <- new_board_option("test", NULL)

  expect_true(has_board_option_value(opt))
  expect_null(board_option_value(opt))
})

test_that("core's board options take their value alone and compute nothing", {

  ctors <- list(
    n_rows = new_n_rows_option,
    dark_mode = new_dark_mode_option,
    show_conditions = new_show_conditions_option
  )

  withr::local_options(
    blockr.n_rows = 10L,
    blockr.dark_mode = "dark",
    blockr.show_conditions = "error"
  )

  for (id in names(ctors)) {

    expect_identical(names(formals(ctors[[id]])), "value", info = id)

    opt <- ctors[[id]]()

    expect_identical(board_option_id(opt), id)
    expect_false(has_board_option_value(opt), info = id)
  }

  # The definitions in the client check a value, and R does not
  expect_identical(board_option_value(new_n_rows_option(-1L)), -1L)
  expect_identical(board_option_value(new_dark_mode_option(TRUE)), TRUE)
  expect_identical(
    board_option_value(new_show_conditions_option(-1L)),
    -1L
  )
})

test_that("a board option has a compact str_value()", {

  opt <- new_n_rows_option(10L)

  expect_identical(str_value(opt), "n_rows: 10")
  expect_identical(str_value(opt), format(opt))

  expect_identical(capture.output(str(opt)), " n_rows: 10")
})
