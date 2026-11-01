test_that("opt utils", {

  opt <- new_board_option(
    "test",
    "test",
    ctor = "new_board_option",
    pkg = "blockr.core"
  )

  expect_true(is_board_option(opt))
  expect_identical(validate_board_option(opt), opt)
  expect_identical(as_board_option(opt), opt)

  expect_error(
    validate_board_option(list()),
    class = "board_option_inheritance_invalid"
  )

  expect_error(
    validate_board_option(structure(list(), id = 1L, class = "board_option")),
    class = "board_option_component_id_invalid"
  )

  expect_error(
    validate_board_option(
      structure(list(transform = "test"), id = "test", class = "board_option")
    ),
    class = "board_option_component_transform_invalid"
  )

  expect_snapshot(opt)
})

test_that("a board option's label defaults to its ID in sentence case", {

  new_test_opt <- function(value = "test", ...) {
    new_board_option("test_opt", value, ...)
  }

  expect_identical(board_option_label(new_test_opt()), "Test opt")

  expect_identical(
    board_option_label(new_test_opt(label = "My label")),
    "My label"
  )

  expect_error(
    new_test_opt(label = 1L),
    class = "board_option_component_label_invalid"
  )
})

test_that("core board options take a label", {

  ctors <- c(
    "new_n_rows_option", "new_dark_mode_option", "new_show_conditions_option"
  )

  for (ctor in ctors) {

    opt <- do.call(ctor, list(label = "My label"))

    expect_identical(board_option_label(opt), "My label", info = ctor)
  }
})

test_that("board n_rows opt", {

  expect_true(is_board_option(new_n_rows_option()))

  expect_error(
    new_n_rows_option(-1L),
    class = "board_options_n_rows_invalid"
  )
})

test_that("board dark_mode opt", {

  expect_true(is_board_option(new_dark_mode_option()))
  expect_true(is_board_option(new_dark_mode_option(TRUE)))
  expect_true(is_board_option(new_dark_mode_option(FALSE)))

  expect_error(
    new_dark_mode_option(-1L),
    class = "board_options_dark_mode_invalid"
  )
})

test_that("board show_conditions opt", {

  expect_true(is_board_option(new_show_conditions_option()))

  expect_error(
    new_show_conditions_option(-1L),
    class = "board_options_show_conditions_invalid"
  )
})

test_that("a board option has a compact str_value()", {

  opt <- new_n_rows_option(10L)

  expect_identical(str_value(opt), "n_rows: 10")
  expect_identical(str_value(opt), format(opt))

  expect_identical(capture.output(str(opt)), " n_rows: 10")
})
