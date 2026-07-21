test_that("tabular display selection", {

  expect_s3_class(minimal_display, "tabular_display")
  expect_s3_class(dt_display, "tabular_display")

  expect_true(is_tabular_display(new_tabular_display("custom_display")))
  expect_false(is_tabular_display(list()))

  expect_identical(tabular_display(), minimal_display)

  withr::local_options(blockr.tabular_display = dt_display)
  expect_identical(tabular_display(), dt_display)
})

test_that("tabular display falls back on an invalid option", {

  withr::local_options(blockr.tabular_display = "not a display")
  expect_identical(tabular_display(), minimal_display)
})

test_that("block board options follow the active display", {

  blk <- new_dataset_block("iris")

  expect_setequal(names(board_options(blk)), "n_rows")

  withr::local_options(blockr.tabular_display = dt_display)
  expect_setequal(
    names(board_options(blk)),
    c("n_rows", "page_size", "filter_rows")
  )
})

test_that("tabular ui dispatches on the active display", {

  expect_s3_class(tabular_ui(minimal_display, "id"), "shiny.tag")
  expect_s3_class(tabular_ui(dt_display, "id"), "shiny.tag.list")
})

test_that("tabular options are display-specific board_options", {

  expect_setequal(names(tabular_options(minimal_display)), "n_rows")
  expect_setequal(
    names(tabular_options(dt_display)),
    c("n_rows", "page_size", "filter_rows")
  )
})

test_that("the minimal display renders a tibble preview and triggers", {

  blk <- new_dataset_block("iris")

  with_mock_session(
    {
      render <- tabular_output(minimal_display, iris, blk, session)
      expect_type(render, "closure")
      expect_match(render(), "A tibble")

      expect_silent(tabular_trigger(minimal_display, session))
    }
  )
})

test_that("the dt display renders and triggers when active", {

  withr::local_options(blockr.tabular_display = dt_display)

  blk <- new_dataset_block("iris")

  with_mock_session(
    {
      expect_type(tabular_output(dt_display, iris, blk, session), "closure")
      expect_silent(tabular_trigger(dt_display, session))
    }
  )
})
