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
