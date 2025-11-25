test_that("dummy text block ui test", {
  expect_s3_class(block_ui("test", new_glue_block()), "shiny.tag.list")
})
