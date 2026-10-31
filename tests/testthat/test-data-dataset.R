test_that("dataset block constructor", {

  blk <- new_dataset_block("mtcars")

  expect_s3_class(blk, "dataset_block")
})
