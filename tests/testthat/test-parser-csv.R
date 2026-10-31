test_that("csv block constructor", {

  blk <- new_csv_block()

  expect_s3_class(blk, "csv_block")
})
