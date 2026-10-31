test_that("upload block constructor", {

  blk <- new_upload_block()

  expect_s3_class(blk, "upload_block")
})
