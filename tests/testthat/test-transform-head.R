test_that("head block constructor", {

  blk <- new_head_block()

  expect_s3_class(blk, "head_block")
})
