test_that("merge block constructor", {

  blk <- new_merge_block()

  expect_s3_class(blk, "merge_block")
})
