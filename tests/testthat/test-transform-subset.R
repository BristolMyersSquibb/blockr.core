test_that("subset block constructor", {

  blk <- new_subset_block()

  expect_s3_class(blk, "subset_block")
})
