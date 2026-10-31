test_that("scetter plot block constructor", {

  blk <- new_scatter_block()

  expect_s3_class(blk, "scatter_block")
})
