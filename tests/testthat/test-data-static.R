test_that("static block constructor", {

  blk <- new_static_block(datasets::mtcars)

  expect_s3_class(blk, "static_block")
})
