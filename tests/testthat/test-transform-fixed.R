test_that("fixed block constructor", {

  blk <- new_fixed_block(quote(utils::head(data)))

  expect_s3_class(blk, "fixed_block")

  expect_error(new_fixed_block())
  expect_error(new_fixed_block(123), class = "invalid_block_input")

  expect_identical(
    blk,
    blockr_deser(blockr_ser(blk)),
    ignore_function_env = TRUE
  )
})
