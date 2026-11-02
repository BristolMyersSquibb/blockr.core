test_that("fixed block constructor", {

  blk <- new_fixed_block(quote(utils::head(data)))

  expect_s3_class(blk, "fixed_block")

  expect_error(new_fixed_block())
  expect_error(new_fixed_block(123), class = "invalid_block_input")

  # Today's files hold the expression as the text that deparses it
  expect_identical(
    blockr_deser(
      v014_block(
        c("fixed_block", "transform_block"),
        list(expr = "utils::head(data)", block_name = "Fixed")
      )
    ),
    blk,
    ignore_function_env = TRUE
  )
})
