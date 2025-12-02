test_that("fixed block constructor", {

  blk <- new_fixed_block(quote(utils::head(data)))

  expect_s3_class(blk, "fixed_block")

  expect_error(new_fixed_block())
  expect_error(new_fixed_block(123), class = "invalid_block_input")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_identical(
        session$returned$result(),
        head(datasets::mtcars)
      )
    },
    args = list(
      x = blk,
      data = list(data = function() datasets::mtcars)
    )
  )

  expect_identical(
    blk,
    blockr_deser(blockr_ser(blk)),
    ignore_function_env = TRUE
  )
})
