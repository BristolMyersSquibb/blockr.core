test_that("head block constructor", {

  blk <- new_head_block()

  expect_s3_class(blk, "head_block")

  block_expr_test_server(
    blk,
    {
      expect_equal(nrw(), 6L)
      expect_equal(til(), FALSE)

      expect_equal(session$returned$state$n(), 6L)
      expect_equal(session$returned$state$direction(), "head")

      session$setInputs(n = 10L, tail = TRUE)

      session$flushReact()

      expect_equal(session$returned$state$n(), 10L)
      expect_equal(session$returned$state$direction(), "tail")
    },
    args = list(data = mtcars)
  )

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
})
