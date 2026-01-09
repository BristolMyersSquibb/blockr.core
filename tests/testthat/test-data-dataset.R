test_that("dataset block constructor", {

  blk <- new_dataset_block("mtcars")

  expect_s3_class(blk, "dataset_block")

  block_expr_test_server(
    blk,
    {
      expect_equal(dataset(), "mtcars")
      session$setInputs(dataset = "iris")
      expect_equal(dataset(), "iris")
    }
  )

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_identical(
        session$returned$result(),
        datasets::mtcars
      )
    },
    args = list(x = blk)
  )
})
