test_that("glue block constructor", {

  blk <- new_glue_block()

  expect_s3_class(blk, "glue_block")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_null(session$returned$result())

      expr <- session$makeScope("expr")
      expr$setInputs(text = "The dataset has {nrow(data)} rows.")

      session$flushReact()

      expect_identical(
        session$returned$result(),
        paste0("The dataset has ", nrow(mtcars), " rows.")
      )
    },
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          data = mtcars
        )
      )
    )
  )
})
