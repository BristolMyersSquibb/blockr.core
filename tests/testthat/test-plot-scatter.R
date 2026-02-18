test_that("scetter plot block constructor", {

  blk <- new_scatter_block()

  expect_s3_class(blk, "scatter_block")

  testServer(
    block_expr_server(blk),
    {
      expect_identical(x_col(), character())
      expect_identical(y_col(), character())

      expect_identical(cols(), colnames(iris))

      expect_identical(session$returned$state$x(), character())
      expect_identical(session$returned$state$y(), character())

      session$setInputs(xcol = "Sepal.Length", ycol = "Sepal.Width")

      expect_identical(x_col(), "Sepal.Length")
      expect_identical(y_col(), "Sepal.Width")

      expect_identical(session$returned$state$x(), "Sepal.Length")
      expect_identical(session$returned$state$y(), "Sepal.Width")
    },
    args = list(data = function() iris)
  )

  testServer(
    get_s3_method("block_server", blk),
    {
      expr <- session$makeScope("expr")
      expr$setInputs(xcol = "Sepal.Length", ycol = "Sepal.Width")
      session$flushReact()

      expect_s3_class(
        session$returned$result(),
        "evaluate_evaluation"
      )
    },
    args = list(x = blk, data = list(data = function() iris))
  )
})
