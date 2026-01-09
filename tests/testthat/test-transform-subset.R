test_that("subset block constructor", {

  blk <- new_subset_block()

  expect_s3_class(blk, "subset_block")

  block_expr_test_server(
    blk,
    {
      expect_equal(sub(), "")
      expect_equal(sel(), "")

      expect_equal(session$returned$state$subset(), "")
      expect_equal(session$returned$state$select(), "")
    },
    args = list(data = mtcars)
  )

  testServer(
    get_s3_method("block_server", blk),
    {
      expr <- session$makeScope("expr")

      expr$setInputs(
        subset = "",
        select = "c(Sepal.Width, Sepal.Length)",
        eval = 1
      )

      session$flushReact()

      res <- session$returned$result()

      expect_named(res, c("Sepal.Width", "Sepal.Length"))
      expect_identical(nrow(res), nrow(iris))

      expr$setInputs(
        subset = "Sepal.Width > 3",
        select = "",
        eval = 2
      )

      session$flushReact()

      res <- session$returned$result()

      expect_named(res, names(iris))
      expect_identical(nrow(res), nrow(iris[iris$Sepal.Width > 3, ]))

      expr$setInputs(
        subset = "Sepal.Width > 3",
        select = "c(Sepal.Width, Sepal.Length)",
        eval = 3
      )

      session$flushReact()

      res <- session$returned$result()

      expect_named(res, c("Sepal.Width", "Sepal.Length"))
      expect_identical(nrow(res), nrow(iris[iris$Sepal.Width > 3, ]))

      expr$setInputs(
        subset = "",
        select = "",
        eval = 4
      )

      session$flushReact()

      res <- session$returned$result()

      expect_named(res, names(iris))
      expect_identical(nrow(res), nrow(iris))
    },
    args = list(x = blk, data = list(data = function() iris))
  )
})
