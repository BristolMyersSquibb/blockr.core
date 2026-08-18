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

      expect_identical(
        session$returned$expr(),
        quote(
          plot(.(data)[["Sepal.Length"]], .(data)[["Sepal.Width"]],
               xlab = "Sepal.Length", ylab = "Sepal.Width")
        )
      )
    },
    args = list(data = function() iris, ui_ready = function() TRUE)
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

test_that("scatter block holds its control updates until the UI is ready", {

  pushed <- new.env()
  pushed$ids <- character()

  record_push <- function(...) {

    args <- list(...)

    pushed$ids <- c(pushed$ids, args$inputId)
    pushed[[args$inputId]] <- args$choices

    invisible()
  }

  local_mocked_bindings(
    updateSelectInput = record_push,
    .package = "blockr.core"
  )

  ready <- reactiveVal(FALSE)

  testServer(
    block_expr_server(new_scatter_block("Sepal.Length", "Sepal.Width")),
    {
      session$flushReact()

      expect_identical(pushed$ids, character())

      ready(TRUE)
      session$flushReact()

      expect_identical(pushed$ids, c("xcol", "ycol"))
      expect_identical(pushed$xcol, colnames(iris))
      expect_identical(pushed$ycol, colnames(iris))
    },
    args = list(data = function() iris, ui_ready = ready)
  )
})
