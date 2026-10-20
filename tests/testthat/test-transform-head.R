test_that("head block constructor", {

  blk <- new_head_block()

  expect_s3_class(blk, "head_block")

  testServer(
    block_expr_server(blk),
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

test_that("the head block's tail switch is Shiny's checkbox", {

  ui <- expr_ui("x", new_head_block(direction = "tail"))

  box <- Filter(
    function(x) identical(htmltools::tagGetAttribute(x, "type"), "checkbox"),
    htmltools::tagQuery(ui)$find("input")$selectedTags()
  )

  expect_length(box, 1L)
  expect_identical(htmltools::tagGetAttribute(box[[1L]], "id"), "x-expr-tail")
  expect_true(htmltools::tagHasAttribute(box[[1L]], "checked"))

  # A head block on a board with no bslib of its own brings none
  deps <- chr_xtr(htmltools::findDependencies(ui), "name")

  expect_false(any(grepl("bslib", deps)))
})
