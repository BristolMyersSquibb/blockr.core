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
    args = list(data = function() mtcars, ui_ready = function() TRUE)
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

test_that("head block holds its row bound until the UI is ready", {

  pushed <- new.env()
  pushed$n <- 0L

  record_push <- function(...) {

    args <- list(...)

    pushed$n <- pushed$n + 1L
    pushed$max <- args$max

    invisible()
  }

  local_mocked_bindings(
    updateNumericInput = record_push,
    .package = "blockr.core"
  )

  ready <- reactiveVal(FALSE)

  testServer(
    block_expr_server(new_head_block()),
    {
      session$flushReact()

      expect_identical(pushed$n, 0L)

      ready(TRUE)
      session$flushReact()

      expect_identical(pushed$n, 1L)
      expect_identical(pushed$max, nrow(datasets::mtcars))
    },
    args = list(data = function() datasets::mtcars, ui_ready = ready)
  )
})
