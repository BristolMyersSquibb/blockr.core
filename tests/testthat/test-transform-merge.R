test_that("merge block constructor", {

  blk <- new_merge_block()

  expect_s3_class(blk, "merge_block")

  testServer(
    block_expr_server(blk),
    {
      expect_identical(sels(), character())
      expect_identical(allx(), FALSE)
      expect_identical(ally(), FALSE)

      expect_identical(session$returned$state$by(), character())
      expect_identical(session$returned$state$all_x(), FALSE)
      expect_identical(session$returned$state$all_y(), FALSE)

      session$flushReact()
      session$setInputs(by = "name", type = "all.y")

      expect_identical(sels(), "name")
      expect_identical(allx(), FALSE)
      expect_identical(ally(), TRUE)

      expect_identical(session$returned$state$by(), "name")
      expect_identical(session$returned$state$all_x(), FALSE)
      expect_identical(session$returned$state$all_y(), TRUE)
    },
    args = list(
      x = function() band_members,
      y = function() band_instruments,
      ui_ready = function() TRUE
    )
  )
})

test_that("merge block holds its control update until the UI is ready", {

  pushed <- new.env()
  pushed$n <- 0L

  record_push <- function(...) {

    args <- list(...)

    pushed$n <- pushed$n + 1L
    pushed$choices <- args$choices
    pushed$selected <- args$selected

    invisible()
  }

  local_mocked_bindings(
    updateSelectInput = record_push,
    .package = "blockr.core"
  )

  ready <- reactiveVal(FALSE)

  testServer(
    block_expr_server(new_merge_block(by = "name")),
    {
      session$flushReact()

      expect_identical(pushed$n, 0L)

      ready(TRUE)
      session$flushReact()

      expect_identical(pushed$n, 1L)
      expect_identical(pushed$choices, "name")
      expect_identical(pushed$selected, "name")
    },
    args = list(
      x = function() band_members,
      y = function() band_instruments,
      ui_ready = ready
    )
  )
})
