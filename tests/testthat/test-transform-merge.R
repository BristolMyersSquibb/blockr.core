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
      y = function() band_instruments
    )
  )
})

test_that("merge block ui carries by", {

  selected_by <- function(blk) {

    opts <- htmltools::tagQuery(expr_ui("blk", blk))$find(
      "#blk-expr-by"
    )$selectedTags()[[1L]]$children[[1L]]

    regmatches(
      opts,
      gregexpr("(?<=value=\")[^\"]+(?=\"[^>]*selected)", opts, perl = TRUE)
    )[[1L]]
  }

  expect_identical(selected_by(new_merge_block()), character())

  expect_identical(
    selected_by(new_merge_block(by = c("name", "plays"))),
    c("name", "plays")
  )
})
