test_that("merge block constructor", {

  blk <- new_merge_block()

  expect_s3_class(blk, "merge_block")

  block_expr_test_server(
    blk,
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
