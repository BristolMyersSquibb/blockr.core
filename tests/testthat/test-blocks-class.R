test_that("blocks utils", {

  a <- new_dataset_block()
  b <- new_subset_block()

  ab <- c(a, b)

  expect_s3_class(ab, "blocks")
  expect_length(ab, 2L)
  expect_named(ab)

  names(ab) <- c("a", "b")

  expect_named(ab, c("a", "b"))

  names(ab)[2] <- "c"

  expect_named(ab, c("a", "c"))

  expect_error(
    names(ab)[1] <- "c",
    class = "blocks_names_unique_invalid"
  )

  expect_error(
    names(ab) <- c("a", "a"),
    class = "blocks_names_unique_invalid"
  )

  names(ab) <- NULL

  expect_named(ab, c("", ""))

  names(ab) <- c("a", "b")

  expect_snapshot(print(ab))

  c <- new_dataset_block()

  abc <- c(ab, c = c)

  expect_s3_class(abc, "blocks")
  expect_length(abc, 3L)
  expect_named(abc, c("a", "b", "c"))

  abc["c"] <- NULL

  expect_identical(abc, ab)

  expect_error(
    abc["c"] <- NULL
  )

  abc <- c(ab, c = c)

  expect_error(
    abc["c"] <- c,
    class = "blocks_assignment_name_invalid"
  )

  abc["c"] <- blocks(c = c)

  expect_identical(abc, c(ab, c = c))

  abc["c"] <- list(c = c)

  expect_identical(abc, c(ab, c = c))

  expect_error(
    abc["c"] <- list(d = c),
    class = "blocks_assignment_name_invalid"
  )

  abc[["c"]] <- c

  expect_identical(abc, c(ab, c = c))

  expect_s3_class(abc["c"], "blocks")
  expect_length(abc["c"], 1L)
  expect_named(abc["c"], "c")

  expect_s3_class(abc[["c"]], "block")

  expect_error(
    validate_blocks("a"),
    class = "blocks_class_invalid"
  )

  expect_error(
    validate_blocks(
      structure(
        1,
        class = c("blocks", "vctrs_vctr", "list")
      )
    ),
    class = "blocks_contains_invalid"
  )

  expect_error(
    validate_blocks(
      structure(
        list(a),
        class = c("blocks", "vctrs_vctr", "list")
      )
    ),
    class = "blocks_names_invalid"
  )

  expect_error(
    validate_blocks(
      structure(
        list(a = a, a = b),
        class = c("blocks", "vctrs_vctr", "list")
      )
    ),
    class = "blocks_names_invalid"
  )
})

test_that("a blocks container has a compact str_value()", {

  brd <- new_board(blocks = c(a = new_dataset_block(), b = new_subset_block()))

  expect_identical(
    str_value(board_blocks(brd)),
    paste(
      "<blocks[2]>",
      "  a: <dataset_block> dataset*, package",
      "  b: <subset_block> subset*, select*",
      sep = "\n"
    )
  )

  expect_identical(str_value(board_blocks(new_board())), "<blocks[0]>")

  out <- capture.output(res <- withVisible(str(board_blocks(brd))))

  expect_identical(
    out,
    c(
      " <blocks[2]>",
      "  a: <dataset_block> dataset*, package",
      "  b: <subset_block> subset*, select*"
    )
  )
  expect_false(res$visible)
  expect_identical(res$value, board_blocks(brd))
})

test_that("str_value() of a blocks container is extensible via NextMethod()", {

  registerS3method(
    "str_value", "test_sub_blocks",
    function(x, ...) paste0(NextMethod(), "\n[extra]"),
    envir = asNamespace("blockr.core")
  )

  blk <- board_blocks(new_board(blocks = c(a = new_dataset_block())))
  class(blk) <- c("test_sub_blocks", class(blk))

  expect_identical(
    str_value(blk),
    "<test_sub_blocks[1]>\n  a: <dataset_block> dataset*, package\n[extra]"
  )
})
