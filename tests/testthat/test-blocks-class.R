test_that("blocks utils", {

  a <- new_dataset_block()
  b <- new_subset_block()

  ab <- c(a, b)

  expect_s3_class(ab, "blocks")
  expect_length(ab, 2L)
  expect_named(ab)

  ab <- c(a = a, b = b)

  expect_named(ab, c("a", "b"))

  expect_snapshot(print(ab))

  c <- new_dataset_block()

  abc <- c(ab, c = c)

  expect_s3_class(abc, "blocks")
  expect_length(abc, 3L)
  expect_named(abc, c("a", "b", "c"))

  expect_error(
    c(abc, c = c),
    class = "blocks_names_invalid"
  )

  expect_identical(abc[c("a", "b")], ab)

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
        list(a = a, a = b),
        class = c("blocks", "vctrs_vctr", "list")
      )
    ),
    class = "blocks_names_invalid"
  )
})

test_that("a block may lack an ID, but one given is not made of digits", {

  a <- new_dataset_block()

  expect_named(blocks(a, new_subset_block()), c("", ""))
  expect_named(c(a = a, new_subset_block()), c("a", ""))
  expect_named(blocks(a1 = a, `1a` = new_subset_block()), c("a1", "1a"))

  expect_s3_class(
    validate_blocks(
      structure(list(a), class = c("blocks", "vctrs_vctr", "list"))
    ),
    "blocks"
  )

  expect_error(
    c(a = a, `1` = new_subset_block()),
    "\"1\"",
    class = "blocks_names_invalid"
  )

  expect_error(
    blocks(`01` = a),
    "\"01\"",
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
