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

test_that("blocks split into singletons", {

  blks <- c(
    b = new_dataset_block(),
    a = new_subset_block(),
    c = new_subset_block()
  )

  res <- split(blks)

  expect_type(res, "list")
  expect_length(res, 3L)
  expect_true(all(lgl_ply(res, is_blocks)))
  expect_identical(int_ply(res, length), rep(1L, 3L))
  expect_identical(chr_ply(res, names), c("b", "a", "c"))

  expect_length(split(blocks()), 0L)

  many <- do.call(
    blocks,
    set_names(
      replicate(12L, new_subset_block(), simplify = FALSE),
      paste0("b", seq_len(12L))
    )
  )

  expect_identical(chr_ply(split(many), names), paste0("b", seq_len(12L)))

  grp <- split(blks, c("g1", "g1", "g2"))

  expect_named(grp, c("g1", "g2"))
  expect_s3_class(grp[["g1"]], "blocks")
  expect_length(grp[["g1"]], 2L)
  expect_length(grp[["g2"]], 1L)

  fac <- factor(c("g1", "g1", "g2"), levels = c("g1", "g2", "g3"))

  expect_named(split(blks, fac), c("g1", "g2", "g3"))
  expect_named(split(blks, fac, drop = TRUE), c("g1", "g2"))
})
