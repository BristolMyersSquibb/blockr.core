test_that("stacks class", {

  x <- new_stack(letters[1:5], "stack a")
  y <- new_stack(letters[6:8], "stack b")

  stk <- stacks(x, y)

  expect_s3_class(stk, "stacks")
  expect_true(is_stacks(stk))
  expect_length(stk, 2)
  expect_named(stk)

  expect_error(
    validate_stacks(1),
    class = "stacks_class_invalid"
  )

  expect_error(
    validate_stacks(
      structure(1, class = c("stacks", "vctrs_vctr", "list"))
    ),
    class = "stacks_type_invalid"
  )

  expect_error(
    validate_stacks(
      structure(list(1), class = c("stacks", "vctrs_vctr", "list"))
    ),
    class = "stacks_contains_invalid"
  )

  expect_error(
    validate_stacks(
      structure(list(x), class = c("stacks", "vctrs_vctr", "list"))
    ),
    class = "stacks_names_invalid"
  )

  expect_error(
    validate_stacks(
      structure(list(a = x, a = y), class = c("stacks", "vctrs_vctr", "list"))
    ),
    class = "stacks_names_invalid"
  )

  expect_error(
    validate_stacks(
      structure(list(a = x, b = x), class = c("stacks", "vctrs_vctr", "list"))
    ),
    class = "stacks_blocks_invalid"
  )

  names(stk) <- c("a", "b")

  expect_named(stk, c("a", "b"))

  expect_error(
    names(stk) <- c("a", "a"),
    class = "stacks_names_unique_invalid"
  )

  expect_error(
    names(stk)[1] <- "b",
    class = "stacks_names_unique_invalid"
  )

  expect_snapshot(print(stk))

  expect_identical(as_stacks(stk), stk)
  expect_identical(as_stacks(as.list(stk)), stk)

  expect_s3_class(as_stacks(x), "stacks")
  expect_true(is_stacks(as_stacks(x)))
  expect_length(as_stacks(x), 1)

  stks <- c(a = x, b = list(y), c = letters[9:10])

  expect_s3_class(as_stacks(stks), "stacks")
  expect_true(is_stacks(as_stacks(stks)))
  expect_length(as_stacks(stks), 3)
  expect_named(stks, c("a", "b", "c"))

  stkss <- c(stks, d = letters[11:15])

  expect_s3_class(as_stacks(stkss), "stacks")
  expect_true(is_stacks(as_stacks(stkss)))
  expect_length(as_stacks(stkss), 4)
  expect_named(stkss, c("a", "b", "c", "d"))

  orig <- stacks(a = x, b = y)
  modi <- orig

  modi["b"] <- stacks(b = y)

  expect_identical(orig, modi)

  modi["b"] <- list(b = y)

  expect_identical(orig, modi)

  expect_error(
    modi["b"] <- y,
    class = "stacks_assignment_name_invalid"
  )

  modi[["b"]] <- y

  expect_identical(orig, modi)
})

test_that("stacks split into singletons", {

  stk <- stacks(
    b = new_stack(letters[1:2], "stack b"),
    a = new_stack(letters[3:4], "stack a")
  )

  res <- split(stk)

  expect_type(res, "list")
  expect_length(res, 2L)
  expect_true(all(lgl_ply(res, is_stacks)))
  expect_identical(int_ply(res, length), c(1L, 1L))
  expect_identical(chr_ply(res, names), c("b", "a"))

  expect_length(split(stacks()), 0L)

  grp <- split(stk, c("g", "g"))

  expect_named(grp, "g")
  expect_s3_class(grp[["g"]], "stacks")
  expect_length(grp[["g"]], 2L)
})
