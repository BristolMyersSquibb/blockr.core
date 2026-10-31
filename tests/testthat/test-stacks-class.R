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

  stk <- stacks(a = x, b = y)

  expect_named(stk, c("a", "b"))

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
})

test_that("stack IDs are neither empty nor made of digits alone", {

  x <- new_stack(letters[1:2], "stack a")

  expect_error(
    stacks(a = x, `2` = new_stack("c")),
    "\"2\"",
    class = "stacks_names_invalid"
  )

  expect_error(
    validate_stacks(
      structure(
        list(x),
        names = "",
        class = c("stacks", "vctrs_vctr", "list")
      )
    ),
    class = "stacks_names_invalid"
  )
})

test_that("a stacks container has a compact str_value()", {

  stk <- stacks(s1 = new_stack(c("a", "b"), name = "my stack"))

  expect_identical(
    str_value(stk),
    "<stacks[1]>\n  s1: <stack> \"my stack\": a, b"
  )

  expect_identical(str_value(stacks()), "<stacks[0]>")

  expect_identical(capture.output(str(stk))[1L], " <stacks[1]>")
})
