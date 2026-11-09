test_that("stack class", {

  x <- new_stack(letters[1:5])

  expect_s3_class(x, "stack")
  expect_true(is_stack(x))
  expect_length(x, 5)

  expect_setequal(as_stack(x), x)
  expect_setequal(as_stack(letters[1:5]), x)
  expect_setequal(as_stack(list(blocks = letters[1:5])), x)

  expect_setequal(as_stack(as.character(x)), x)
  expect_setequal(as_stack(as.list(x)), x)

  expect_identical(duplicated(x), rep(FALSE, length(x)))
  expect_identical(anyDuplicated(x), 0L)

  expect_error(
    validate_stack(1),
    class = "stack_class_invalid"
  )

  expect_error(
    validate_stack(structure(1, name = 2, class = "stack")),
    class = "stack_name_invalid"
  )

  expect_error(new_stack("a", name = 1), class = "stack_name_invalid")
  expect_error(new_stack("a", name = function() 1),
               class = "stack_name_invalid")

  expect_error(
    validate_stack(structure(1, name = "a", class = "stack")),
    class = "stack_type_invalid"
  )

  expect_error(
    validate_stack(structure("", name = "a", class = "stack")),
    class = "stack_blocks_invalid"
  )

  expect_error(
    validate_stack(structure(NA_character_, name = "a", class = "stack")),
    class = "stack_blocks_invalid"
  )

  expect_error(
    validate_stack(structure(c("a", "a"), name = "a", class = "stack")),
    class = "stack_block_duplicates"
  )

  x <- new_stack(letters[1:5], "my stack")

  expect_snapshot(print(x))

  y <- new_stack("a")

  expect_length(setdiff(x, y), 4L)
  expect_s3_class(setdiff(x, y), "stack")
  expect_identical(setdiff(x, y), setdiff(x, "a"))

  expect_length(setdiff(letters[1:5], y), 4L)
  expect_length(union(x, y), length(x))
  expect_length(intersect(x, y), length(y))

  expect_true(setequal(x, x))
  expect_false(setequal(y, x))

  expect_true(is.element(y, x))
  expect_false(is.element("y", x))

  expect_type(x[1], "character")
  expect_type(x[[1]], "character")

  expect_error(x["a"])
  expect_error(x[[1:2]])

  expect_error(
    x[1] <- "c",
    class = "stack_subassignment_invalid"
  )

  expect_error(
    x[[1]] <- "c",
    class = "stack_subassignment_invalid"
  )

  stks <- c(x, letters[6:8])

  expect_s3_class(stks, "stacks")
  expect_true(is_stacks(stks))
  expect_length(stks, 2)
})

test_that("a stack may have a color, which is a hex color", {

  x <- new_stack(c("a", "b"), "Input", color = "#66c2a5")

  expect_identical(stack_color(x), "#66c2a5")
  expect_identical(stack_color(new_stack("a", color = "#FC8D62")), "#FC8D62")
  expect_null(stack_color(new_stack("a")))
  expect_null(stack_color(new_stack("a", color = NULL)))

  expect_identical(
    format(x),
    c(
      "<stack[2]>",
      "Name: \"Input\"",
      "Color: \"#66c2a5\"",
      "Blocks: \"a\" and \"b\""
    )
  )

  expect_identical(as_stack(as.list(x)), x)
  expect_identical(stack_color(setdiff(x, "b")), "#66c2a5")
  expect_identical(stack_color(stacks(s = x)[["s"]]), "#66c2a5")

  for (color in list("teal", "#fc8", "#66c2a", "66c2a5", "#66c2a5ff",
                     NA_character_, c("#ffffff", "#000000"), 1)) {
    expect_error(new_stack("a", color = color), class = "stack_color_invalid")
  }
})

test_that("a stack takes core's properties and nothing else", {

  expect_error(new_stack("a", "Input", NULL, "extra"), "unused argument")
  expect_error(new_stack("a", collapsed = TRUE), "unused argument")
  expect_error(new_stack("a", class = "dock_stack"), "unused argument")
})

test_that("a stack's color has the pattern of the schema of a JSON board", {

  stack <- json_board_schema()[["definitions"]][["Stack"]]

  expect_identical(
    stack[["properties"]][["color"]][["pattern"]],
    hex_color_pattern()
  )
})

test_that("stacks have a compact str_value()", {

  stk <- new_stack(c("a", "b"), name = "my stack")

  expect_identical(str_value(stk), "<stack> \"my stack\": a, b")
  expect_identical(str_value(new_stack(name = "empty")), "<stack> \"empty\"")

  out <- capture.output(res <- withVisible(str(stk)))

  expect_identical(out, " <stack> \"my stack\": a, b")
  expect_false(res$visible)
  expect_identical(res$value, stk)
})

test_that("str_value() is extensible via NextMethod()", {

  registerS3method(
    "str_value", "test_substack",
    function(x, ...) paste0(NextMethod(), " [extra]"),
    envir = asNamespace("blockr.core")
  )

  sub <- new_stack(c("a", "b"), name = "S")
  class(sub) <- c("test_substack", class(sub))

  expect_identical(str_value(sub), "<test_substack> \"S\": a, b [extra]")

  expect_identical(
    capture.output(str(sub)),
    " <test_substack> \"S\": a, b [extra]"
  )
})
