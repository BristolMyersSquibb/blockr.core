test_that("static block constructor", {

  blk <- new_static_block("datasets::mtcars")

  expect_s3_class(blk, "static_block")
  expect_identical(block_state(blk), list(data = "datasets::mtcars"))

  expect_identical(
    block_state(new_static_block(quote(datasets::mtcars))),
    list(data = "datasets::mtcars")
  )

  expect_error(new_static_block(datasets::mtcars), "not an object of class")
  expect_error(new_static_block(datasets::mtcars), class = "block_code_invalid")
  expect_error(new_static_block(data = 1), class = "block_code_invalid")
  expect_error(new_static_block("datasets::"), "unexpected")
  expect_error(new_static_block("a; b"), "not of length one")

  expect_identical(registry_id_from_block(blk), character())
})

test_that("a static block evaluates its expression", {

  res <- evaluate_block(static_block, list(data = "utils::head(datasets::BOD)"))

  expect_identical(res[["status"]], "ok")
  expect_identical(res[["expr"]], quote(utils::head(datasets::BOD)))
  expect_identical(res[["result"]], utils::head(datasets::BOD))

  res <- evaluate_block(static_block, list(data = "letters"))

  expect_identical(res[["expr"]], quote((letters)))
  expect_identical(res[["result"]], letters)

  expect_identical(
    evaluate_block(static_block, list(data = "3L"))[["result"]],
    3L
  )

  res <- evaluate_block(static_block, list(data = "1 +"))

  expect_identical(res[["status"]], "error")
  expect_identical(res[["conditions"]][[1L]][["phase"]], "expr")

  res <- evaluate_block(static_block, list())

  expect_identical(res[["status"]], "invalid")
})

test_that("the static block's fixture", {
  expect_block_fixture(
    static_block,
    states = list(
      list(data = "datasets::iris"),
      list(data = "utils::head(datasets::mtcars)")
    )
  )
})
