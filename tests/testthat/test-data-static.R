test_that("static block constructor", {

  blk <- new_static_block("datasets::mtcars")

  expect_s3_class(blk, "static_block")
  expect_identical(block_state(blk), list(expression = "datasets::mtcars"))

  expect_error(new_static_block(datasets::mtcars), "as a string")
  expect_error(new_static_block(datasets::mtcars), class = "block_code_invalid")
  expect_error(new_static_block(quote(datasets::mtcars)),
               class = "block_code_invalid")
  expect_error(new_static_block("datasets::"), "unexpected")
  expect_error(new_static_block("a; b"), "not of length one")

  # The class gives its registry id, though core does not register the class
  # as it loads
  expect_false("blockr.core-static_block" %in% list_blocks())
  expect_identical(registry_id_from_block(blk), "blockr.core-static_block")
})

test_that("a static block evaluates its expression", {

  res <- evaluate_block(static_block,
                        list(expression = "utils::head(datasets::BOD)"))

  expect_identical(res[["status"]], "ok")
  expect_identical(res[["expr"]], quote(utils::head(datasets::BOD)))
  expect_identical(res[["result"]], utils::head(datasets::BOD))

  res <- evaluate_block(static_block, list(expression = "1 +"))

  expect_identical(res[["status"]], "error")
  expect_identical(res[["conditions"]][[1L]][["phase"]], "expr")

  res <- evaluate_block(static_block, list())

  expect_identical(res[["status"]], "invalid")
  expect_identical(res[["reason"]][["arg"]], "expression")
})

test_that("the static block's fixture", {
  expect_block_fixture(
    static_block,
    states = list(
      list(expression = "datasets::iris"),
      list(expression = "utils::head(datasets::mtcars)")
    )
  )
})
