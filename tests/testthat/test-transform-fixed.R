test_that("fixed block constructor", {

  blk <- new_fixed_block("utils::head(data)")

  expect_s3_class(blk, "fixed_block")
  expect_identical(block_state(blk), list(expr = "utils::head(data)"))

  expect_identical(
    block_state(new_fixed_block(quote(utils::head(data)))),
    list(expr = "utils::head(data)")
  )

  expect_identical(
    block_state(new_fixed_block()),
    set_names(list(), character())
  )

  expect_error(new_fixed_block(123), class = "block_code_invalid")
  expect_error(new_fixed_block("utils::head(data"), "unexpected")

  expect_identical(registry_id_from_block(blk), character())
})

test_that("a fixed block calls a function of its input", {

  res <- evaluate_block(
    fixed_block,
    list(expr = "x <- 2L\nutils::head(data, x)"),
    list(data = "a"),
    list(a = datasets::BOD)
  )

  expect_identical(res[["status"]], "ok")
  expect_identical(res[["result"]], utils::head(datasets::BOD, 2L))

  expect_identical(
    res[["expr"]],
    quote(
      (function(data) {
        x <- 2L
        utils::head(data, x)
      })(data = a)
    )
  )
})

test_that("the fixed block's fixture", {
  expect_block_fixture(
    fixed_block,
    states = list(list(expr = "utils::head(data, 3)")),
    inputs = list(data = datasets::iris)
  )
})
