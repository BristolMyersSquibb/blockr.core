test_that("head block constructor", {

  blk <- new_head_block()

  expect_s3_class(blk, "head_block")
  expect_identical(block_state(blk), list(n = 6L, direction = "head"))
  expect_identical(block_name(blk), "Head")

  expect_identical(
    block_state(new_head_block(n = 10L, direction = "tail")),
    list(n = 10L, direction = "tail")
  )

  expect_error(new_head_block(direction = "middle"), "should be one of")
  expect_error(new_head_block(m = 10L), "unused argument")

  expect_identical(registry_id_from_block(blk), "blockr.core-head_block")
})

test_that("head block expression and request", {

  res <- evaluate_block(
    head_block,
    list(n = 2L, direction = "tail"),
    list(data = "a"),
    list(a = datasets::BOD)
  )

  expect_identical(res[["status"]], "ok")
  expect_identical(res[["expr"]], quote(utils::tail(a, n = 2L)))
  expect_identical(res[["result"]], utils::tail(datasets::BOD, n = 2L))

  res <- evaluate_block(head_block, list(n = 2L), list(data = "a"),
                        list(a = letters))

  expect_identical(res[["status"]], "error")
  expect_identical(res[["conditions"]][[1L]][["phase"]], "inputs")
  expect_contains(res[["conditions"]][[1L]][["class"]],
                  "head_block_inputs_invalid")

  expect_identical(
    run_block_request(head_block, "n_max", list(data = datasets::BOD)),
    6L
  )
})

test_that("the head block's fixture", {
  expect_block_fixture(
    head_block,
    states = list(
      list(n = 6L, direction = "head"),
      list(n = 10L, direction = "tail"),
      list(n = 10L, direction = "head"),
      list(n = 20L, direction = "tail")
    ),
    inputs = list(data = datasets::iris),
    requests = list(list(name = "n_max"))
  )
})
