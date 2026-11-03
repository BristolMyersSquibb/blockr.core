test_that("rbind block constructor", {

  blk <- new_rbind_block()

  expect_s3_class(blk, "rbind_block")
  expect_identical(block_state(blk), set_names(list(), character()))
  expect_true(is.na(block_arity(blk)))

  expect_identical(registry_id_from_block(blk), "blockr.core-rbind_block")
})

test_that("an rbind block binds its inputs in link order", {

  res <- evaluate_block(
    rbind_block,
    list(),
    list(... = list("a", b2 = "b")),
    list(a = datasets::BOD, b = datasets::BOD)
  )

  expect_identical(res[["status"]], "ok")
  expect_identical(res[["expr"]], quote(rbind(a, b2 = b)))
  expect_identical(nrow(res[["result"]]), 12L)
})

test_that("the rbind block's fixture", {
  expect_block_fixture(
    rbind_block,
    states = list(list()),
    inputs = list(... = list(datasets::BOD, b2 = datasets::BOD))
  )
})
