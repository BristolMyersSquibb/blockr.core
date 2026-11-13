test_that("merge block constructor", {

  blk <- new_merge_block()

  expect_s3_class(blk, "merge_block")
  expect_identical(block_state(blk), list(all_x = FALSE, all_y = FALSE))

  expect_identical(
    block_state(new_merge_block(by = "Time", all_x = TRUE)),
    list(by = "Time", all_x = TRUE, all_y = FALSE)
  )

  expect_identical(registry_id_from_block(blk), "blockr.core-merge_block")
})

test_that("a merge block is invalid until it joins by a column", {

  inputs <- list(x = "a", y = "b")
  results <- list(a = datasets::BOD, b = datasets::BOD)

  res <- evaluate_block(merge_block, list(), inputs, results)

  expect_identical(res[["status"]], "invalid")
  expect_identical(
    res[["reason"]],
    list(message = "Choose at least one column to join by.", arg = "by")
  )

  res <- evaluate_block(merge_block, list(by = list()), inputs, results)

  expect_identical(res[["status"]], "invalid")
  expect_identical(
    res[["reason"]],
    list(message = "Choose at least one column to join by.", arg = "by")
  )

  res <- evaluate_block(merge_block, list(by = "Time", all_x = TRUE), inputs,
                        results)

  expect_identical(res[["status"]], "ok")
  expect_identical(
    res[["expr"]],
    quote(merge(a, b, by = "Time", all.x = TRUE, all.y = FALSE))
  )

  res <- evaluate_block(merge_block, list(by = "Time"), inputs,
                        list(a = datasets::BOD, b = 1:3))

  expect_identical(res[["status"]], "error")
  expect_identical(res[["conditions"]][[1L]][["phase"]], "inputs")
  expect_contains(res[["conditions"]][[1L]][["class"]],
                  "merge_block_inputs_invalid")
})

test_that("a merge block asks for the columns its inputs share", {

  expect_identical(
    run_block_request(
      merge_block,
      "shared",
      list(x = datasets::BOD, y = data.frame(Time = 1:2, rate = 3:4))
    ),
    "Time"
  )
})

test_that("the merge block's fixture", {
  expect_block_fixture(
    merge_block,
    states = list(
      list(by = I("Time"), all_x = FALSE, all_y = FALSE),
      list(by = I(c("Time", "demand")), all_x = TRUE, all_y = TRUE),
      list(by = I(c("Time", "demand")), all_x = FALSE, all_y = FALSE),
      list(by = I(c("demand", "Time")), all_x = FALSE, all_y = FALSE),
      list(by = I("Time"), all_x = TRUE, all_y = FALSE)
    ),
    inputs = list(x = datasets::BOD, y = datasets::BOD),
    requests = list(list(name = "shared"))
  )
})
