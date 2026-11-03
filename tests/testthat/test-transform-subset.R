test_that("subset block constructor", {

  blk <- new_subset_block()

  expect_s3_class(blk, "subset_block")
  expect_identical(block_state(blk), list(subset = "", select = ""))

  expect_error(new_subset_block(subset = 1), "character")
  expect_error(new_subset_block(subset = quote(Sepal.Width > 3)))
  expect_error(new_subset_block(select = "a +"), "unexpected")
  expect_error(new_subset_block(select = "a; b"), "not of length one")

  expect_identical(registry_id_from_block(blk), "blockr.core-subset_block")
})

test_that("a subset block puts its expressions in its block expression", {

  run <- function(state) {
    evaluate_block(subset_block, state, list(data = "a"),
                   list(a = datasets::iris))
  }

  expect_identical(
    run(list(subset = "Sepal.Width > 4", select = "Species"))[["expr"]],
    quote(subset(a, Sepal.Width > 4, select = Species))
  )

  expect_identical(
    run(list(subset = "Sepal.Width > 4", select = " "))[["expr"]],
    quote(subset(a, Sepal.Width > 4))
  )

  expect_identical(
    run(list(subset = "", select = "-Species"))[["expr"]],
    quote(subset(a, select = -Species))
  )

  res <- run(list(subset = "", select = ""))

  expect_identical(res[["expr"]], quote(subset(a)))
  expect_identical(res[["result"]], datasets::iris)

  res <- evaluate_block(subset_block, list(), list(data = "a"),
                        list(a = letters))

  expect_identical(res[["status"]], "error")
  expect_contains(res[["conditions"]][[1L]][["class"]],
                  "subset_block_inputs_invalid")

  res <- run(list(subset = "Sepal.Width >", select = ""))

  expect_identical(res[["status"]], "error")
  expect_identical(res[["conditions"]][[1L]][["phase"]], "expr")
})

test_that("the subset block's fixture", {
  expect_block_fixture(
    subset_block,
    states = list(
      list(subset = "", select = ""),
      list(subset = "Sepal.Width > 3", select = ""),
      list(subset = "", select = "Species"),
      list(subset = "Sepal.Width > 3", select = "c(Sepal.Length, Species)")
    ),
    inputs = list(data = datasets::iris)
  )
})
