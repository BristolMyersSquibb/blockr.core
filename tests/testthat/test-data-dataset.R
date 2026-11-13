test_that("dataset block constructor", {

  blk <- new_dataset_block()

  expect_s3_class(blk, "dataset_block")
  expect_identical(block_state(blk), list(package = "datasets"))

  expect_identical(
    block_state(new_dataset_block("iris")),
    list(dataset = "iris", package = "datasets")
  )

  expect_identical(registry_id_from_block(blk), "blockr.core-dataset_block")
})

test_that("a dataset block names its dataset in its package", {

  res <- evaluate_block(dataset_block, list(package = "datasets"))

  expect_identical(res[["status"]], "invalid")
  expect_identical(
    res[["reason"]],
    list(message = "Choose a dataset.", arg = "dataset")
  )

  res <- evaluate_block(dataset_block,
                        list(dataset = "BOD", package = "datasets"))

  expect_identical(res[["status"]], "ok")
  expect_identical(res[["expr"]], quote(datasets::BOD))
  expect_identical(res[["result"]], datasets::BOD)

  res <- evaluate_block(dataset_block,
                        list(dataset = "", package = "datasets"))

  expect_identical(res[["status"]], "invalid")
  expect_identical(
    res[["reason"]],
    list(message = "Choose a dataset.", arg = "dataset")
  )

  res <- evaluate_block(dataset_block,
                        list(dataset = "nope", package = "datasets"))

  expect_identical(res[["status"]], "error")
})

test_that("a dataset block lists the data frames a package exports", {

  res <- run_block_request(dataset_block, "datasets",
                           args = list(package = "datasets"))

  expect_type(res, "character")
  expect_contains(res, c("iris", "BOD", "mtcars"))
  expect_false("Titanic" %in% res)

  expect_false(dataset_block[["requests"]][["datasets"]][["volatile"]])
})

# What the datasets package holds depends on the version of R,
# so the fixture records the answer of the request `datasets`
# for a package that ships BOD, iris, mtcars and the table Titanic,
# which is no data frame.
test_that("the dataset block's fixture", {

  local_mocked_bindings(
    package_data = function(package) c("BOD", "iris", "mtcars", "Titanic")
  )

  expect_block_fixture(
    dataset_block,
    states = list(
      list(dataset = "iris", package = "datasets"),
      list(dataset = "BOD", package = "datasets")
    ),
    requests = list(list(name = "datasets", args = list(package = "datasets")))
  )
})
