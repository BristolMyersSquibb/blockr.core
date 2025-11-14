test_that("serialization", {

  orig <- new_dataset_block("iris", "datasets")
  json <- blockr_ser(orig)

  expect_equal(orig, blockr_deser(json), ignore_function_env = TRUE)

  json <- blockr_ser(
    new_dataset_block(),
    list(dataset = "iris", package = "datasets")
  )

  expect_equal(
    new_dataset_block("iris", "datasets"),
    blockr_deser(json),
    ignore_function_env = TRUE
  )

  orig <- c(
    a = new_dataset_block(),
    b = new_subset_block()
  )

  expect_equal(
    orig,
    blockr_deser(blockr_ser(orig)),
    ignore_function_env = TRUE
  )

  orig <- blocks(
    a = new_dataset_block("iris", "datasets")
  )

  json <- blockr_ser(
    blocks(a = new_dataset_block()),
    list(
      a = list(dataset = "iris", package = "datasets")
    )
  )

  expect_equal(orig, blockr_deser(json), ignore_function_env = TRUE)

  orig <- links(from = "a", to = "b")

  expect_equal(orig, blockr_deser(blockr_ser(orig)))

  orig <- new_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_subset_block()
    ),
    links = links(from = "a", to = "b")
  )

  expect_equal(
    orig,
    blockr_deser(blockr_ser(orig)),
    ignore_function_env = TRUE
  )
})
