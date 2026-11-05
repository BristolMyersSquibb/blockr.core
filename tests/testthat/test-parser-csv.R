test_that("csv block constructor", {

  blk <- new_csv_block()

  expect_s3_class(blk, "csv_block")
  expect_identical(block_state(blk), list(sep = ",", quote = "\""))
  expect_identical(registry_id_from_block(blk), "blockr.core-csv_block")
})

test_that("a csv block reads the file its input names", {

  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(datasets::BOD, csv, row.names = FALSE)

  res <- evaluate_block(csv_block, list(sep = ",", quote = "\""),
                        list(file = "a"), list(a = csv))

  expect_identical(res[["status"]], "ok")
  expect_equal(
    res[["result"]],
    data.frame(Time = datasets::BOD$Time, demand = datasets::BOD$demand)
  )

  for (input in list(tempfile(), 1L)) {

    res <- evaluate_block(csv_block, list(), list(file = "a"),
                          list(a = input))

    expect_identical(res[["status"]], "error")
    expect_identical(res[["conditions"]][[1L]][["phase"]], "inputs")
    expect_contains(res[["conditions"]][[1L]][["class"]],
                    "csv_block_inputs_invalid")
  }
})

test_that("the csv block's fixture", {

  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(datasets::BOD, csv, row.names = FALSE)

  expect_block_fixture(
    csv_block,
    states = list(
      list(sep = ",", quote = "\""),
      list(sep = ";", quote = ""),
      list(sep = ";", quote = "\"")
    ),
    inputs = list(file = csv)
  )
})
