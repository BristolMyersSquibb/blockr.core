test_that("scatter block constructor", {

  blk <- new_scatter_block()

  expect_s3_class(blk, "scatter_block")
  expect_identical(block_state(blk), set_names(list(), character()))

  expect_identical(
    block_state(new_scatter_block(x = "Time", y = "demand")),
    list(x = "Time", y = "demand")
  )

  expect_identical(registry_id_from_block(blk), "blockr.core-scatter_block")
})

test_that("a scatter block captures the plot it draws", {

  res <- evaluate_block(scatter_block, list(x = "Time", y = "demand"),
                        list(data = "a"), list(a = datasets::BOD))

  expect_identical(res[["status"]], "ok")
  expect_identical(
    res[["expr"]],
    quote(
      plot(a[, "Time", drop = TRUE], a[, "demand", drop = TRUE],
           xlab = "Time", ylab = "demand")
    )
  )
  expect_s3_class(res[["result"]], "captured_plot")

  out <- as_plot_output(res[["result"]], info = list(width = 320, height = 240))

  expect_s3_class(out, "plot_output")
  expect_identical(out[["width"]], 320)

  res <- evaluate_block(scatter_block, list(x = "Time"), list(data = "a"),
                        list(a = datasets::BOD))

  expect_identical(res[["status"]], "invalid")
  expect_identical(res[["conditions"]][[1L]][["message"]],
                   "Choose a column for each axis.")

  res <- evaluate_block(scatter_block, list(x = "Time", y = ""),
                        list(data = "a"), list(a = datasets::BOD))

  expect_identical(res[["status"]], "invalid")
  expect_identical(res[["conditions"]][[1L]][["message"]],
                   "Choose a column for each axis.")

  res <- evaluate_block(scatter_block, list(x = "Time", y = "demand"),
                        list(data = "a"), list(a = letters))

  expect_identical(res[["status"]], "error")
  expect_identical(res[["conditions"]][[1L]][["phase"]], "inputs")
  expect_contains(res[["conditions"]][[1L]][["class"]],
                  "scatter_block_inputs_invalid")

  expect_identical(
    run_block_request(scatter_block, "columns", list(data = datasets::BOD)),
    c("Time", "demand")
  )
})

test_that("a scatter block fails where its data lacks a column", {

  for (axes in list(c("wt", "mpg"), c("wt", "Sepal.Width"),
                    c("Sepal.Length", "mpg"))) {

    res <- evaluate_block(scatter_block, list(x = axes[1L], y = axes[2L]),
                          list(data = "a"), list(a = datasets::iris))

    expect_identical(res[["status"]], "error")
    expect_identical(res[["conditions"]][[1L]][["phase"]], "block_eval")
    expect_identical(res[["conditions"]][[1L]][["message"]],
                     "undefined columns selected")
  }

  res <- evaluate_block(scatter_block, list(x = "wt", y = "mpg"),
                        list(data = "a"), list(a = datasets::mtcars))

  expect_identical(res[["status"]], "ok")

  skip_if_not_installed("tibble")

  res <- evaluate_block(scatter_block, list(x = "Time", y = "mpg"),
                        list(data = "a"),
                        list(a = tibble::as_tibble(datasets::BOD)))

  expect_identical(res[["status"]], "error")
})

test_that("the scatter block's fixture", {
  expect_block_fixture(
    scatter_block,
    states = list(
      list(x = "Sepal.Length", y = "Sepal.Width"),
      list(x = "Sepal.Length", y = "Petal.Length")
    ),
    inputs = list(data = datasets::iris),
    requests = list(list(name = "columns"))
  )
})
