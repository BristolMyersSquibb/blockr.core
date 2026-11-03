# A constructor that no longer returns a block: the one class change
# deserialization refuses
not_a_block <- blockr_ser(new_blockr_ctor(function(...) list()))

test_that("serialization", {

  lnks <- links(from = "a", to = "b")

  expect_identical(lnks, blockr_deser(blockr_ser(lnks)))

  stks <- stacks(ab = c("a", "b"))

  expect_identical(stks, blockr_deser(blockr_ser(stks)))

  new_test_opt <- function(val1 = 1, val2 = "2", ...) {
    new_board_option("test_opt", list(val1 = val1, val2 = val2))
  }

  opt <- new_test_opt()

  expect_error(
    blockr_ser(opt, option = 3),
    class = "option_value_arg_number_mismatch"
  )

  expect_silent(blockr_ser(opt, option = list(val1 = 3, val2 = "4")))
  expect_output(blockr_ser(opt, option = list(3, "4")))

  expect_error(
    blockr_ser(opt, option = list(val1 = 3, val2 = "4", val3 = "boom")),
    class = "option_value_arg_name_mismatch"
  )
})

test_that("deserialization checks the kind of object, not its classes", {

  stk <- new_stack(c("a", "b"), name = "Inputs")

  ser <- blockr_ser(stk)
  ser$object <- c("foo_stack", ser$object)

  expect_identical(blockr_deser(ser), stk)

  board <- new_board(name = "Inputs")

  ser <- blockr_ser(board)
  ser$object <- c("foo_board", ser$object)

  expect_identical(blockr_deser(ser), board, ignore_function_env = TRUE)

  ser <- blockr_ser(stk)
  ser$constructor <- not_a_block

  expect_error(blockr_deser(ser), class = "block_deser_class_error")
})

test_that("a board keeps its ID and name through serialization", {

  board <- new_board(id = "sales_review", name = "Q3 numbers")

  ser <- blockr_ser(board)

  expect_identical(ser[["id"]], "sales_review")
  expect_identical(ser[["name"]], "Q3 numbers")

  res <- blockr_deser(ser)

  expect_identical(board_id(res), "sales_review")
  expect_identical(board_name(res), "Q3 numbers")
})

test_that("a board saved by blockr.core 0.1.4 keeps its name and ID", {

  board <- new_board(id = "sales_review")

  # As blockr.core 0.1.4 wrote it: the name as the value of a `board_name`
  # board option, and the ID only where the app passed one
  ser <- blockr_ser(board)
  ser[["name"]] <- NULL

  name_opt <- list(
    object = c("board_name_option", "board_option"),
    payload = list(
      value = "Q3 numbers",
      category = "Board options",
      label = "Board name"
    ),
    constructor = list(
      object = "blockr_ctor",
      constructor = "new_board_name_option",
      package = "blockr.core",
      version = "0.1.4"
    )
  )

  ser$payload$options$payload <- c(list(name_opt), ser$payload$options$payload)

  res <- blockr_deser(ser)

  expect_identical(board_id(res), "sales_review")
  expect_identical(board_name(res), "Q3 numbers")
  expect_identical(board_option_ids(res), board_option_ids(board))

  ser$payload$options$payload[[1L]]$payload$value <- NULL
  ser["id"] <- list(NULL)

  res <- blockr_deser(ser)

  expect_null(board_id(res))
  expect_null(board_name(res))
})

test_that("objects without an ID round-trip without one", {

  ser <- blockr_ser(new_board())

  expect_true(all(c("id", "name") %in% names(ser)))
  expect_null(ser[["id"]])
  expect_null(ser[["name"]])

  stks <- stacks(new_stack("a"), b = new_stack("b"))

  expect_identical(blockr_deser(blockr_ser(stks)), stks)

  lnks <- links(new_link("a", "b"), bc = new_link("b", "c"))

  expect_identical(blockr_deser(blockr_ser(lnks)), lnks)
})

test_that("a board option keeps the label it was given", {

  opt <- new_n_rows_option(label = "Rows")

  expect_identical(
    blockr_deser(blockr_ser(opt)),
    opt,
    ignore_function_env = TRUE
  )
})

test_that("blockr_deser.list forwards `...` to per-class methods", {

  captured <- NULL

  registerS3method(
    "blockr_deser", "deser_ctx_probe",
    function(x, data, version = NULL, ...) {
      captured <<- version
      structure(list(), class = "deser_ctx_probe")
    },
    envir = asNamespace("blockr.core")
  )

  with_ctx <- blockr_deser(
    list(object = "deser_ctx_probe"),
    version = "1.2.3"
  )

  expect_s3_class(with_ctx, "deser_ctx_probe")
  expect_identical(captured, "1.2.3")

  no_ctx <- blockr_deser(list(object = "deser_ctx_probe"))

  expect_s3_class(no_ctx, "deser_ctx_probe")
  expect_null(captured)
})
