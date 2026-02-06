test_that("serialization", {

  blk_1 <- new_dataset_block("iris", "datasets")

  expect_identical(
    blk_1,
    blockr_deser(blockr_ser(blk_1)),
    ignore_function_env = TRUE
  )

  ser_1 <- blockr_ser(
    new_dataset_block(),
    list(dataset = "iris", package = "datasets")
  )

  expect_identical(
    new_dataset_block("iris", "datasets"),
    blockr_deser(ser_1),
    ignore_function_env = TRUE
  )

  blks_1 <- c(
    a = new_dataset_block(),
    b = new_subset_block()
  )

  expect_identical(
    blks_1,
    blockr_deser(blockr_ser(blks_1)),
    ignore_function_env = TRUE
  )

  blks_2 <- blocks(
    a = new_dataset_block("iris", "datasets")
  )

  json <- blockr_ser(
    blocks(a = new_dataset_block()),
    list(
      a = list(dataset = "iris", package = "datasets")
    )
  )

  expect_identical(blks_2, blockr_deser(json), ignore_function_env = TRUE)

  lnks <- links(from = "a", to = "b")

  expect_identical(lnks, blockr_deser(blockr_ser(lnks)))

  stks <- stacks(ab = c("a", "b"))

  expect_identical(stks, blockr_deser(blockr_ser(stks)))

  board <- new_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_subset_block()
    ),
    links = links(from = "a", to = "b")
  )

  expect_identical(
    board,
    blockr_deser(blockr_ser(board)),
    ignore_function_env = TRUE
  )

  new_identity_block <- function(...) {
    new_transform_block(
      function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {
            list(
              expr = reactive(quote(identity(data))),
              state = list()
            )
          }
        )
      },
      function(id) {
        tagList()
      },
      class = "identity_block",
      block_metadata = list(),
      ...
    )
  }

  blk_2 <- new_identity_block()

  expect_identical(
    blk_2,
    blockr_deser(blockr_ser(blk_2)),
    ignore_function_env = TRUE
  )

  blk_3 <- with_mocked_bindings(
    new_identity_block(
      ctor = "new_identity_block",
      ctor_pkg = "some_imaginary_package"
    ),
    get_ctor_from_pkg = function(...) new_identity_block
  )

  expect_error(
    blockr_deser(
      with_mocked_bindings(blockr_ser(blk_3), pkg_version = function(x) "1.0.0")
    ),
    class = "blockr_deser_missing_pkg"
  )

  ser_2 <- blockr_ser(blk_1)

  ser_2$object <- c("foo_block", ser_2$object)

  expect_error(
    blockr_deser(ser_2),
    class = "block_deser_class_error"
  )

  new_test_opt <- function(val1 = 1, val2 = "2", ...) {
    new_board_option(
      "test_opt",
      list(val1 = val1, val2 = val2),
      ui = function(id) tagList()
    )
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
