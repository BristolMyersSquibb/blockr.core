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

test_that("a partial block-state snapshot serializes from constructor scope", {

  blks <- blocks(
    a = new_dataset_block("iris", "datasets"),
    b = new_dataset_block("mtcars", "datasets")
  )

  full <- blockr_ser(
    blks,
    list(
      a = list(dataset = "iris", package = "datasets"),
      b = list(dataset = "mtcars", package = "datasets")
    )
  )

  partial <- blockr_ser(
    blks,
    list(a = list(dataset = "iris", package = "datasets"))
  )

  expect_identical(partial, full)

  expect_identical(
    blks,
    blockr_deser(partial),
    ignore_function_env = TRUE
  )

  expect_error(
    blockr_ser(blks, list(a = list(dataset = "iris"), z = list())),
    "names\\(blocks\\)"
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

test_that("a board with variadic links round-trips and still evaluates", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("BOD"),
      c = new_rbind_block()
    ),
    links = links(
      ac = new_link("a", "c"),
      bc = new_link("b", "c")
    )
  )

  expect_identical(
    board,
    blockr_deser(blockr_ser(board)),
    ignore_function_env = TRUE
  )

  restored <- blockr_deser(blockr_ser(board))

  testServer(
    get_s3_method("board_server", restored),
    {
      session$flushReact()

      expect_identical(
        rv$blocks$c$server$result(),
        rbind(datasets::BOD, datasets::BOD)
      )
    },
    args = list(x = restored)
  )
})

test_that("blocks deser drops offending blocks when on_error is drop", {

  blks <- c(a = new_dataset_block("iris"), b = new_subset_block())

  class_err <- blockr_ser(blks)
  class_err$payload$b$object <- c("foo_block", class_err$payload$b$object)

  expect_error(
    blockr_deser(class_err),
    class = "block_deser_class_error"
  )

  expect_error(
    blockr_deser(class_err, on_error = "abort"),
    class = "block_deser_class_error"
  )

  kept <- expect_output(
    blockr_deser(class_err, on_error = "drop"),
    "Dropping block b"
  )

  expect_true(is_blocks(kept))
  expect_identical(names(kept), "a")

  miss_pkg <- blockr_ser(blks)
  miss_pkg$payload$b$constructor$package <- "some_imaginary_pkg"

  expect_error(
    blockr_deser(miss_pkg),
    class = "blockr_deser_missing_pkg"
  )

  dropped_pkg <- expect_output(
    blockr_deser(miss_pkg, on_error = "drop"),
    "Dropping block b"
  )

  expect_identical(names(dropped_pkg), "a")

  expect_error(blockr_deser(class_err, on_error = "nonsense"))
})

test_that("board deser prunes links and stacks referencing dropped blocks", {

  board <- new_board(
    c(
      a = new_dataset_block("iris"),
      b = new_subset_block(),
      c = new_subset_block()
    ),
    links = links(from = "a", to = "b"),
    stacks = list(with_a = new_stack(c("a", "b")), only_c = new_stack("c"))
  )

  ser <- blockr_ser(board)

  for (id in c("b", "c")) {
    ser$payload$blocks$payload[[id]]$object <- c(
      "foo_block", ser$payload$blocks$payload[[id]]$object
    )
  }

  expect_error(
    blockr_deser(ser),
    class = "block_deser_class_error"
  )

  res <- expect_output(
    blockr_deser(ser, on_error = "drop"),
    "Dropping block b"
  )

  expect_true(is_board(res))
  expect_identical(board_block_ids(res), "a")
  expect_length(board_links(res), 0L)

  # with_a keeps its surviving member; only_c empties out and is removed
  expect_identical(board_stack_ids(res), "with_a")
  expect_identical(stack_blocks(board_stacks(res)[["with_a"]]), "a")
})

test_that("deser_on_error default is sourced from a blockr_option", {

  blks <- c(a = new_dataset_block("iris"), b = new_subset_block())
  ser <- blockr_ser(blks)
  ser$payload$b$object <- c("foo_block", ser$payload$b$object)

  withr::with_envvar(
    c(BLOCKR_DESER_ON_ERROR = NA),
    withr::with_options(
      list(blockr.deser_on_error = NULL),
      expect_error(blockr_deser(ser), class = "block_deser_class_error")
    )
  )

  withr::with_options(
    list(blockr.deser_on_error = "drop"),
    {
      opt_res <- expect_output(blockr_deser(ser), "Dropping block b")
      expect_identical(names(opt_res), "a")
    }
  )

  withr::with_envvar(
    c(BLOCKR_DESER_ON_ERROR = "drop"),
    {
      env_res <- expect_output(blockr_deser(ser), "Dropping block b")
      expect_identical(names(env_res), "a")
    }
  )
})

test_that("board deser resolves the option and threads it to blocks", {

  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_subset_block()),
    links = links(from = "a", to = "b"),
    stacks = list(with_a = new_stack(c("a", "b")))
  )

  ser <- blockr_ser(board)
  ser$payload$blocks$payload$b$object <- c(
    "foo_block", ser$payload$blocks$payload$b$object
  )

  withr::with_envvar(
    c(BLOCKR_DESER_ON_ERROR = NA),
    withr::with_options(
      list(blockr.deser_on_error = NULL),
      expect_error(blockr_deser(ser), class = "block_deser_class_error")
    )
  )

  res <- withr::with_options(
    list(blockr.deser_on_error = "drop"),
    expect_output(blockr_deser(ser), "Dropping block b")
  )

  expect_true(is_board(res))
  expect_identical(board_block_ids(res), "a")
  expect_length(board_links(res), 0L)
  expect_identical(stack_blocks(board_stacks(res)[["with_a"]]), "a")
})
