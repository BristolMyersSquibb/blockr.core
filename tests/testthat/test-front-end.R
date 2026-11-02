test_that("core declares its basic front-end for boards", {

  entry <- get_front_end("blockr.core-basic")

  expect_identical(entry$class, "board")
  expect_identical(entry$package, "blockr.core")
  expect_identical(entry$module, test_module("js/basic.js", "blockr.core"))

  expect_identical(
    names(front_end_options("blockr.core-basic")),
    names(default_board_options())
  )

  expect_identical(board_front_end(new_board()), "blockr.core-basic")
  expect_identical(
    board_front_end(new_board(class = "other_board")),
    "blockr.core-basic"
  )
})

test_that("a board's front-end is the one of its most specific class", {

  local_declared(declared_front_ends)

  entry <- register_front_end(
    "test",
    test_module("test.js"),
    class = "test_board",
    package = "blockr.test"
  )

  expect_identical(entry$id, "blockr.test-test")
  expect_identical(get_front_end("blockr.test-test"), entry)
  expect_true("blockr.test-test" %in% list_front_ends())

  expect_identical(
    board_front_end(new_board(class = "test_board")),
    "blockr.test-test"
  )

  expect_identical(
    board_front_end(new_board(class = c("sub_board", "test_board"))),
    "blockr.test-test"
  )

  expect_identical(board_front_end(new_board()), "blockr.core-basic")

  register_front_end(
    "sub",
    test_module("sub.js"),
    class = "sub_board",
    package = "blockr.test"
  )

  expect_identical(
    board_front_end(new_board(class = c("sub_board", "test_board"))),
    "blockr.test-sub"
  )

  expect_identical(
    board_front_end(new_board(class = "test_board")),
    "blockr.test-test"
  )
})

test_that("R picks the front-end of a board read from today's stack", {

  brd <- blockr_deser(blockr_ser(new_board(name = "My board")))

  expect_identical(board_front_end(brd), "blockr.core-basic")
})

test_that("a board class takes one front-end", {

  local_declared(declared_front_ends)

  register_front_end(
    "one",
    test_module("one.js"),
    class = "test_board",
    package = "blockr.test"
  )

  expect_error(
    register_front_end(
      "two",
      test_module("two.js", "blockr.other"),
      class = "test_board",
      package = "blockr.other"
    ),
    class = "front_end_class_taken",
    regexp = "blockr.test-one and blockr.other-two"
  )

  expect_error(
    register_front_end(
      "basic",
      test_module("basic.js"),
      class = "board",
      package = "blockr.test"
    ),
    class = "front_end_class_taken"
  )

  entry <- register_front_end(
    "one",
    test_module("one-again.js"),
    class = "test_board",
    package = "blockr.test"
  )

  expect_identical(get_front_end("blockr.test-one"), entry)
})

test_that("a front-end's board options are collected for its boards", {

  local_declared(declared_front_ends)

  register_front_end(
    "rows",
    test_module("rows.js"),
    class = "rows_board",
    options = new_n_rows_option,
    package = "blockr.test"
  )

  register_front_end(
    "all",
    test_module("all.js"),
    class = "all_board",
    options = list(new_n_rows_option, default_board_options),
    package = "blockr.test"
  )

  register_front_end(
    "none",
    test_module("none.js"),
    class = "none_board",
    package = "blockr.test"
  )

  expect_identical(names(front_end_options("blockr.test-rows")), "n_rows")
  expect_identical(
    names(front_end_options("blockr.test-all")),
    c("n_rows", "show_conditions", "dark_mode")
  )
  expect_length(front_end_options("blockr.test-none"), 0L)

  brd <- new_board(options = new_board_options(), class = "rows_board")
  opts <- blockr_app_options(brd)

  expect_true("n_rows" %in% names(opts))
  expect_false("dark_mode" %in% names(opts))
  expect_identical(board_option_value(opts[["n_rows"]]), 50L)

  withr::local_options(blockr.n_rows = 25L)

  expect_identical(
    board_option_value(blockr_app_options(brd)[["n_rows"]]),
    25L
  )

  brd <- new_board(options = new_n_rows_option(10L), class = "rows_board")

  expect_identical(
    board_option_value(blockr_app_options(brd)[["n_rows"]]),
    10L
  )
})

test_that("a front-end is declared by the package whose code declares it", {

  local_declared(declared_front_ends)

  declare <- function() {
    register_front_end("pkg", js_module("pkg.js"), class = "pkg_board")
  }

  environment(declare) <- asNamespace("blockr.core")

  entry <- declare()

  expect_identical(entry$id, "blockr.core-pkg")
  expect_identical(entry$module$package, "blockr.core")
})

test_that("a front-end's declaration is checked", {

  local_declared(declared_front_ends)

  mod <- test_module("test.js")

  expect_error(
    register_front_end("test", mod, class = "test_board", package = NULL),
    class = "front_end_invalid",
    regexp = "declared by a package"
  )

  expect_error(
    register_front_end("", mod, class = "test_board", package = "blockr.test"),
    class = "front_end_invalid"
  )

  expect_error(
    register_front_end("test", "test.js", class = "test_board",
                       package = "blockr.test"),
    class = "front_end_invalid"
  )

  expect_error(
    register_front_end("test", mod, class = c("a_board", "b_board"),
                       package = "blockr.test"),
    class = "front_end_invalid"
  )

  expect_error(
    register_front_end("test", mod, class = "test_board", options = "n_rows",
                       package = "blockr.test"),
    class = "front_end_invalid"
  )

  expect_error(get_front_end("blockr.test-test"), class = "front_end_missing")

  register_front_end("test", mod, class = "test_board",
                     options = function() 50L, package = "blockr.test")

  expect_error(
    front_end_options("blockr.test-test"),
    class = "front_end_invalid",
    regexp = "blockr.test-test"
  )

  rm(list = list_front_ends(), envir = declared_front_ends)

  expect_error(board_front_end(new_board()), class = "front_end_missing")
})
