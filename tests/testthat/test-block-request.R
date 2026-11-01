test_that("a request's args are bound by name", {

  cls <- new_block_class(
    "req_block",
    inputs = "data",
    expr = function(column = NULL, n = 1L) bbquote(utils::head(.(data), .(n))),
    ui = js_module("req.js"),
    requests = list(
      n_max = function(data) nrow(data),
      values = function(data, column, query = "", limit = 1000L) {
        vals <- unique(as.character(data[[column]]))
        utils::head(vals[grepl(query, vals, fixed = TRUE)], limit)
      },
      page = reads_options(function(data, options) options, new_n_rows_option),
      unset = function(data, column) missing(column)
    )
  )

  iris <- datasets::iris

  expect_identical(block_request(cls, "n_max", list(data = iris)), 150L)

  expect_identical(
    block_request(cls, "values", list(data = iris),
                  args = list(column = "Species", query = "v")),
    c("versicolor", "virginica")
  )

  expect_identical(
    block_request(cls, "values", list(data = iris),
                  args = list(column = "Species", limit = 1L)),
    "setosa"
  )

  expect_identical(
    block_request(cls, "page", list(data = iris), options = list(n_rows = 5L)),
    list(n_rows = 5L)
  )

  expect_error(
    block_request(cls, "n_max", list(data = iris), options = list(n_rows = 5L)),
    class = "block_request_args_invalid"
  )

  expect_true(block_request(cls, "unset", list(data = iris)))

  expect_error(
    block_request(cls, "n_max", list(data = iris), args = list(nope = 1L)),
    class = "block_request_args_invalid"
  )

  expect_error(block_request(cls, "nope"), class = "block_request_unknown")
  expect_error(block_request(cls, 1), class = "block_request_unknown")
})

test_that("a variadic block's request takes its inputs as ...", {

  cls <- new_block_class(
    "var_block",
    inputs = "...",
    expr = function() bbquote(rbind(...)),
    ui = js_module("var.js"),
    requests = list(
      counts = function(...) vapply(list(...), nrow, integer(1L))
    )
  )

  expect_identical(
    block_request(cls, "counts",
                  list(... = list(datasets::BOD, b2 = datasets::iris))),
    c(6L, b2 = 150L)
  )
})

test_that("volatile(), action() and reads_options() mark a request", {

  f <- function(options, path = "") path

  vol <- volatile(f)
  act <- action(function(file) file)

  expect_true(is_volatile(vol))
  expect_false(is_action(vol))
  expect_true(is_action(act))
  expect_identical(vol(path = "x"), "x")
  expect_null(oldClass(vol))

  expect_identical(request_file_arg(act), "file")
  expect_identical(request_file_arg(vol), character())

  expect_identical(
    volatile(reads_options(f, new_n_rows_option)),
    reads_options(volatile(f), new_n_rows_option)
  )

  expect_identical(
    reads_options(reads_options(f, new_n_rows_option), new_dark_mode_option),
    reads_options(f, new_n_rows_option, new_dark_mode_option)
  )

  expect_error(volatile("x"), class = "block_request_invalid")
  expect_error(reads_options(f, "n_rows"), class = "block_request_invalid")
})

test_that("a registry entry describes each request", {

  cls <- new_block_class(
    "req_block",
    inputs = "data",
    expr = function(column = NULL) bbquote(.(data)),
    ui = js_module("req.js"),
    requests = list(
      values = reads_options(
        function(data, column, options, limit = 10L) NULL,
        new_n_rows_option
      ),
      dirs = volatile(function(path = "") NULL),
      upload = action(function(file, column) NULL)
    )
  )

  expect_identical(
    request_entry(cls$requests$values, cls),
    list(inputs = "data", state_args = "column", options = "n_rows",
         volatile = FALSE, action = FALSE)
  )

  expect_identical(
    request_entry(cls$requests$dirs, cls),
    list(inputs = character(), state_args = character(),
         options = character(), volatile = TRUE, action = FALSE)
  )

  expect_identical(
    request_entry(cls$requests$upload, cls),
    list(inputs = character(), state_args = "column",
         options = character(), volatile = FALSE, action = TRUE)
  )
})
