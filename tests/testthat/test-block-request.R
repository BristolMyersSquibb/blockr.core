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

  expect_true(block_request(cls, "unset", list(data = iris)))

  expect_error(
    block_request(cls, "n_max", list(data = iris), args = list(nope = 1L)),
    class = "block_request_args_invalid"
  )

  expect_error(
    block_request(cls, "n_max", list(data = iris), options = list(n_rows = 5L)),
    class = "block_request_args_invalid"
  )

  expect_error(block_request(cls, "nope"), class = "block_request_unknown")
  expect_error(block_request(cls, 1), class = "block_request_unknown")
})

test_that("a request takes the values of the board options it reads", {

  cls <- new_block_class(
    "opts_block",
    inputs = "data",
    expr = function() bbquote(.(data)),
    ui = js_module("opts.js"),
    requests = list(
      page = new_request(
        function(data, options) utils::head(data, options),
        options = new_n_rows_option
      ),
      shown = new_request(
        function(rows, conds) list(rows = rows, conds = conds),
        options = list(rows = new_n_rows_option,
                       conds = new_show_conditions_option)
      ),
      unset = new_request(
        function(rows) missing(rows),
        options = list(rows = new_n_rows_option)
      )
    )
  )

  iris <- datasets::iris

  expect_identical(
    block_request(cls, "page", list(data = iris), options = list(n_rows = 5L)),
    utils::head(iris, 5L)
  )

  expect_identical(
    block_request(
      cls, "shown",
      options = list(n_rows = 5L, show_conditions = c("warning", "error"))
    ),
    list(rows = 5L, conds = c("warning", "error"))
  )

  expect_identical(
    block_request(cls, "shown",
                  options = list(n_rows = NULL, show_conditions = list(a = 1))),
    list(rows = NULL, conds = list(a = 1))
  )

  expect_true(block_request(cls, "unset"))
  expect_false(block_request(cls, "unset", options = list(n_rows = NULL)))

  expect_error(
    block_request(cls, "page", list(data = iris),
                  options = list(dark_mode = "dark")),
    class = "block_request_args_invalid"
  )

  expect_error(
    block_request(cls, "page", list(data = iris), args = list(options = 5L)),
    class = "block_request_args_invalid"
  )
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

test_that("a request made by new_request() says what it reads and does", {

  f <- function(options, path = "") path

  req <- new_request(f, options = new_n_rows_option, volatile = TRUE)

  expect_s3_class(req, "blockr_request")
  expect_identical(req$fun, f)
  expect_identical(req$options, list(options = new_n_rows_option))
  expect_identical(request_option_ids(req), c(options = "n_rows"))
  expect_true(req$volatile)
  expect_false(req$action)

  expect_identical(
    new_request(f, options = list(path = new_n_rows_option))$options,
    list(path = new_n_rows_option)
  )

  plain <- new_request(function(data) nrow(data))

  expect_null(plain$options)
  expect_identical(request_option_ids(plain), character())

  act <- new_request(function(file) file, action = TRUE)

  expect_identical(request_file_arg(act), "file")
  expect_identical(request_file_arg(req), character())

  expect_snapshot({
    print(req)
    print(act)
  })

  expect_error(new_request("x"), class = "block_request_invalid")

  expect_error(
    new_request(f, options = "n_rows"),
    class = "block_request_invalid"
  )

  expect_error(
    new_request(f, options = list(new_n_rows_option)),
    class = "block_request_invalid"
  )

  expect_error(
    new_request(
      f,
      options = list(path = new_n_rows_option, path = new_dark_mode_option)
    ),
    class = "block_request_invalid"
  )

  expect_error(
    new_request(function(path) path, options = new_n_rows_option),
    class = "block_request_invalid"
  )

  expect_error(new_request(f, volatile = NA), class = "block_request_invalid")
})

test_that("a registry entry describes each request", {

  cls <- new_block_class(
    "req_block",
    inputs = "data",
    expr = function(column = NULL) bbquote(.(data)),
    ui = js_module("req.js"),
    requests = list(
      values = new_request(
        function(data, column, rows, limit = 10L) NULL,
        options = list(rows = new_n_rows_option)
      ),
      dirs = new_request(function(path = "") NULL, volatile = TRUE),
      upload = new_request(function(file, column) NULL, action = TRUE)
    )
  )

  expect_identical(
    request_entry(cls$requests$values, cls),
    list(inputs = "data", state_args = "column", options = c(rows = "n_rows"),
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
