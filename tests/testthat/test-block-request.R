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

  expect_identical(run_block_request(cls, "n_max", list(data = iris)), 150L)

  expect_identical(
    run_block_request(cls, "values", list(data = iris),
                      args = list(column = "Species", query = "v")),
    c("versicolor", "virginica")
  )

  expect_identical(
    run_block_request(cls, "values", list(data = iris),
                      args = list(column = "Species", limit = 1L)),
    "setosa"
  )

  expect_true(run_block_request(cls, "unset", list(data = iris)))

  expect_error(
    run_block_request(cls, "n_max", list(data = iris), args = list(nope = 1L)),
    class = "block_request_args_invalid"
  )

  expect_error(
    run_block_request(cls, "n_max", list(data = iris),
                      options = list(n_rows = 5L)),
    class = "block_request_args_invalid"
  )

  expect_error(run_block_request(cls, "nope"), class = "block_request_unknown")
  expect_error(run_block_request(cls, 1), class = "block_request_unknown")
})

test_that("a request takes the values of the board options it reads", {

  cls <- new_block_class(
    "opts_block",
    inputs = "data",
    expr = function() bbquote(.(data)),
    ui = js_module("opts.js"),
    requests = list(
      page = new_block_request(
        function(data, options) utils::head(data, options),
        options = new_n_rows_option
      ),
      shown = new_block_request(
        function(rows, mode) list(rows = rows, mode = mode),
        options = list(rows = new_n_rows_option, mode = new_dark_mode_option)
      ),
      unset = new_block_request(
        function(rows) missing(rows),
        options = list(rows = new_n_rows_option)
      )
    )
  )

  iris <- datasets::iris

  expect_identical(
    run_block_request(cls, "page", list(data = iris),
                      options = list(n_rows = 5L)),
    utils::head(iris, 5L)
  )

  expect_identical(
    run_block_request(
      cls, "shown",
      options = list(n_rows = 5L, dark_mode = "dark")
    ),
    list(rows = 5L, mode = "dark")
  )

  expect_identical(
    run_block_request(
      cls, "shown",
      options = list(n_rows = NULL, dark_mode = list(a = 1))
    ),
    list(rows = NULL, mode = list(a = 1))
  )

  expect_true(run_block_request(cls, "unset"))
  expect_false(run_block_request(cls, "unset", options = list(n_rows = NULL)))

  expect_error(
    run_block_request(cls, "page", list(data = iris),
                      options = list(dark_mode = "dark")),
    class = "block_request_args_invalid"
  )

  expect_error(
    run_block_request(cls, "page", list(data = iris),
                      args = list(options = 5L)),
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
    run_block_request(cls, "counts",
                      list(... = list(datasets::BOD, b2 = datasets::iris))),
    c(6L, b2 = 150L)
  )
})

test_that("a request made by new_block_request() says what it reads and does", {

  f <- function(options, path = "") path

  req <- new_block_request(f, options = new_n_rows_option, volatile = TRUE)

  expect_s3_class(req, "block_request")
  expect_identical(req$fun, f)
  expect_identical(req$options, list(options = new_n_rows_option))
  expect_identical(request_option_ids(req), c(options = "n_rows"))
  expect_true(req$volatile)
  expect_false(req$action)

  expect_identical(
    new_block_request(f, options = list(path = new_n_rows_option))$options,
    list(path = new_n_rows_option)
  )

  plain <- new_block_request(function(data) nrow(data))

  expect_null(plain$options)
  expect_identical(request_option_ids(plain), character())

  act <- new_block_request(function(upload) upload, action = TRUE,
                           file = "upload")

  expect_identical(act$file, "upload")
  expect_null(req$file)

  expect_snapshot({
    print(req)
    print(act)
  })

  expect_error(new_block_request("x"), class = "block_request_invalid")

  expect_error(
    new_block_request(f, options = "n_rows"),
    class = "block_request_invalid"
  )

  expect_error(
    new_block_request(f, options = list(new_n_rows_option)),
    class = "block_request_invalid"
  )

  expect_error(
    new_block_request(
      f,
      options = list(path = new_n_rows_option, path = new_dark_mode_option)
    ),
    class = "block_request_invalid"
  )

  expect_error(
    new_block_request(function(path) path, options = new_n_rows_option),
    class = "block_request_invalid"
  )

  expect_error(
    new_block_request(f, volatile = NA),
    class = "block_request_invalid"
  )

  for (file in list("nope", c("path", "options"), "", NA_character_, 1)) {
    expect_error(
      new_block_request(f, file = file),
      class = "block_request_invalid"
    )
  }

  expect_error(
    new_block_request(function(...) NULL, file = "..."),
    class = "block_request_invalid"
  )

  expect_error(
    new_block_request(f, options = list(path = new_n_rows_option),
                      file = "path"),
    class = "block_request_invalid"
  )
})

test_that("a request takes a file only in the argument that it names", {

  cls <- new_block_class(
    "file_block",
    expr = function(path = "") bbquote(.(path)),
    ui = js_module("file.js"),
    requests = list(
      count = new_block_request(
        function(upload, sep = ",") c(upload[["name"]], sep),
        file = "upload"
      ),
      save = new_block_request(function(file = "out.csv") file, action = TRUE),
      peek = function(file = NULL) if (is.null(file)) "no file" else "file"
    )
  )

  sent <- list(datapath = "x.tmp", name = "sales.csv", type = "text/csv",
               size = 18L)

  expect_identical(
    run_block_request(cls, "count", args = list(sep = ";"), file = sent),
    c("sales.csv", ";")
  )

  expect_identical(
    run_block_request(cls, "save", args = list(file = "report.csv")),
    "report.csv"
  )
  expect_identical(run_block_request(cls, "save"), "out.csv")
  expect_identical(run_block_request(cls, "peek"), "no file")

  expect_error(
    run_block_request(cls, "save", file = sent),
    class = "block_request_args_invalid"
  )
  expect_error(
    run_block_request(cls, "peek", file = sent),
    class = "block_request_args_invalid"
  )
  expect_error(
    run_block_request(cls, "count", args = list(upload = "x")),
    class = "block_request_args_invalid"
  )
})

test_that("a registry entry describes each request", {

  cls <- new_block_class(
    "req_block",
    inputs = "data",
    expr = function(column = NULL) bbquote(.(data)),
    ui = js_module("req.js"),
    requests = list(
      values = new_block_request(
        function(data, column, rows, limit = 10L) NULL,
        options = list(rows = new_n_rows_option)
      ),
      dirs = new_block_request(function(path = "") NULL, volatile = TRUE),
      upload = new_block_request(function(sent, column) NULL, action = TRUE,
                                 file = "sent")
    )
  )

  expect_identical(
    request_entry(cls$requests$values, cls),
    list(inputs = "data", state_args = "column", options = c(rows = "n_rows"),
         volatile = FALSE, action = FALSE, file = NULL)
  )

  expect_identical(
    request_entry(cls$requests$dirs, cls),
    list(inputs = character(), state_args = character(),
         options = character(), volatile = TRUE, action = FALSE, file = NULL)
  )

  expect_identical(
    request_entry(cls$requests$upload, cls),
    list(inputs = character(), state_args = "column",
         options = character(), volatile = FALSE, action = TRUE,
         file = "sent")
  )
})
