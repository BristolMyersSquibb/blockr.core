test_that("a board option has no requests unless its package gives them", {

  expect_identical(board_option_requests(new_board_option("data_dir")), list())

  testthat::local_mocked_s3_method(
    "board_option_requests",
    "data_dir_option",
    function(x, ...) list(list_dir = function(path = ".") path)
  )

  requests <- board_option_requests(new_board_option("data_dir"))

  expect_named(requests, "list_dir")
  expect_identical(requests$list_dir("~"), "~")
})

test_that("a type without a method of request_call() is refused", {

  x <- structure(list(name = "x"), class = c("nothing_request", "request"))

  expect_error(
    request_call(x, list()),
    "R answers no request of type nothing",
    class = "request_type_unknown"
  )
})

test_that("R binds what it passes a request itself", {

  fun <- function(path, reads) list(path, reads)
  bind <- function(fun, args, bound) {
    request_call_args(fun, "r", args, bound, c("reads", "file"), "invalid")
  }

  expect_identical(
    bind(fun, list(path = "~"), list(reads = list())),
    list(reads = list(), path = "~")
  )

  expect_error(bind(fun, list(reads = 1), list(reads = list())),
               class = "invalid")
  expect_error(bind(fun, list(file = 1), list()), class = "invalid")
  expect_error(bind(fun, list(other = 1), list()), class = "invalid")

  dots <- function(...) list(...)

  expect_identical(
    bind(dots, list(other = 1), list(file = "f")),
    list(other = 1)
  )
})
