test_that("a board option's requests are registered under its id", {

  withr::defer(unregister_requests("data_dir"))

  list_dir <- function(path = ".") list.files(path)

  entry <- register_requests(list(list_dir = list_dir), option = "data_dir",
                             package = "blockr.io")

  expect_identical(entry$id, "data_dir")
  expect_identical(entry$package, "blockr.io")
  expect_identical(entry$allow_locked, character())
  expect_identical(registered_request("option", "data_dir", "list_dir"),
                   list_dir)

  # Registering again, as when the package is loaded again, replaces them.
  register_requests(list(count = function() 1L), option = "data_dir",
                    package = "blockr.io")

  expect_error(
    registered_request("option", "data_dir", "list_dir"),
    class = "registered_request_unknown"
  )

  expect_error(
    register_requests(list(count = function() 2L), option = "data_dir",
                      package = "blockr.other"),
    "registered already, by blockr.io",
    class = "registered_requests_taken"
  )
})

test_that("an extension's requests are registered under {pkg}-{name}", {

  withr::defer(unregister_requests(extension = "blockr.code-code"))

  script <- function(reads) names(reads)

  entry <- register_requests(list(script = script), extension = "code",
                             allow_locked = "script", package = "blockr.code")

  expect_identical(entry$id, "blockr.code-code")
  expect_identical(entry$allow_locked, "script")
  expect_identical(
    registered_request("extension", "blockr.code-code", "script"),
    script
  )

  expect_error(
    registered_request("extension", "blockr.code-code", "nonsense"),
    "No package registers a request nonsense for extension blockr.code-code",
    class = "registered_request_unknown"
  )

  expect_error(
    register_requests(list(script = script), extension = "code",
                      package = NULL),
    class = "registered_requests_invalid"
  )
})

test_that("registration refuses requests it cannot resolve", {

  fun <- function() 1L

  expect_error(
    register_requests(list(a = fun), package = "p"),
    class = "registered_requests_invalid"
  )

  expect_error(
    register_requests(list(a = fun), option = "o", extension = "e",
                      package = "p"),
    class = "registered_requests_invalid"
  )

  expect_error(
    register_requests(list(a = fun), option = "", package = "p"),
    class = "registered_requests_invalid"
  )

  expect_error(
    register_requests(list(a = fun), extension = "", package = "p"),
    class = "registered_requests_invalid"
  )

  expect_error(
    register_requests(list(fun), option = "o", package = "p"),
    class = "registered_requests_invalid"
  )

  expect_error(
    register_requests(list(a = 1), option = "o", package = "p"),
    class = "registered_requests_invalid"
  )

  expect_error(
    register_requests(list(a = fun), option = "o", allow_locked = "b",
                      package = "p"),
    class = "registered_requests_invalid"
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
