test_that("shiny utils", {

  with_mock_session(
    {
      expect_null(invalidate_inputs())
      expect_false(destroy_observers("test"))
    }
  )

  withr::with_options(
    list(blockr.observe_hook_disabled = TRUE),
    with_mock_session(
      expect_false(destroy_observers("test"))
    )
  )

  with_mock_session(
    {
      expect_null(invalidate_inputs())
      expect_false(destroy_observers("test"))
    }
  )

  withr::with_options(
    list(blockr.observe_hook_disabled = TRUE),
    {
      expect_false(trace_observe())
      expect_false(untrace_observe())
    }
  )

  withr::with_options(
    list(blockr.start_span_hook_disabled = TRUE),
    {
      expect_false(trace_start_span())
      expect_false(untrace_start_span())
    }
  )

  withr::defer(
    {
      if (!is_observe_traced()) {
        trace_observe()
      }
      if (!is_start_span_traced()) {
        trace_start_span()
      }
    }
  )

  withr::local_options(
    blockr.observe_hook_disabled = FALSE,
    blockr.start_span_hook_disabled = FALSE
  )

  untrace_observe()
  untrace_start_span()

  expect_false(untrace_observe())
  expect_type(trace_observe(), "closure")
  expect_false(trace_observe())
  expect_true(untrace_observe())

  skip_if_not(pkg_avail("otel"))

  expect_false(untrace_start_span())
  expect_type(trace_start_span(), "closure")
  expect_false(trace_start_span())
  expect_true(untrace_start_span())
})

test_that("trace_start_span injects code.namespace from code.file.path", {

  skip_if_not(pkg_avail("otel"))

  was_traced <- is_start_span_traced()
  if (!was_traced) trace_start_span()
  withr::defer(if (!was_traced) untrace_start_span())

  tracer <- otel::get_default_tracer_provider()$get_tracer("blockr.test")

  captured <- list()
  original_method <- tracer$start_span
  withr::defer(tracer$start_span <- original_method)

  tracer$start_span <- function(name, attributes = NULL, links = NULL,
                                options = NULL, ...) {
    captured$attrs <<- attributes
    original_method(name, attributes, links, options, ...)
  }

  shiny_root <- normalizePath(
    getNamespaceInfo("shiny", "path"), winslash = "/", mustWork = FALSE
  )

  captured <<- list()
  otel::start_span(
    "test-span",
    attributes = list(
      code.file.path = file.path(shiny_root, "R", "fake.R"),
      other = "unchanged"
    ),
    tracer = tracer
  )

  expect_identical(captured$attrs[["code.namespace"]], "shiny")
  expect_identical(captured$attrs[["other"]], "unchanged")

  captured <<- list()
  otel::start_span(
    "test-span",
    attributes = list(code.file.path = "/home/user/myapp/server.R"),
    tracer = tracer
  )

  expect_null(captured$attrs[["code.namespace"]])

  captured <<- list()
  otel::start_span(
    "test-span",
    attributes = list(other = "no-file-path"),
    tracer = tracer
  )

  expect_null(captured$attrs[["code.namespace"]])
})

test_that("trace_start_span is a no-op when otel is unavailable", {

  local_mocked_bindings(pkg_avail = function(...) FALSE)

  expect_false(trace_start_span())
  expect_false(untrace_start_span())
  expect_false(is_start_span_traced())
})
