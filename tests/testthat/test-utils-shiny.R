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

test_that("trace_start_span injects an attribute-augmenting tracer", {

  skip_if_not(pkg_avail("otel"))

  was_traced <- is_start_span_traced()
  if (!was_traced) trace_start_span()
  withr::defer(if (!was_traced) untrace_start_span())

  body_src <- paste(deparse(body(otel::start_span)), collapse = "\n")
  expect_match(body_src, "code\\.file\\.path", fixed = FALSE)
  expect_match(body_src, "code\\.namespace", fixed = FALSE)
  expect_match(body_src, "getNamespaceInfo", fixed = FALSE)
})

test_that("trace_start_span is a no-op when otel is unavailable", {

  local_mocked_bindings(pkg_avail = function(...) FALSE)

  expect_false(trace_start_span())
  expect_false(untrace_start_span())
  expect_false(is_start_span_traced())
})
