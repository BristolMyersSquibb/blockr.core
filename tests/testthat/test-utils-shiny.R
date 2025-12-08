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

  withr::defer(
    {
      if (is_observe_traced()) {
        trace_observe()
      }
    }
  )

  withr::local_options(
    blockr.observe_hook_disabled = FALSE
  )

  untrace_observe()

  expect_false(untrace_observe())
  expect_type(trace_observe(), "closure")
  expect_false(trace_observe())
  expect_true(untrace_observe())
})
