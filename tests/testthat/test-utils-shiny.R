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

test_that("notify(glue = FALSE) surfaces literal text with braces", {

  shown <- NULL

  local_mocked_bindings(
    showNotification = function(ui, ...) {
      shown <<- as.character(ui)
      invisible()
    }
  )

  with_mock_session(
    notify(
      "object {x} not found",
      type = "error",
      glue = FALSE,
      log = FALSE,
      session = session
    )
  )

  expect_match(shown, "object {x} not found", fixed = TRUE)
})

test_that("notify() interpolates by default and errors on bad braces", {

  local_mocked_bindings(showNotification = function(ui, ...) invisible())

  with_mock_session(
    expect_error(
      notify("object {x} not found", type = "error", session = session)
    )
  )
})

test_that("notify(log = FALSE) skips the log entry", {

  logged <- new.env(parent = emptyenv())
  logged$n <- 0L

  local_mocked_bindings(
    showNotification = function(ui, ...) invisible(),
    log_error = function(...) {
      logged$n <- logged$n + 1L
      invisible()
    }
  )

  with_mock_session(
    {
      notify("boom", type = "error", glue = FALSE, log = FALSE,
             session = session)
      expect_identical(logged$n, 0L)

      notify("boom", type = "error", glue = FALSE, session = session)
      expect_identical(logged$n, 1L)
    }
  )
})

test_that("notify_remove removes a notification by id", {

  removed <- NULL

  local_mocked_bindings(
    removeNotification = function(id, ...) {
      removed <<- id
      invisible()
    }
  )

  with_mock_session(notify_remove("toast-1", session))

  expect_identical(removed, "toast-1")
})
