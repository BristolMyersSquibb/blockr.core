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

test_that("trim_rv removes keys, invalidates readers, and allows re-add", {

  with_mock_session(
    {
      rv <- reactiveValues(a = 1, b = 2, c = 3)

      seen <- NULL
      observe(seen <<- list(names = names(rv), b = rv[["b"]]))
      session$flushReact()

      expect_setequal(seen$names, c("a", "b", "c"))
      expect_identical(seen$b, 2)

      # Unlike `rv[["b"]] <- NULL` (which leaves a NULL-valued key behind),
      # trim_rv() drops the key outright and re-runs the reader.
      trim_rv(rv, "b")
      session$flushReact()

      expect_setequal(seen$names, c("a", "c"))
      expect_null(seen$b)

      # The slot is genuinely free, so the same key can be re-added.
      rv[["b"]] <- 99
      session$flushReact()

      expect_setequal(seen$names, c("a", "c", "b"))
      expect_identical(seen$b, 99)

      expect_error(trim_rv(rv, "absent"))
    }
  )
})

test_that("trim_rv and reorder_rv run outside a reactive context", {

  rv <- reactiveValues(a = 1, b = 2, c = 3)

  expect_silent(trim_rv(rv, "b"))
  expect_setequal(isolate(names(rv)), c("a", "c"))

  expect_silent(reorder_rv(rv, c("c", "a")))
  expect_identical(isolate(names(rv)), c("c", "a"))

  expect_error(trim_rv(rv, "absent"))
  expect_error(reorder_rv(rv, c("a", "b", "c")))
})

test_that("trim_rv invalidates reactiveValuesToList readers", {

  with_mock_session(
    {
      rv <- reactiveValues(a = 1, b = 2, c = 3)

      as_seen <- NULL
      all_seen <- NULL
      observe(as_seen <<- reactiveValuesToList(rv))
      observe(all_seen <<- reactiveValuesToList(rv, all.names = TRUE))
      session$flushReact()

      expect_true(all(c("a", "b", "c") %in% names(as_seen)))
      expect_true(all(c("a", "b", "c") %in% names(all_seen)))

      trim_rv(rv, "b")
      session$flushReact()

      expect_false("b" %in% names(as_seen))
      expect_false("b" %in% names(all_seen))
    }
  )
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
