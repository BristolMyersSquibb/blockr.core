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
