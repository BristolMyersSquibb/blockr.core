test_that("conditions", {

  with_mock_session(
    {
      vals <- reactiveValues(test = character())

      res <- withr::with_options(
        list(blockr.show_conditions = c("warning", "error")),
        isolate(
          capture_conditions(
            {
              message("hello")
              warning("test1")
              warning("test2")
              stop("bye")
            },
            rv = vals,
            slot = "test",
            error_val = NULL
          )
        )
      )

      expect_null(res)

      cnds <- isolate(vals$test)

      expect_named(cnds, c("message", "warning", "error"))

      expect_length(cnds$message, 0L)
      expect_length(cnds$warning, 2L)
      expect_length(cnds$error, 1L)

      res <- withr::with_options(
        list(blockr.show_conditions = c("message", "warning", "error")),
        isolate(
          capture_conditions(
            {
              message("hello")
              warning("test1")
              warning("test2")
              stop("bye")
            },
            rv = vals,
            slot = "test",
            error_val = NULL
          )
        )
      )

      expect_null(res)

      cnds <- isolate(vals$test)

      expect_named(cnds, c("message", "warning", "error"))

      expect_length(cnds$message, 1L)
      expect_length(cnds$warning, 2L)
      expect_length(cnds$error, 1L)
    }
  )

  test_val <- "world"

  expect_error(
    blockr_abort("hello {test_val}", class = "test_error"),
    class = "test_error"
  )

  expect_warning(
    blockr_warn("hello {test_val}", class = "test_warning"),
    class = "test_warning"
  )

  expect_message(
    blockr_inform("hello {test_val}", class = "test_message"),
    class = "test_message"
  )

  cnd <- new_condition("xyz", as_list = FALSE)

  expect_s3_class(cnd, "block_cnd")
  expect_identical(cnd, new_condition(cnd, as_list = FALSE))
})
