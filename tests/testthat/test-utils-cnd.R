test_that("conditions", {

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
})

test_that("replayed condition messages with braces do not glue-interpolate", {

  # tidyr::pivot_wider() emits duplicate-value warnings whose text contains
  # literal `{summary_fun}`/`{data}` placeholders. Replayed condition messages
  # are already-formatted text and must never be run through glue.
  brace_msg <- "Use `values_fn = {summary_fun}` to summarise duplicates."

  expect_no_error(replay(simpleWarning(brace_msg)))
  expect_no_error(replay(simpleError(brace_msg)))
})
