test_that("notify_user tracks board$conditions()", {

  testServer(
    notify_user_server,
    {
      expect_null(session$returned)

      session$flushReact()
      expect_identical(shown(), character())

      board$conditions(
        data.frame(
          block = "a",
          phase = "eval",
          severity = "error",
          message = "some error",
          id = "id1"
        )
      )

      session$flushReact()

      expect_identical(shown(), "error-id1")

      board$conditions(empty_conditions_frame())

      session$flushReact()

      expect_identical(shown(), character())
      expect_null(session$returned)
    },
    args = list(
      board = reactiveValues(
        conditions = reactiveVal(empty_conditions_frame())
      )
    )
  )
})

test_that("update_condition_notif gates and de-duplicates", {

  with_mock_session(
    {
      df <- data.frame(
        block = c("a", "b", "c"),
        phase = c("eval", "render", "data"),
        severity = c("error", "warning", "message"),
        message = c("boom", "careful", "fyi"),
        id = c("e1", "w1", "m1")
      )

      shown <- withr::with_options(
        list(blockr.show_conditions = c("warning", "error")),
        update_condition_notif(df, character(), session)
      )

      expect_setequal(shown, c("error-e1", "warning-w1"))

      shown <- withr::with_options(
        list(blockr.show_conditions = c("message", "warning", "error")),
        update_condition_notif(df, shown, session)
      )

      expect_setequal(shown, c("error-e1", "warning-w1", "message-m1"))

      dup <- data.frame(
        block = c("a", "b"),
        phase = c("eval", "render"),
        severity = c("error", "error"),
        message = c("boom", "boom"),
        id = c("e1", "e1")
      )

      shown <- withr::with_options(
        list(blockr.show_conditions = c("warning", "error")),
        update_condition_notif(dup, character(), session)
      )

      expect_identical(shown, "error-e1")

      shown <- withr::with_options(
        list(blockr.show_conditions = c("warning", "error")),
        update_condition_notif(empty_conditions_frame(), shown, session)
      )

      expect_identical(shown, character())
    }
  )
})
