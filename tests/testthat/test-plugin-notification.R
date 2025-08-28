test_that("notify_user", {

  testServer(
    notify_user_server,
    {
      expect_null(session$returned)

      expect_type(state(), "list")
      expect_length(state(), 0L)

      board$blocks <- list(
        a = list(
          server = list(
            cond = reactiveValues(
              data = NULL,
              eval = NULL
            )
          )
        )
      )

      session$flushReact()

      expect_length(state(), 1L)
      expect_named(state(), "a")

      expect_identical(state()[["a"]][["data"]][["ids"]](), character())
      expect_identical(state()[["a"]][["eval"]][["ids"]](), character())

      board$blocks$a$server$cond$eval <- list(
        error = new_condition("some error")
      )

      session$flushReact()

      expect_identical(state()[["a"]][["data"]][["ids"]](), character())
      expect_length(state()[["a"]][["eval"]][["ids"]](), 1L)
      expect_true(grepl("^error-", state()[["a"]][["eval"]][["ids"]]()))

      board$blocks$a$server$cond$eval <- list(error = list())

      session$flushReact()

      expect_identical(state()[["a"]][["data"]][["ids"]](), character())
      expect_identical(state()[["a"]][["eval"]][["ids"]](), character())

      expect_null(session$returned)
    },
    args = list(board = reactiveValues(blocks = list()))
  )
})

test_that("notify_user return validation", {

  with_mock_session(
    {
      check_block_notifications_val(1)
      sink_msg(
        expect_warning(
          session$flushReact(),
          "Expecting `notify_user` to return `NULL`."
        )
      )
    }
  )
})
