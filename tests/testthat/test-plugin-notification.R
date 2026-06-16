cnd_row <- function(block, severity, id, message = "m", phase = "eval") {
  data.frame(
    block = block,
    phase = phase,
    severity = severity,
    message = message,
    id = id
  )
}

record_notifications <- function() {
  rec <- new.env(parent = emptyenv())
  rec$events <- character()
  rec
}

test_that("notify_user shows and clears conditions per block", {

  rec <- record_notifications()

  local_mocked_bindings(
    showNotification = function(ui, ..., id = NULL, session = NULL) {
      rec$events <- c(rec$events, paste0("show:", id))
      id
    },
    removeNotification = function(id, session = NULL) {
      rec$events <- c(rec$events, paste0("remove:", id))
      invisible()
    }
  )

  cond_a <- reactiveVal(empty_conditions_frame())

  board <- reactiveValues(
    blocks = list(a = list(server = list(conditions = cond_a)))
  )

  testServer(
    notify_user_server,
    {
      session$flushReact()
      expect_identical(rec$events, character())

      cond_a(cnd_row("a", "error", "e1", "boom"))
      session$flushReact()
      expect_identical(rec$events, "show:a-error-e1")

      cond_a(empty_conditions_frame())
      session$flushReact()
      expect_identical(rec$events, c("show:a-error-e1", "remove:a-error-e1"))

      expect_null(session$returned)
    },
    args = list(board = board)
  )
})

test_that("notify_user shows a separate toast per block for a shared message", {

  rec <- record_notifications()

  local_mocked_bindings(
    showNotification = function(ui, ..., id = NULL, session = NULL) {
      rec$events <- c(rec$events, paste0("show:", id))
      id
    },
    removeNotification = function(id, session = NULL) {
      rec$events <- c(rec$events, paste0("remove:", id))
      invisible()
    }
  )

  cond_a <- reactiveVal(empty_conditions_frame())
  cond_b <- reactiveVal(empty_conditions_frame())

  board <- reactiveValues(
    blocks = list(
      a = list(server = list(conditions = cond_a)),
      b = list(server = list(conditions = cond_b))
    )
  )

  testServer(
    notify_user_server,
    {
      session$flushReact()

      cond_a(cnd_row("a", "error", "e1", "boom"))
      session$flushReact()

      cond_b(cnd_row("b", "error", "e1", "boom"))
      session$flushReact()

      expect_identical(rec$events, c("show:a-error-e1", "show:b-error-e1"))

      cond_a(empty_conditions_frame())
      session$flushReact()

      expect_identical(
        rec$events,
        c("show:a-error-e1", "show:b-error-e1", "remove:a-error-e1")
      )
    },
    args = list(board = board)
  )
})

test_that("notify_user clears notifications of a removed block", {

  rec <- record_notifications()

  local_mocked_bindings(
    showNotification = function(ui, ..., id = NULL, session = NULL) {
      rec$events <- c(rec$events, paste0("show:", id))
      id
    },
    removeNotification = function(id, session = NULL) {
      rec$events <- c(rec$events, paste0("remove:", id))
      invisible()
    }
  )

  cond_a <- reactiveVal(cnd_row("a", "error", "ea", "boom"))
  cond_b <- reactiveVal(cnd_row("b", "warning", "wb", "careful"))

  board <- reactiveValues(
    blocks = list(
      a = list(server = list(conditions = cond_a)),
      b = list(server = list(conditions = cond_b))
    )
  )

  testServer(
    notify_user_server,
    {
      session$flushReact()
      expect_setequal(rec$events, c("show:a-error-ea", "show:b-warning-wb"))

      rec$events <- character()

      board$blocks <- board$blocks["a"]
      session$flushReact()

      expect_identical(rec$events, "remove:b-warning-wb")
    },
    args = list(board = board)
  )
})

test_that("notify_user reads only the changed block's conditions", {

  local_mocked_bindings(
    showNotification = function(ui, ..., id = NULL, session = NULL) id,
    removeNotification = function(id, session = NULL) invisible()
  )

  reads <- new.env(parent = emptyenv())
  reads$a <- 0L
  reads$b <- 0L

  val_a <- reactiveVal(empty_conditions_frame())
  val_b <- reactiveVal(cnd_row("b", "error", "eb", "B"))

  cond_a <- function() {
    reads$a <- reads$a + 1L
    val_a()
  }

  cond_b <- function() {
    reads$b <- reads$b + 1L
    val_b()
  }

  board <- reactiveValues(
    blocks = list(
      a = list(server = list(conditions = cond_a)),
      b = list(server = list(conditions = cond_b))
    )
  )

  testServer(
    notify_user_server,
    {
      session$flushReact()

      reads_a <- reads$a
      reads_b <- reads$b

      for (i in seq_len(5L)) {
        val_a(cnd_row("a", "error", paste0("ea", i), "A"))
        session$flushReact()
      }

      expect_gt(reads$a, reads_a)
      expect_identical(reads$b, reads_b)
    },
    args = list(board = board)
  )
})

test_that("notif_frame gates by show_conditions", {

  frame <- rbind(
    cnd_row("a", "error", "e1", "boom"),
    cnd_row("b", "warning", "w1", "careful"),
    cnd_row("c", "message", "m1", "fyi")
  )

  with_mock_session(
    {
      gated <- withr::with_options(
        list(blockr.show_conditions = c("warning", "error")),
        notif_frame(frame, session)
      )

      expect_setequal(gated$key, c("a-error-e1", "b-warning-w1"))

      full <- withr::with_options(
        list(blockr.show_conditions = c("message", "warning", "error")),
        notif_frame(frame, session)
      )

      expect_setequal(
        full$key,
        c("a-error-e1", "b-warning-w1", "c-message-m1")
      )
    }
  )
})

test_that("notif_frame collapses a block's duplicate keys to one row", {

  dup <- rbind(
    cnd_row("a", "error", "e1", "boom"),
    cnd_row("a", "error", "e1", "boom", phase = "render")
  )

  with_mock_session(
    {
      res <- withr::with_options(
        list(blockr.show_conditions = c("warning", "error")),
        notif_frame(dup, session)
      )

      expect_identical(nrow(res), 1L)
      expect_identical(res$key, "a-error-e1")
    }
  )
})
