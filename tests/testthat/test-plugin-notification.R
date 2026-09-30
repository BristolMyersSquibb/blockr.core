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

test_that("notify_user toasts a status note while the front-end holds it", {

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

  cond_a <- reactiveVal(
    rbind(
      cnd_row("a", "warning", "w1", "waiting", phase = "status"),
      cnd_row("a", "error", "e1", "boom")
    )
  )

  held <- reactiveVal(character())

  board <- reactiveValues(
    blocks = list(a = list(server = list(conditions = cond_a))),
    front_end_eager = held
  )

  testServer(
    notify_user_server,
    {
      session$flushReact()
      expect_identical(rec$events, "show:a-error-e1")

      rec$events <- character()

      held("a")
      session$flushReact()
      expect_identical(rec$events, "show:a-warning-w1")

      rec$events <- character()

      held(character())
      session$flushReact()
      expect_identical(rec$events, "remove:a-warning-w1")

      rec$events <- character()

      # A board no front-end gates puts every block on screen.
      held(TRUE)
      session$flushReact()
      expect_identical(rec$events, "show:a-warning-w1")
    },
    args = list(board = board)
  )
})

test_that("notify_user reads the front-end's set only for a status note", {

  local_mocked_bindings(
    showNotification = function(ui, ..., id = NULL, session = NULL) id,
    removeNotification = function(id, session = NULL) invisible()
  )

  reads <- new.env(parent = emptyenv())
  reads$a <- 0L
  reads$b <- 0L

  val_a <- reactiveVal(cnd_row("a", "warning", "wa", "A", phase = "status"))
  val_b <- reactiveVal(cnd_row("b", "error", "eb", "B"))

  cond_a <- function() {
    reads$a <- reads$a + 1L
    val_a()
  }

  cond_b <- function() {
    reads$b <- reads$b + 1L
    val_b()
  }

  held <- reactiveVal(character())

  board <- reactiveValues(
    blocks = list(
      a = list(server = list(conditions = cond_a)),
      b = list(server = list(conditions = cond_b))
    ),
    front_end_eager = held
  )

  testServer(
    notify_user_server,
    {
      session$flushReact()

      reads_a <- reads$a
      reads_b <- reads$b

      held("a")
      session$flushReact()

      expect_gt(reads$a, reads_a)
      expect_identical(reads$b, reads_b)
    },
    args = list(board = board)
  )
})

test_that("notify_user leaves a status note untoasted for a block off screen", {

  withr::local_options(blockr.background_construction_delay = 0)

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

  board <- new_board(
    blocks = c(s = new_dataset_block("iris"), w = new_head_block())
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      # Checked on request while never on screen, w records why it cannot run,
      # and that stays unannounced.
      board_update(list(evaluate = "w"))
      session$flushReact()

      cnds <- rv$conditions()

      expect_identical(rv$eval[["w"]](), "waiting")
      expect_true(any(cnds$block == "w" & cnds$phase == "status"))
      expect_identical(rec$events, character())

      board_update(list(eager = list(`front-end` = list(add = "w"))))
      session$flushReact()

      expect_length(rec$events, 1L)
      expect_match(rec$events, "^show:w-warning-")

      rec$events <- character()

      board_update(list(eager = list(`front-end` = list(rm = "w"))))
      session$flushReact()

      expect_length(rec$events, 1L)
      expect_match(rec$events, "^remove:w-warning-")
    },
    args = list(
      x = board,
      plugins = board_plugins(board, which = "notify_user"),
      callbacks = function(visibility, ...) {
        visibility$visible[["s"]](TRUE)
        eager("front-end", "s")
      }
    )
  )
})

test_that("notify_user toasts every status note on an ungated board", {

  withr::local_options(blockr.background_construction_delay = 0)

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

  board <- new_board(
    blocks = c(s = new_dataset_block("iris"), w = new_head_block())
  )

  args <- list(x = board, plugins = board_plugins(board, which = "notify_user"))

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()
      expect_match(rec$events, "^show:w-warning-")
    },
    args = args
  )

  rec$events <- character()

  # A declared front-end does not gate with gating switched off.
  withr::local_options(blockr.gate_visibility = FALSE)

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()
      expect_match(rec$events, "^show:w-warning-")
    },
    args = c(args, list(callbacks = function(...) eager("front-end", "s")))
  )
})
