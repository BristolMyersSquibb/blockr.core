test_that("block server", {

  blk <- new_dataset_block("iris")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      expect_equal(session$returned$result(), iris)

      session$makeScope("expr")$setInputs(dataset = "mtcars")
      expect_equal(session$returned$result(), mtcars)
    },
    args = list(x = blk, data = list())
  )

  new_identity_block <- function() {
    new_transform_block(
      function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {
            list(
              expr = reactive(quote(identity(data))),
              state = list()
            )
          }
        )
      },
      function(id) {
        tagList()
      },
      class = "identity_block",
      block_metadata = list()
    )
  }

  idt <- new_identity_block()

  testServer(
    get_s3_method("block_server", idt),
    {
      session$flushReact()
      expect_equal(session$returned$result(), iris)
    },
    args = list(x = idt, data = list(data = function() iris))
  )
})

test_that("block conditions", {

  new_conds_block <- function() {
    new_transform_block(
      function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {
            list(
              expr = reactive(
                quote(
                  {
                    message("hello")
                    warning("oh no")
                    message("world")
                    identity(data)
                  }
                )
              ),
              state = list(),
              cond = reactiveValues(
                message = "message",
                warning = c("warning 1", "warning 2")
              )
            )
          }
        )
      },
      function(id) {
        tagList()
      },
      class = "conds_block",
      block_metadata = list()
    )
  }

  cnd <- new_conds_block()

  withr::with_options(
    list(blockr.show_conditions = c("warning", "error")),
    testServer(
      get_s3_method("block_server", cnd),
      {
        session$flushReact()

        expect_s3_class(cond, "reactivevalues")

        expect_true("block" %in% names(cond))
        expect_named(cond$block, c("warning", "error"), ignore.order = TRUE)

        expect_length(cond$block$warning, 2L)
        expect_length(cond$block$error, 0L)

        for (x in unlst(cond$block)) {
          expect_s3_class(x, "blk_cnd")
        }

        expect_true("eval" %in% names(cond))
        expect_named(cond$eval, c("warning", "error"), ignore.order = TRUE)

        expect_length(cond$eval$warning, 1L)
        expect_length(cond$eval$error, 0L)

        for (x in unlst(cond$eval)) {
          expect_s3_class(x, "blk_cnd")
        }
      },
      args = list(x = cnd, data = list(data = function() iris))
    )
  )

  withr::with_options(
    list(blockr.show_conditions = c("message", "warning")),
    testServer(
      get_s3_method("block_server", cnd),
      {
        session$flushReact()

        expect_s3_class(cond, "reactivevalues")

        expect_true("block" %in% names(cond))
        expect_named(cond$block, c("warning", "message"), ignore.order = TRUE)

        expect_length(cond$block$warning, 2L)
        expect_length(cond$block$message, 1L)

        for (x in unlst(cond$block)) {
          expect_s3_class(x, "blk_cnd")
        }

        expect_true("eval" %in% names(cond))
        expect_named(cond$eval, c("warning", "message"), ignore.order = TRUE)

        expect_length(cond$eval$warning, 1L)
        expect_length(cond$eval$message, 2L)

        for (x in unlst(cond$eval)) {
          expect_s3_class(x, "blk_cnd")
        }
      },
      args = list(x = cnd, data = list(data = function() iris))
    )
  )
})

test_that("block expr validation", {

  blk <- new_dataset_block()

  expect_error(
    check_expr_val(NULL, blk),
    class = "expr_server_return_type_invalid"
  )

  expect_error(
    check_expr_val(list(), blk),
    class = "expr_server_return_required_component_missing"
  )

  expect_error(
    check_expr_val(list(expr = "a", state = "b"), blk),
    class = "expr_server_return_expr_invalid"
  )

  expect_error(
    check_expr_val(list(expr = reactiveVal(), state = "b"), blk),
    class = "expr_server_return_state_type_invalid"
  )

  expect_error(
    check_expr_val(list(expr = reactiveVal(), state = list()), blk),
    class = "expr_server_return_state_missing_component"
  )

  expect_error(
    check_expr_val(
      list(
        expr = reactiveVal(),
        state = list(dataset = "a", package = "b", ui = NULL)
      ),
      blk
    ),
    class = "expr_server_return_state_invalid_component"
  )

  expect_error(
    check_expr_val(
      list(
        expr = reactiveVal(),
        state = list(dataset = reactiveVal("a"), package = reactiveVal("b")),
        cond = NULL
      ),
      blk
    ),
    class = "expr_server_return_cond_invalid"
  )

  expect_error(
    with_mock_session(
      check_expr_val(
        list(
          expr = reactiveVal(),
          state = list(dataset = reactiveVal("a"), package = reactiveVal("b")),
          cond = reactiveValues(foo = "abc")
        ),
        blk
      )
    ),
    class = "expr_server_return_cond_invalid"
  )

  with_mock_session(
    {
      check_expr_val(
        list(
          expr = reactiveVal(),
          state = list(
            dataset = reactiveVal("a"),
            package = reactiveVal("b")
          ),
          cond = reactiveValues(message = 123)
        ),
        blk
      )
      sink_msg(expect_warning(session$flushReact()))
    }
  )
})

test_that("block server exposes a reactive conditions frame", {

  blk <- new_dataset_block("iris")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      expect_true(is.reactive(conditions))
      expect_named(
        conditions(),
        c("block", "phase", "severity", "message", "id")
      )
      expect_identical(nrow(conditions()), 0L)

      cond$eval <- list(error = list(new_blk_cnd("boom")))

      session$flushReact()

      res <- conditions()

      expect_identical(nrow(res), 1L)
      expect_identical(res$phase, "eval")
      expect_identical(res$severity, "error")
      expect_identical(res$message, "boom")

      cond$eval <- list(error = list())

      session$flushReact()

      expect_identical(nrow(conditions()), 0L)
    },
    args = list(x = blk, data = list())
  )
})

test_that("an unconnected block is waiting and explains why", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block()
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval$a(), "ready")
      expect_identical(rv$eval$b(), "waiting")

      expect_identical(rv$blocks$a$server$result(), datasets::iris)
      expect_null(rv$blocks$b$server$result())

      # `waiting` surfaces a status-phase explanation (a warning, not an error)
      cnds <- rv$blocks$b$server$conditions()
      expect_false("error" %in% cnds$severity)
      expect_true(any(cnds$phase == "status" & cnds$severity == "warning"))
    },
    args = list(x = board)
  )
})

test_that("a waiting block evaluates once its input is connected", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block()
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()
      expect_identical(rv$eval$b(), "waiting")

      board_update(
        list(links = list(add = links(ab = new_link("a", "b", "data"))))
      )
      session$flushReact()

      expect_identical(rv$eval$b(), "ready")
      expect_identical(
        rv$blocks$b$server$result(),
        utils::head(datasets::iris)
      )
      expect_identical(nrow(rv$blocks$b$server$conditions()), 0L)
    },
    args = list(x = board)
  )
})

test_that("a waiting upstream holds its downstream waiting (cascade)", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block(),
      c = new_head_block()
    ),
    links = links(bc = new_link("b", "c", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval$b(), "waiting")
      expect_identical(rv$eval$c(), "waiting")
      expect_false("error" %in% rv$conditions()$severity)

      board_update(
        list(links = list(add = links(ab = new_link("a", "b", "data"))))
      )
      session$flushReact()

      expect_identical(rv$eval$b(), "ready")
      expect_identical(rv$eval$c(), "ready")
      expect_identical(
        rv$blocks$c$server$result(),
        utils::head(datasets::iris)
      )
    },
    args = list(x = board)
  )
})

test_that("a block returns to waiting when its input is disconnected", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block()
    ),
    links = links(ab = new_link("a", "b", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval$b(), "ready")
      expect_identical(rv$blocks$b$server$result(), utils::head(datasets::iris))

      board_update(list(links = list(rm = "ab")))
      session$flushReact()

      expect_identical(rv$eval$b(), "waiting")
      expect_null(rv$blocks$b$server$result())
    },
    args = list(x = board)
  )
})

test_that("output clears when a ready block's input is disconnected", {

  blk <- new_head_block()
  inputs_ready <- reactiveVal(TRUE)

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      expect_false(is.null(output$result))

      inputs_ready(FALSE)
      session$flushReact()

      expect_null(session$returned$result())

      # a cleared output holds no value, so fetching it errors
      expect_error(output$result)
    },
    args = list(
      x = blk,
      data = list(data = reactive(utils::head(datasets::iris))),
      inputs_ready = inputs_ready
    )
  )
})

test_that("a block whose expression errors is failed, not ready", {

  boom_block <- function() {
    new_transform_block(
      function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {
            list(expr = reactive(quote(stop("boom"))), state = list())
          }
        )
      },
      function(id) tagList(),
      class = "boom_block",
      block_metadata = list()
    )
  }

  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = boom_block()),
    links = links(ab = new_link("a", "b", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval$b(), "failed")
      expect_null(rv$blocks$b$server$result())

      cnds <- rv$blocks$b$server$conditions()
      expect_true(any(cnds$phase == "eval" & cnds$severity == "error"))
    },
    args = list(x = board)
  )
})
