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

        cond <- session$returned$cond

        expect_s3_class(cond, "reactivevalues")

        expect_true("block" %in% names(cond))
        expect_named(cond$block, c("warning", "error"), ignore.order = TRUE)

        expect_length(cond$block$warning, 2L)
        expect_length(cond$block$error, 0L)

        for (x in unlst(cond$block)) {
          expect_s3_class(x, "block_cnd")
        }

        expect_true("eval" %in% names(cond))
        expect_named(cond$eval, c("warning", "error"), ignore.order = TRUE)

        expect_length(cond$eval$warning, 1L)
        expect_length(cond$eval$error, 0L)

        for (x in unlst(cond$eval)) {
          expect_s3_class(x, "block_cnd")
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

        cond <- session$returned$cond

        expect_s3_class(cond, "reactivevalues")

        expect_true("block" %in% names(cond))
        expect_named(cond$block, c("warning", "message"), ignore.order = TRUE)

        expect_length(cond$block$warning, 2L)
        expect_length(cond$block$message, 1L)

        for (x in unlst(cond$block)) {
          expect_s3_class(x, "block_cnd")
        }

        expect_true("eval" %in% names(cond))
        expect_named(cond$eval, c("warning", "message"), ignore.order = TRUE)

        expect_length(cond$eval$warning, 1L)
        expect_length(cond$eval$message, 2L)

        for (x in unlst(cond$eval)) {
          expect_s3_class(x, "block_cnd")
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
