test_that("conditions", {

  with_mock_session(
    {
      vals <- reactiveValues(test = NULL)

      res <- withr::with_options(
        list(blockr.show_conditions = c("warning", "error")),
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

      expect_null(res)

      cnds <- vals$test

      expect_named(cnds, c("warning", "error"), ignore.order = TRUE)

      expect_length(cnds$warning, 2L)
      expect_length(cnds$error, 1L)

      res <- withr::with_options(
        list(blockr.show_conditions = c("message", "warning", "error")),
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

      expect_null(res)

      cnds <- vals$test

      expect_named(cnds, c("message", "warning", "error"), ignore.order = TRUE)

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

test_that("block_cnd data api", {

  cnd <- new_block_cnd("xyz")

  expect_s3_class(cnd, "block_cnd")
  expect_true(is_block_cnd(cnd))
  expect_false(is_block_cnd("xyz"))

  expect_identical(cnd_message(cnd), "xyz")
  expect_identical(cnd_id(cnd), attr(cnd, "id"))
  expect_null(cnd_id("xyz"))

  expect_identical(cnd, new_block_cnd(cnd))
})

test_that("block_cnds flattens a cond", {

  empty <- block_cnds(list())

  expect_s3_class(empty, "data.frame")
  expect_identical(nrow(empty), 0L)
  expect_named(empty, c("block", "phase", "severity", "message", "id"))
  expect_identical(empty, empty_conditions_frame())

  expect_identical(
    nrow(block_cnds(list(eval = NULL, render = NULL), block = "a")),
    0L
  )

  cond <- list(
    state = list(error = list(new_block_cnd("state bad"))),
    eval = list(
      warning = list(new_block_cnd("w1"), new_block_cnd("w2")),
      error = list(new_block_cnd("boom"))
    ),
    block = list(message = list(new_block_cnd("hi")))
  )

  res <- block_cnds(cond, block = "blk1")

  expect_named(res, c("block", "phase", "severity", "message", "id"))
  expect_identical(nrow(res), 5L)
  expect_setequal(unique(res$block), "blk1")
  expect_setequal(res$severity, c("error", "warning", "message"))

  eval_warn <- res[res$phase == "eval" & res$severity == "warning", ]
  expect_setequal(eval_warn$message, c("w1", "w2"))

  expect_identical(
    res$id,
    chr_ply(res$message, function(m) cnd_id(new_block_cnd(m)))
  )

  expect_identical(
    res$id[res$message == "boom"],
    cnd_id(new_block_cnd("boom"))
  )
})
