register_ext_method <- function(generic, method) {

  registerS3method(
    generic, "test_ext_board", method,
    envir = asNamespace("blockr.core")
  )
}

reset_ext_methods <- function() {

  register_ext_method(
    "validate_board_update",
    function(payload, board) NextMethod()
  )

  register_ext_method(
    "augment_board_update",
    function(upd, board, ...) NextMethod()
  )

  register_ext_method(
    "apply_board_update",
    function(board, upd, rv, ...) NextMethod()
  )
}

test_that("augment_board_update dispatches and chains via NextMethod", {

  reset_ext_methods()

  register_ext_method(
    "augment_board_update",
    function(upd, board) {
      upd <- NextMethod()
      upd$ext$augmented <- TRUE
      upd
    }
  )

  ext_board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_subset_block()),
    links = links(ab = new_link("a", "b")),
    class = "test_ext_board"
  )

  out <- augment_board_update(
    list(
      blocks = list(rm = "a"),
      ext = list(views = list(grid = "two-up"))
    ),
    ext_board
  )

  expect_identical(out$ext$augmented, TRUE)
  expect_identical(out$ext$views$grid, "two-up")
  expect_identical(out$links$rm, "ab")
})

test_that("subclass errors during augment_board_update propagate", {

  reset_ext_methods()

  register_ext_method(
    "augment_board_update",
    function(upd, board) stop("subclass augment failure")
  )

  ext_board <- new_board(class = "test_ext_board")

  expect_error(
    augment_board_update(list(blocks = list(rm = "x")), ext_board),
    "subclass augment failure"
  )
})

test_that("validate_board_update dispatches and chains via NextMethod", {

  reset_ext_methods()

  register_ext_method(
    "validate_board_update",
    function(payload, board) {
      NextMethod()
      if (length(payload$ext$views)) {
        stopifnot(is.character(payload$ext$views))
      }
      invisible(payload)
    }
  )

  ext_board <- new_board(class = "test_ext_board")

  expect_silent(
    validate_board_update(
      list(ext = list(views = "two-up")),
      ext_board
    )
  )

  expect_error(
    validate_board_update(
      list(ext = list(views = 42L)),
      ext_board
    )
  )

  expect_error(
    validate_board_update(
      list(blocks = list(mod = list(a = "not a list"))),
      ext_board
    ),
    class = "board_update_blocks_mod_unknown_id"
  )
})

test_that("board_update lifecycle runs augment before apply and resets", {

  reset_ext_methods()

  call_log <- character()

  register_ext_method(
    "augment_board_update",
    function(upd, board) {
      call_log <<- c(call_log, "augment")
      upd <- NextMethod()
      upd$ext$augmented <- TRUE
      upd
    }
  )

  register_ext_method(
    "apply_board_update",
    function(board, upd, rv, ...) {
      call_log <<- c(call_log, "apply")
      rv$ext_seen <- upd$ext
      NextMethod()
    }
  )

  ext_board <- new_board(class = "test_ext_board")

  testServer(
    get_s3_method("board_server", ext_board),
    {
      session$flushReact()

      board_update(
        list(
          blocks = list(add = as_blocks(new_dataset_block())),
          ext = list(views = list(grid = "two-up"))
        )
      )

      session$flushReact()

      expect_true(any(call_log == "augment"))
      expect_true(any(call_log == "apply"))
      expect_lt(
        max(which(call_log == "augment")),
        min(which(call_log == "apply"))
      )

      expect_identical(rv$ext_seen$augmented, TRUE)
      expect_identical(rv$ext_seen$views$grid, "two-up")

      expect_length(board_blocks(rv$board), 1L)

      expect_null(board_update())
    },
    args = list(
      x = ext_board,
      plugins = list(manage_blocks())
    )
  )
})

test_that("apply_board_update splices board_server `...` as named args", {

  reset_ext_methods()

  seen <- NULL

  register_ext_method(
    "apply_board_update",
    function(board, upd, rv, ...) {
      seen <<- list(...)
      NextMethod()
    }
  )

  ext_board <- new_board(class = "test_ext_board")

  testServer(
    get_s3_method("board_server", ext_board),
    {
      session$flushReact()
      board_update(list(blocks = list(add = as_blocks(new_dataset_block()))))
      session$flushReact()

      expect_identical(seen$passthrough, "carry me")
      expect_named(seen, c("session", "passthrough"))
    },
    args = list(
      x = ext_board,
      plugins = list(manage_blocks()),
      passthrough = "carry me"
    )
  )
})

test_that("apply_board_update runs after core has settled rv state", {

  reset_ext_methods()

  observed <- NULL

  register_ext_method(
    "apply_board_update",
    function(board, upd, rv, ...) {
      observed <<- board_block_ids(rv$board)
      NextMethod()
    }
  )

  ext_board <- new_board(class = "test_ext_board")

  testServer(
    get_s3_method("board_server", ext_board),
    {
      session$flushReact()
      board_update(
        list(blocks = list(add = as_blocks(c(a = new_dataset_block()))))
      )
      session$flushReact()

      expect_identical(observed, "a")
    },
    args = list(
      x = ext_board,
      plugins = list(manage_blocks())
    )
  )
})
