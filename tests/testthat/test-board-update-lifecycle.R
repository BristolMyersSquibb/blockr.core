register_ext_method <- function(generic, method) {

  registerS3method(
    generic, "test_ext_board", method,
    envir = asNamespace("blockr.core")
  )
}

test_that("augment_board_update dispatches and chains via NextMethod", {

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

test_that("board_update lifecycle runs augment before apply and resets", {

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
