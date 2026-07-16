test_that("board server", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(from = "a", to = "b")
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()
      expect_equal(rv$blocks$b$server$result(), iris)
    },
    args = list(x = board)
  )

  empty <- new_board()

  testServer(
    get_s3_method("board_server", empty),
    {
      expect_length(board_blocks(x), 0L)
      expect_length(board_blocks(rv$board), 0L)

      expect_null(board_update())

      board_update(
        list(
          blocks = list(add = as_blocks(new_dataset_block()))
        )
      )

      session$flushReact()

      expect_length(board_blocks(x), 0L)
      expect_length(board_blocks(rv$board), 1L)

      expect_null(board_update())

      board_update(
        list(
          blocks = list(add = as_blocks(new_subset_block()))
        )
      )

      session$flushReact()

      expect_length(board_blocks(x), 0L)
      expect_length(board_blocks(rv$board), 2L)

      expect_null(board_update())

      board_update(
        list(
          blocks = list(rm = board_block_ids(rv$board))
        )
      )

      session$flushReact()

      expect_length(board_blocks(x), 0L)
      expect_length(board_blocks(rv$board), 0L)

      expect_null(board_update())
    },
    args = list(
      x = empty,
      plugins = list(manage_blocks())
    )
  )

  testServer(
    get_s3_method("board_server", empty),
    {
      expect_length(board_blocks(x), 0L)
    },
    args = list(
      x = empty,
      callbacks = function(board, ...) {
        expect_length(board_blocks(board$board), 0L)
        NULL
      }
    )
  )

  test_xtra_args <- function(id, board, plugin_a, plugin_b) {
    moduleServer(
      id,
      function(input, output, session) {

        parent <- reactiveVal()

        board_server(
          "board",
          board,
          list(
            preserve_board(server = plugin_a, ui = NULL),
            manage_blocks(server = plugin_b, ui = NULL)
          ),
          parent = parent
        )
      }
    )
  }

  testServer(
    test_xtra_args,
    session$flushReact(),
    args = list(
      board = empty,
      plugin_a = function(id, board, update, parent) {
        moduleServer(
          id,
          function(input, output, session) {
            observeEvent(TRUE, parent(1L), once = TRUE)
            reactiveVal()
          }
        )
      },
      plugin_b = function(id, board, update, parent) {
        moduleServer(
          id,
          function(input, output, session) {
            observeEvent(
              parent(),
              expect_identical(parent(), 1L)
            )
            NULL
          }
        )
      }
    )
  )

  testServer(
    get_s3_method("board_server", empty),
    {
      session$flushReact()
      rv$abc <- 1L
    },
    args = list(
      x = empty,
      plugins = list(
        preserve_board(
          function(id, board, ...) {
            moduleServer(
              id,
              function(input, output, session) {
                observeEvent(
                  board$abc,
                  expect_identical(board$abc, 1L)
                )
                reactiveVal()
              }
            )
          },
          NULL
        )
      )
    )
  )

  testServer(
    function(id, board, plugin) {
      moduleServer(
        id,
        function(input, output, session) {
          board_server("board", board, preserve_board(plugin, NULL))
        }
      )
    },
    session$flushReact(),
    args = list(
      board = empty,
      plugin = function(id, board, ...) {
        moduleServer(
          id,
          function(input, output, session) {
            observeEvent(
              TRUE,
              {
                expect_error(
                  {
                    board$abc <- 1
                  }
                )
              },
              once = TRUE
            )
            reactiveVal()
          }
        )
      }
    )
  )
})

test_that("blocks$mod with ctrl-able delta writes reactiveVals in place", {

  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_subset_block()),
    links = links(from = "a", to = "b")
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$blocks$b$server$result(), datasets::iris)

      pre_server <- rv$blocks$b$server

      board_update(
        list(
          blocks = list(
            mod = list(b = list(subset = "Species == \"setosa\""))
          )
        )
      )

      session$flushReact()

      expect_identical(rv$blocks$b$server, pre_server)
      expect_identical(
        rv$blocks$b$server$result(),
        subset(datasets::iris, Species == "setosa")
      )
    },
    args = list(x = board)
  )
})

test_that("blocks$mod routes block_name without disturbing the server", {

  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_head_block())
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      pre_server <- rv$blocks$b$server
      pre_result <- rv$blocks$b$server$result()

      board_update(
        list(blocks = list(mod = list(b = list(block_name = "Renamed"))))
      )

      session$flushReact()

      expect_identical(rv$blocks$b$server, pre_server)
      expect_identical(rv$blocks$b$server$result(), pre_result)
      expect_identical(block_name(board_blocks(rv$board)$b), "Renamed")
    },
    args = list(x = board)
  )
})

test_that("blocks$mod with both ctrl-arg and block_name delta applies both", {

  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_subset_block()),
    links = links(from = "a", to = "b")
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      pre_server <- rv$blocks$b$server

      board_update(
        list(
          blocks = list(
            mod = list(
              b = list(
                subset = "Species == \"setosa\"",
                block_name = "Setosa only"
              )
            )
          )
        )
      )

      session$flushReact()

      expect_identical(rv$blocks$b$server, pre_server)
      expect_identical(
        rv$blocks$b$server$result(),
        subset(datasets::iris, Species == "setosa")
      )
      expect_identical(block_name(board_blocks(rv$board)$b), "Setosa only")
    },
    args = list(x = board)
  )
})

test_that("blocks$mod rejects non-ctrl-able arguments", {

  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_head_block(n = 6L)),
    links = links(from = "a", to = "b")
  )

  expect_error(
    validate_board_update(
      list(blocks = list(mod = list(b = list(n = 3L)))),
      board
    ),
    class = "board_update_blocks_mod_not_ctrl"
  )
})

test_that("ctrl plugin sees block_name as a routable var", {

  blk <- new_dataset_block("iris")
  upd <- reactiveVal()

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      session$makeScope("ctrl_block")$setInputs(
        block_name = "NewName",
        submit = 1L
      )

      session$flushReact()

      expect_identical(
        upd(),
        list(
          blocks = list(
            mod = list(blk = list(block_name = "NewName"))
          )
        )
      )
    },
    args = list(
      x = blk,
      ctrl_block = ctrl_block(),
      board = reactiveValues(
        board = new_board(blocks = c(blk = new_dataset_block("iris")))
      ),
      block_id = "blk",
      update = upd
    )
  )
})

test_that("a delta-set block_name survives serialize + deserialize", {

  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_subset_block())
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      board_update(
        list(blocks = list(mod = list(b = list(block_name = "Persisted"))))
      )

      session$flushReact()

      state <- lapply(
        blockr.core:::lst_xtr(rv$blocks, "server", "state"),
        lapply,
        reval_if
      )
      ser <- blockr_ser(rv$board, blocks = state)
      restored <- blockr_deser(ser)

      expect_identical(
        block_name(board_blocks(restored)$b),
        "Persisted"
      )
    },
    args = list(x = board)
  )
})

test_that("block_name attr and ctrl reactiveVal stay in sync without loops", {

  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_subset_block())
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      n_updates <- 0L

      observe({
        upd <- board_update()
        if (!is.null(upd) && !is.null(upd$blocks$mod$b$block_name)) {
          n_updates <<- n_updates + 1L
        }
      })

      session$flushReact()

      board_update(
        list(blocks = list(mod = list(b = list(block_name = "FromBoard"))))
      )

      session$flushReact()

      expect_identical(block_name(board_blocks(rv$board)$b), "FromBoard")

      pre <- n_updates
      session$flushReact()
      expect_identical(n_updates, pre)
    },
    args = list(x = board)
  )
})

test_that("links$mod accepts partial-args deltas via update_link", {

  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_subset_block()),
    links = links(ab = list(from = "a", to = "b", input = "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      board_update(
        list(links = list(mod = list(ab = list(input = "data"))))
      )

      session$flushReact()

      expect_identical(board_links(rv$board)$input, "data")
    },
    args = list(x = board)
  )
})

test_that("stacks$mod merges deltas onto current stack via update_stack", {

  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_subset_block()),
    stacks = stacks(s1 = new_stack(c("a"), "Initial"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      board_update(
        list(stacks = list(mod = list(s1 = list(blocks = c("a", "b")))))
      )

      session$flushReact()

      stk <- board_stacks(rv$board)$s1
      expect_setequal(stack_blocks(stk), c("a", "b"))
      expect_identical(stack_name(stk), "Initial")

      board_update(
        list(stacks = list(mod = list(s1 = list(name = "Renamed"))))
      )

      session$flushReact()

      stk <- board_stacks(rv$board)$s1
      expect_setequal(stack_blocks(stk), c("a", "b"))
      expect_identical(stack_name(stk), "Renamed")
    },
    args = list(x = board)
  )
})

test_that("update_stack default preserves extra attrs through reconstruction", {

  stk <- new_stack(
    c("a", "b"),
    name = "S",
    color = "red",
    pkg = "blockr.core"
  )

  out <- update_stack(stk, list(blocks = "c"))
  expect_identical(stack_blocks(out), "c")
  expect_identical(stack_name(out), "S")
  expect_identical(attr(out, "color"), "red")

  out2 <- update_stack(stk, list(color = "blue"))
  expect_setequal(stack_blocks(out2), c("a", "b"))
  expect_identical(attr(out2, "color"), "blue")
})

test_that("update_link default preserves all fields through reconstruction", {

  lnk <- new_link("a", "b", "data")

  out <- update_link(lnk, list(input = "x"))
  expect_identical(out$from, "a")
  expect_identical(out$to, "b")
  expect_identical(out$input, "x")

  out2 <- update_link(lnk, list(to = "c", input = "y"))
  expect_identical(out2$from, "a")
  expect_identical(out2$to, "c")
  expect_identical(out2$input, "y")
})

test_that("removing a variadic link drops its ...args slot", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("BOD"),
      v = new_rbind_block()
    ),
    links = links(av = new_link("a", "v"), bv = new_link("b", "v"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      args <- rv$inputs$v[["...args"]]
      expect_identical(isolate(length(args)), 2L)

      board_update(list(links = list(rm = "av")))
      session$flushReact()

      expect_identical(isolate(length(args)), 1L)
      expect_identical(rv$blocks$v$server$result(), datasets::BOD)
    },
    args = list(x = board)
  )
})

test_that("variadic ...args slots each bind to their own upstream", {

  board <- new_board(
    blocks = c(
      a = new_static_block(iris[1:3, ]),
      b = new_static_block(iris[4:6, ]),
      v = new_rbind_block()
    ),
    links = links(
      av = new_link("a", "v", "x"),
      bv = new_link("b", "v", "y")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(
        rv$blocks$v$server$result(),
        rbind(x = iris[1:3, ], y = iris[4:6, ])
      )
    },
    args = list(x = board)
  )
})

test_that("update validation", {

  expect_error(
    validate_board_update("a", new_board()),
    class = "board_update_type_invalid"
  )

  expect_silent(
    validate_board_update(list(test = list(add = "a")), new_board())
  )

  expect_error(
    validate_board_update(
      list(
        blocks = list(
          add = blocks(a = new_dataset_block()),
          mod = list(a = list(dataset = "iris"))
        )
      ),
      new_board()
    ),
    class = "board_update_blocks_add_mod_clash"
  )

  expect_error(
    validate_board_update(
      list(blocks = list(mod = list(a = "not a list"))),
      new_board(blocks(a = new_dataset_block()))
    ),
    class = "board_update_blocks_mod_entry_invalid"
  )

  expect_error(
    validate_board_update(
      list(blocks = list(mod = list(b = list(dataset = "iris")))),
      new_board(blocks(a = new_dataset_block()))
    ),
    class = "board_update_blocks_mod_unknown_id"
  )

  expect_error(
    validate_board_update(
      list(blocks = list(mod = list(a = list()))),
      new_board(blocks(a = new_dataset_block()))
    ),
    class = "board_update_blocks_mod_entry_empty"
  )

  expect_error(
    validate_board_update(
      list(blocks = list(mod = list(a = list(no_such_arg = 1)))),
      new_board(blocks(a = new_dataset_block()))
    ),
    class = "board_update_blocks_mod_not_ctrl"
  )

  expect_error(
    validate_board_update(
      list(blocks = list(mod = list(a = list(package = "datasets")))),
      new_board(blocks(a = new_dataset_block()))
    ),
    class = "board_update_blocks_mod_not_ctrl"
  )

  expect_error(
    validate_board_update(
      list(blocks = list(mod = "xyz")),
      new_board(blocks(a = new_dataset_block()))
    ),
    class = "board_update_mod_component_invalid"
  )

  expect_error(
    validate_board_update(
      list(blocks = list(mod = blocks(a = new_dataset_block()))),
      new_board(blocks(a = new_dataset_block()))
    ),
    class = "board_update_blocks_mod_entry_invalid"
  )

  expect_error(
    validate_board_update(
      list(
        links = list(
          add = links(ab = list(from = "a", to = "b", input = "data")),
          mod = links(ab = list(from = "a", to = "b", input = "data"))
        )
      ),
      new_board(blocks(a = new_dataset_block(), b = new_head_block()))
    ),
    class = "board_update_links_add_mod_clash"
  )

  expect_error(
    validate_board_update(
      list(links = list(mod = list(ab = "xyz"))),
      new_board(
        blocks(a = new_dataset_block(), b = new_head_block()),
        links(ab = list(from = "a", to = "b", input = "data"))
      )
    ),
    class = "board_update_links_mod_entry_invalid"
  )

  expect_error(
    validate_board_update(
      list(
        links = list(
          mod = links(ab = list(from = "a", to = "b", input = "data"))
        )
      ),
      new_board(
        blocks(a = new_dataset_block(), b = new_head_block()),
        links(ab = list(from = "a", to = "b", input = "data"))
      )
    ),
    class = "board_update_links_mod_entry_invalid"
  )

  expect_error(
    validate_board_update(
      list(
        links = list(
          add = links(xyz = list(from = "a", to = "c", input = "data"))
        )
      ),
      new_board(blocks(a = new_dataset_block(), b = new_head_block()))
    ),
    class = "board_block_link_name_mismatch"
  )

  expect_error(
    validate_board_update(
      list(links = list(mod = list(xyz = list(to = "c")))),
      new_board(
        blocks(a = new_dataset_block(), b = new_head_block()),
        links(xyz = list(from = "a", to = "b", input = "data"))
      )
    ),
    class = "board_block_link_name_mismatch"
  )

  expect_error(
    validate_board_update(
      list(stacks = list(add = stacks(a = "a"), mod = stacks(a = "a"))),
      new_board(blocks(a = new_dataset_block()))
    ),
    class = "board_update_stacks_add_mod_clash"
  )

  expect_error(
    validate_board_update(
      list(stacks = list(mod = list(a = "a"))),
      new_board(
        blocks(a = new_dataset_block()),
        stacks = stacks(a = "a")
      )
    ),
    class = "board_update_stacks_mod_entry_invalid"
  )

  expect_error(
    validate_board_update(
      list(stacks = list(mod = list(b = list(blocks = "a")))),
      new_board(
        blocks(a = new_dataset_block()),
        stacks = stacks(a = "a")
      )
    ),
    class = "board_update_stacks_mod_unknown_id"
  )

  expect_error(
    validate_board_update(
      list(stacks = list(add = stacks(b = "b"))),
      new_board(
        blocks(a = new_dataset_block()),
        stacks = stacks(a = "a")
      )
    ),
    class = "board_block_stack_name_mismatch"
  )

  expect_error(
    validate_board_update(
      list(stacks = list(mod = list(a = list(blocks = "b")))),
      new_board(
        blocks(a = new_dataset_block()),
        stacks = stacks(a = "a")
      )
    ),
    class = "board_block_stack_name_mismatch"
  )
})

test_that("public validate_board_update", {

  brd <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_subset_block()),
    links = links(ab = new_link(from = "a", to = "b"))
  )

  expect_identical(
    validate_board_update(list(), brd),
    list()
  )

  pay <- list(links = list(rm = "ab"))
  expect_identical(
    validate_board_update(pay, brd),
    pay
  )

  expect_error(
    validate_board_update("not-a-list", brd),
    class = "board_update_type_invalid"
  )

  expect_error(
    validate_board_update(
      list(
        links = list(
          add = links(xy = new_link(from = "x", to = "y", input = "data"))
        )
      ),
      brd
    ),
    class = "board_block_link_name_mismatch"
  )
})

test_that("validate_board_update handles block-rm orphan cleanup (#177)", {

  brd <- new_board(
    blocks = c(my_data = new_dataset_block("iris"), preview = new_head_block()),
    links = c(lnk1 = new_link("my_data", "preview", "data"))
  )

  payload <- list(
    blocks = list(add = c(preview_new = new_head_block()), rm = "preview"),
    links = list(add = c(lnk_new = new_link("my_data", "preview_new", "data")))
  )

  expect_identical(
    validate_board_update(payload, brd),
    payload
  )

  payload_mod <- list(
    blocks = list(rm = "preview"),
    links = list(mod = list(lnk1 = list(input = "data")))
  )

  expect_error(
    validate_board_update(payload_mod, brd),
    class = "board_block_link_name_mismatch"
  )
})

test_that("board server utils", {

  expect_identical(
    bs_theme_colors(MockShinySession$new()),
    list(bg = "auto", fg = "auto", accent = "auto")
  )

  expect_named(
    withr::with_options(
      list(bslib_theme = bslib::bs_theme()),
      bs_theme_colors(MockShinySession$new())
    ),
    c("bg", "fg", "accent")
  )
})
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
    function(board, upd, ...) NextMethod()
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
    function(board, upd, ...) {
      call_log <<- c(call_log, "apply")
      board <- NextMethod()
      attr(board, "ext_seen") <- upd$ext
      board
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

      ext_seen <- attr(rv$board, "ext_seen")
      expect_identical(ext_seen$augmented, TRUE)
      expect_identical(ext_seen$views$grid, "two-up")

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
    function(board, upd, ...) {
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
      expect_named(seen, c("passthrough", "session"))
    },
    args = list(
      x = ext_board,
      plugins = list(manage_blocks()),
      passthrough = "carry me"
    )
  )
})

test_that("apply_board_update.board returns the supplied board unchanged", {

  brd <- new_board()
  expect_identical(apply_board_update(brd, list()), brd)
})

test_that("apply_board_update runs after core has settled rv state", {

  reset_ext_methods()

  observed <- NULL

  register_ext_method(
    "apply_board_update",
    function(board, upd, ...) {
      observed <<- board_block_ids(board)
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

test_that("successful board update records ok outcome with monotonic seq", {

  empty <- new_board()

  testServer(
    get_s3_method("board_server", empty),
    {
      session$flushReact()

      expect_null(rv$last_update)

      board_update(
        list(blocks = list(add = as_blocks(c(a = new_dataset_block()))))
      )

      session$flushReact()

      expect_length(board_blocks(rv$board), 1L)
      expect_true(rv$last_update$ok)
      expect_identical(rv$last_update$phase, "apply")
      expect_identical(rv$last_update$seq, 1L)
      expect_identical(rv$last_update$message, NA_character_)

      expect_identical(make_read_only(rv)$last_update, rv$last_update)

      board_update(
        list(blocks = list(add = as_blocks(c(b = new_subset_block()))))
      )

      session$flushReact()

      expect_length(board_blocks(rv$board), 2L)
      expect_true(rv$last_update$ok)
      expect_identical(rv$last_update$seq, 2L)
    },
    args = list(
      x = empty,
      plugins = list(manage_blocks())
    )
  )
})

test_that("rejected board update records validate-phase failure each time", {

  empty <- new_board()

  testServer(
    get_s3_method("board_server", empty),
    {
      session$flushReact()

      board_update(list(blocks = list(rm = "missing")))

      session$flushReact()

      expect_false(rv$last_update$ok)
      expect_identical(rv$last_update$phase, "validate")
      expect_true(nzchar(rv$last_update$message))
      expect_null(board_update())
      expect_length(board_blocks(rv$board), 0L)

      first_seq <- rv$last_update$seq

      board_update(list(blocks = list(rm = "missing")))

      session$flushReact()

      expect_false(rv$last_update$ok)
      expect_identical(rv$last_update$phase, "validate")
      expect_identical(rv$last_update$seq, first_seq + 1L)
    },
    args = list(
      x = empty,
      plugins = list(manage_blocks())
    )
  )
})

test_that("apply-phase failure records apply outcome", {

  local_mocked_bindings(
    apply_board_update = function(board, upd, ...) {
      stop("apply boom")
    }
  )

  empty <- new_board()

  testServer(
    get_s3_method("board_server", empty),
    {
      session$flushReact()

      board_update(list())

      session$flushReact()

      expect_false(rv$last_update$ok)
      expect_identical(rv$last_update$phase, "apply")
      expect_match(rv$last_update$message, "apply boom")
    },
    args = list(
      x = empty,
      plugins = list(manage_blocks())
    )
  )
})

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

test_that("board combines per-block conditions and exposes them per block", {

  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = boom_block()),
    links = links(ab = new_link("a", "b", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(make_read_only(rv)$conditions, rv$conditions)

      # block b evaluates against ready data but its expression raises, so it
      # is `failed` with an eval-phase error; a stays healthy
      expect_identical(nrow(rv$blocks$a$server$conditions()), 0L)
      expect_identical(rv$eval$b(), "failed")

      b_cond <- rv$blocks$b$server$conditions()

      expect_identical(nrow(b_cond), 1L)
      expect_identical(b_cond$block, "b")
      expect_identical(b_cond$phase, "eval")
      expect_identical(b_cond$severity, "error")

      combined <- rv$conditions()

      expect_named(combined, c("block", "phase", "severity", "message", "id"))
      expect_identical(nrow(combined), 1L)
      expect_identical(combined$block, "b")
    },
    args = list(x = board, plugins = list(manage_blocks()))
  )
})

test_that("board drops conditions of removed blocks", {

  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = boom_block()),
    links = links(ab = new_link("a", "b", "data"))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$eval$b(), "failed")
      expect_identical(nrow(rv$conditions()), 1L)

      board_update(list(blocks = list(rm = "b")))

      session$flushReact()

      expect_identical(nrow(rv$conditions()), 0L)
    },
    args = list(x = board, plugins = list(manage_blocks()))
  )
})

test_that("a block re-added under a destroyed id reconstructs cleanly", {

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

      board_update(list(blocks = list(rm = "b")))
      session$flushReact()

      expect_false("b" %in% names(rv$blocks))

      board_update(
        list(
          blocks = list(add = c(b = new_head_block())),
          links = list(add = links(ab2 = new_link("a", "b", "data")))
        )
      )
      session$flushReact()

      expect_identical(rv$eval$b(), "ready")
      expect_identical(rv$blocks$b$server$result(), utils::head(datasets::iris))
      expect_identical(nrow(rv$blocks$b$server$conditions()), 0L)
    },
    args = list(x = board, plugins = list(manage_blocks()))
  )
})
