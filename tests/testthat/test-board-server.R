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

test_that("blocks$mod with ctrl-only diff writes reactiveVals", {

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

      new_b <- as_blocks(
        set_names(list(new_subset_block(subset = "Species == \"setosa\"")), "b")
      )

      board_update(list(blocks = list(mod = new_b)))

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

test_that("blocks$mod with non-ctrl diff re-sets up the block server", {

  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_head_block(n = 6L)),
    links = links(from = "a", to = "b")
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(rv$blocks$b$server$result(), utils::head(datasets::iris))

      pre_server <- rv$blocks$b$server

      new_b <- as_blocks(set_names(list(new_head_block(n = 3L)), "b"))

      board_update(list(blocks = list(mod = new_b)))

      session$flushReact()

      expect_false(identical(rv$blocks$b$server, pre_server))
      expect_identical(
        rv$blocks$b$server$result(),
        utils::head(datasets::iris, n = 3L)
      )
    },
    args = list(x = board)
  )
})

test_that("blocks$mod preserves outgoing links across re-setup", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block(n = 6L),
      c = new_head_block(n = 2L)
    ),
    links = links(
      ab = new_link(from = "a", to = "b"),
      bc = new_link(from = "b", to = "c")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_setequal(names(rv$links), c("ab", "bc"))
      expect_identical(
        rv$blocks$c$server$result(),
        utils::head(datasets::iris, n = 2L)
      )

      new_b <- as_blocks(set_names(list(new_head_block(n = 3L)), "b"))

      board_update(list(blocks = list(mod = new_b)))

      session$flushReact()

      expect_setequal(names(rv$links), c("ab", "bc"))
      expect_identical(
        rv$blocks$c$server$result(),
        utils::head(utils::head(datasets::iris, n = 3L), n = 2L)
      )
    },
    args = list(x = board)
  )
})

test_that("blocks$mod with metadata-only change skips re-setup", {

  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_subset_block()),
    links = links(from = "a", to = "b")
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      pre_server <- rv$blocks$b$server
      pre_result <- rv$blocks$b$server$result()

      new_b_blk <- board_blocks(rv$board)[["b"]]
      block_name(new_b_blk) <- "Renamed"

      board_update(
        list(blocks = list(mod = as_blocks(set_names(list(new_b_blk), "b"))))
      )

      session$flushReact()

      expect_identical(rv$blocks$b$server, pre_server)
      expect_identical(rv$blocks$b$server$result(), pre_result)
      expect_identical(block_name(board_blocks(rv$board)$b), "Renamed")
    },
    args = list(x = board)
  )
})

test_that("blocks$mod can swap block class via re-setup", {

  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_head_block()),
    links = links(from = "a", to = "b")
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_s3_class(board_blocks(rv$board)$b, "head_block")

      new_b <- as_blocks(set_names(list(new_subset_block()), "b"))

      board_update(list(blocks = list(mod = new_b)))

      session$flushReact()

      expect_s3_class(board_blocks(rv$board)$b, "subset_block")
      expect_identical(rv$blocks$b$server$result(), datasets::iris)
    },
    args = list(x = board)
  )
})

test_that("update validation", {

  expect_error(
    validate_board_update("a", new_board()),
    class = "board_update_type_invalid"
  )

  expect_error(
    validate_board_update(list(test = list(add = "a")), new_board()),
    class = "board_update_components_invalid"
  )

  expect_error(
    validate_board_update(
      list(
        blocks = list(
          add = blocks(a = new_dataset_block()),
          mod = blocks(a = new_dataset_block())
        )
      ),
      new_board()
    ),
    class = "board_update_blocks_add_mod_clash"
  )

  expect_error(
    validate_board_update(
      list(blocks = list(mod = list(a = list()))),
      new_board(blocks(a = new_dataset_block()))
    ),
    class = "board_update_mod_component_invalid"
  )

  expect_error(
    validate_board_update(
      list(blocks = list(mod = blocks(b = new_dataset_block()))),
      new_board(blocks(a = new_dataset_block()))
    ),
    class = "board_update_blocks_mod_invalid"
  )

  expect_error(
    validate_board_update(
      list(blocks = list(mod = structure("xyz", class = "blocks"))),
      new_board(blocks(a = new_dataset_block()))
    ),
    class = "blocks_contains_invalid"
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
      new_board(blocks(a = new_dataset_block(), b = new_head_block()))
    ),
    class = "board_update_mod_component_invalid"
  )

  expect_error(
    validate_board_update(
      list(
        links = list(
          mod = links(ab = list(from = "a", to = "b", input = "data"))
        )
      ),
      new_board(blocks(a = new_dataset_block(), b = new_head_block()))
    ),
    class = "board_update_links_mod_invalid"
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
      list(
        links = list(
          mod = links(xyz = list(from = "a", to = "c", input = "data"))
        )
      ),
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
      new_board(blocks(a = new_dataset_block()))
    ),
    class = "board_update_mod_component_invalid"
  )

  expect_error(
    validate_board_update(
      list(stacks = list(mod = stacks(b = "b"))),
      new_board(
        blocks(a = new_dataset_block()),
        stacks = stacks(a = "a")
      )
    ),
    class = "board_update_stacks_mod_invalid"
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
      list(stacks = list(mod = stacks(a = "b"))),
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
