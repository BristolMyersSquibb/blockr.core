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

test_that("update validation", {

  with_mock_session(
    {
      expect_error(
        validate_board_update(list(), new_board()),
        class = "board_update_object_invalid"
      )

      expect_error(
        validate_board_update(reactiveVal("a"), new_board()),
        class = "board_update_type_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(list(test = list(add = "a"))),
          new_board()
        ),
        class = "board_update_components_invalid"
      )
    }
  )

  with_mock_session(
    {
      expect_error(
        validate_board_update(
          reactiveVal(
            list(
              blocks = list(
                add = blocks(a = new_dataset_block()),
                mod = blocks(a = new_dataset_block())
              )
            )
          ),
          new_board()
        ),
        class = "board_update_blocks_add_mod_clash"
      )

      expect_error(
        validate_board_update(
          reactiveVal(
            list(
              blocks = list(
                mod = list(a = list())
              )
            )
          ),
          new_board(blocks(a = new_dataset_block()))
        ),
        class = "board_update_blocks_mod_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(
            list(
              blocks = list(
                mod = blocks(b = new_dataset_block())
              )
            )
          ),
          new_board(blocks(a = new_dataset_block()))
        ),
        class = "board_update_blocks_mod_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(
            list(
              blocks = list(
                mod = structure("xyz", class = "blocks")
              )
            )
          ),
          new_board(blocks(a = new_dataset_block()))
        ),
        class = "blocks_contains_invalid"
      )
    }
  )

  with_mock_session(
    {
      expect_error(
        validate_board_update(
          reactiveVal(
            list(
              links = list(
                add = links(ab = list(from = "a", to = "b", input = "data")),
                mod = links(ab = list(from = "a", to = "b", input = "data"))
              )
            )
          ),
          new_board(
            blocks(
              a = new_dataset_block(),
              b = new_head_block()
            )
          )
        ),
        class = "board_update_links_add_mod_clash"
      )

      expect_error(
        validate_board_update(
          reactiveVal(
            list(
              links = list(
                mod = list(ab = "xyz")
              )
            )
          ),
          new_board(
            blocks(
              a = new_dataset_block(),
              b = new_head_block()
            )
          )
        ),
        class = "board_update_links_mod_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(
            list(
              links = list(
                mod = links(ab = list(from = "a", to = "b", input = "data"))
              )
            )
          ),
          new_board(
            blocks(
              a = new_dataset_block(),
              b = new_head_block()
            )
          )
        ),
        class = "board_update_links_mod_invalid"
      )

      expect_error(
        validate_board_update_links_board(
          reactiveVal(
            list(
              links = list(
                add = links(xyz = list(from = "a", to = "c", input = "data"))
              )
            )
          ),
          new_board(
            blocks(
              a = new_dataset_block(),
              b = new_head_block()
            )
          )
        ),
        class = "board_block_link_name_mismatch"
      )

      expect_error(
        validate_board_update_links_board(
          reactiveVal(
            list(
              links = list(
                mod = links(xyz = list(from = "a", to = "c", input = "data"))
              )
            )
          ),
          new_board(
            blocks(
              a = new_dataset_block(),
              b = new_head_block()
            ),
            links(xyz = list(from = "a", to = "b", input = "data"))
          )
        ),
        class = "board_block_link_name_mismatch"
      )
    }
  )

  with_mock_session(
    {
      expect_error(
        validate_board_update(
          reactiveVal(
            list(
              stacks = list(
                add = stacks(a = "a"),
                mod = stacks(a = "a")
              )
            )
          ),
          new_board(
            blocks(
              a = new_dataset_block()
            )
          )
        ),
        class = "board_update_stacks_add_mod_clash"
      )

      expect_error(
        validate_board_update(
          reactiveVal(
            list(
              stacks = list(
                mod = list(a = "a")
              )
            )
          ),
          new_board(
            blocks(a = new_dataset_block())
          )
        ),
        class = "board_update_stacks_mod_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(
            list(
              stacks = list(
                mod = stacks(b = "b")
              )
            )
          ),
          new_board(
            blocks(a = new_dataset_block()),
            stacks = stacks(a = "a")
          )
        ),
        class = "board_update_stacks_mod_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(
            list(
              stacks = list(
                add = stacks(b = "b")
              )
            )
          ),
          new_board(
            blocks(a = new_dataset_block()),
            stacks = stacks(a = "a")
          )
        ),
        class = "board_block_stack_name_mismatch"
      )

      expect_error(
        validate_board_update(
          reactiveVal(
            list(
              stacks = list(
                mod = stacks(a = "b")
              )
            )
          ),
          new_board(
            blocks(a = new_dataset_block()),
            stacks = stacks(a = "a")
          )
        ),
        class = "board_block_stack_name_mismatch"
      )
    }
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
