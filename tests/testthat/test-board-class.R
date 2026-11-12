test_that("board constructor", {

  expect_s3_class(new_board(new_dataset_block()), "board")

  board <- new_board(
    list(
      d = new_merge_block(),
      a = new_dataset_block(),
      c = new_subset_block(),
      e = new_subset_block(),
      b = new_dataset_block()
    ),
    data.frame(
      id = c("ad", "cd", "bc", "de"),
      from = c("a", "c", "b", "d"),
      to = c("d", "d", "c", "e"),
      input = c("x", "y", "", "")
    ),
    list(bc = c("b", "c")),
    id = "my_board"
  )

  expect_s3_class(board, "board")
  expect_snapshot(print(board))

  expect_true(is_acyclic(board))

  expect_error(
    new_board(
      list(
        a = new_dataset_block(),
        b = new_subset_block()
      ),
      new_link("a", "b", "foo")
    ),
    class = "board_block_link_input_mismatch"
  )

  expect_error(
    new_board(
      list(
        a = new_dataset_block(),
        b = new_subset_block()
      ),
      data.frame(from = "a", to = "b", input = "foo")
    ),
    class = "board_block_link_input_mismatch"
  )

  expect_error(
    new_board(
      list(
        a = new_dataset_block(),
        b = new_dataset_block()
      ),
      data.frame(from = "a", to = "b")
    ),
    class = "board_block_link_arity_mismatch"
  )

  expect_error(
    new_board(
      list(
        a = new_dataset_block(),
        b = new_subset_block()
      ),
      stacks = "ab"
    ),
    class = "board_block_stack_name_mismatch"
  )

  expect_error(
    validate_board(structure("123", class = "board")),
    class = "board_list_like_invalid"
  )

  expect_error(
    validate_board(structure(list(), class = "board")),
    class = "board_list_components_invalid"
  )

  expect_error(
    validate_board(NULL),
    class = "board_inheritance_invalid"
  )

  inps <- lapply(board_blocks(board), block_data_inputs)

  expect_type(inps, "list")
  expect_true(all(lgl_ply(inps, is.character)))

  expect_type(board_option_ids(board), "character")
})

test_that("the getters of a board check that they read one", {

  getters <- list(board_blocks, board_links, board_stacks, board_option_ids,
                  board_id, board_name)

  for (getter in getters) {
    expect_error(getter(list()), class = "board_inheritance_invalid")
  }
})

test_that("R draws a board's ID and names the board after it, unless given", {

  board <- new_board(id = "sales_review", name = "Q3 numbers")

  expect_identical(board_id(board), "sales_review")
  expect_identical(board_name(board), "Q3 numbers")

  expect_identical(board_name(new_board(id = "sales_review")), "Sales review")

  board <- new_board()

  expect_true(is_string(board_id(board)))
  expect_identical(board_name(board), id_to_sentence_case(board_id(board)))

  withr::local_options(blockr.rand_id = "letters_8")

  expect_match(board_id(new_board()), "^[a-z]{8}$")

  for (id in c("123", "0", "")) {
    expect_error(
      new_board(id = id),
      paste0("\"", id, "\""),
      class = "board_id_invalid"
    )
  }

  expect_error(new_board(id = NA_character_), class = "board_id_invalid")
  expect_error(new_board(id = 1L), class = "board_id_invalid")
  expect_error(new_board(id = c("a", "b")), class = "board_id_invalid")

  expect_error(new_board(name = 1L), class = "board_name_invalid")
  expect_error(new_board(name = c("a", "b")), class = "board_name_invalid")
  expect_error(new_board(name = NA_character_), class = "board_name_invalid")

  board <- new_board(id = "sales_review")
  attr(board, "id") <- "42"

  expect_error(validate_board(board), "\"42\"", class = "board_id_invalid")

  attr(board, "id") <- NULL

  expect_error(validate_board(board), class = "board_id_invalid")
})

test_that("R draws the IDs of blocks, links and stacks a script leaves out", {

  board <- new_board(
    c(a = new_dataset_block(), b = new_head_block(), new_subset_block()),
    links = links(from = "a", to = "b"),
    stacks = list(new_stack("a"))
  )

  ids <- board_block_ids(board)

  expect_identical(ids[1:2], c("a", "b"))
  expect_false(ids[3L] %in% c("a", "b", ""))
  expect_length(board_link_ids(board), 1L)
  expect_length(board_stack_ids(board), 1L)

  expect_true(is_acyclic(board))
  expect_setequal(topo_sort(board), ids)
  expect_lt(match("a", topo_sort(board)), match("b", topo_sort(board)))
})

test_that("a board holds the state of its plugins, by their classes' ids", {

  notes <- plugin_ctor(function(text = "") NULL, "notes_plugin")
  grid <- plugin_ctor(function() NULL, c("grid_plugin", "frontend_plugin"))

  board <- new_board(plugins = list(grid(), notes("To do")))

  expect_identical(
    names(board_plugins(board)),
    c("blockr.test-grid_plugin", "blockr.test-notes_plugin")
  )
  expect_true(all(lgl_ply(board_plugins(board), is_plugin)))
  expect_identical(
    plugin_state(board_plugins(board)[["blockr.test-notes_plugin"]]),
    list(text = "To do")
  )

  # Every board is of class board
  expect_identical(class(board), "board")
  expect_identical(board_plugins(new_board()), set_names(list(), character()))

  # A board holds a plugin given alone
  expect_identical(
    names(board_plugins(new_board(plugins = grid()))),
    "blockr.test-grid_plugin"
  )

  # A board holds two frontends, since the app says which plugins run
  dock <- plugin_ctor(function() NULL, c("dock_plugin", "frontend_plugin"))

  expect_length(board_plugins(new_board(plugins = list(dock(), grid()))), 2L)

  # A board takes no other component and no class
  expect_error(new_board(views = list()), "unused argument")
  expect_error(new_board(class = "dock_board"), "unused argument")
})

test_that("a board refuses names and two plugins of one class", {

  notes <- plugin_ctor(function() NULL, "notes_plugin")

  expect_error(
    new_board(plugins = list(notes(), notes())),
    "blockr.test-notes_plugin",
    class = "board_plugins_invalid"
  )

  expect_error(
    new_board(plugins = list(todo = notes())),
    "todo",
    class = "board_plugins_invalid"
  )

  expect_error(new_board(plugins = list(1L)), class = "board_plugins_invalid")
  expect_error(new_board(plugins = "notes"), class = "board_plugins_invalid")

  # A board that holds a plugin under another id than its class's is invalid
  board <- new_board(plugins = notes())
  names(board[["plugins"]]) <- "notes"

  expect_error(validate_board(board), class = "board_plugins_invalid")
})

test_that("validate_board checks each collection, not just cross-relations", {

  board <- new_board(c(a = new_dataset_block(), b = new_subset_block()))

  # Board accessors are pure reads, so validate_board must itself run each
  # collection's validator. A bare list (e.g. from a botched deserialization)
  # slips past the cross-relationship checks but not the per-collection ones.

  blocks_corrupt <- board
  blocks_corrupt[["blocks"]] <- list()

  expect_error(
    validate_board(blocks_corrupt),
    class = "blocks_class_invalid"
  )

  links_corrupt <- board
  links_corrupt[["links"]] <- list()

  expect_error(
    validate_board(links_corrupt),
    class = "links_class_invalid"
  )

  stacks_corrupt <- board
  stacks_corrupt[["stacks"]] <- list()

  expect_error(
    validate_board(stacks_corrupt),
    class = "stacks_class_invalid"
  )
})

test_that("a board has a compact str_value()", {

  board <- new_board(
    blocks = c(a = new_dataset_block(), b = new_subset_block()),
    stacks = list(s1 = new_stack(c("a", "b"), name = "my stack")),
    name = "My board"
  )

  res <- str_value(board)
  lines <- strsplit(res, "\n")[[1L]]

  expect_length(res, 1L)
  expect_identical(lines[1L], "<board> \"My board\"")
  expect_true("<blocks[2]>" %in% lines)
  expect_true("  a: <dataset_block> dataset, package" %in% lines)
  expect_true("<links[0]>" %in% lines)
  expect_true("<stacks[1]>" %in% lines)
  expect_true("  s1: <stack> \"my stack\": a, b" %in% lines)
  expect_true(any(startsWith(lines, "<board_options[")))
  expect_false(any(startsWith(lines, "plugins")))

  expect_identical(capture.output(str(board))[1L], " <board> \"My board\"")

  notes <- plugin_ctor(function() NULL, "notes_plugin")

  lines <- strsplit(str_value(new_board(plugins = notes())), "\n")[[1L]]

  expect_identical(
    lines[length(lines)],
    "plugins: <notes_plugin> blockr.test-notes_plugin"
  )
})

test_that("a board prints its plugins", {

  notes <- plugin_ctor(function(text = "") NULL, "notes_plugin")

  out <- format(new_board(plugins = notes("To do"), name = "B"))

  expect_identical(out[1L], "<board>")
  expect_true("Plugins[1]:" %in% out)
  expect_true("<notes_plugin> blockr.test-notes_plugin" %in% out)
  expect_true("text: To do" %in% out)
})

test_that("empty variadic link inputs stay unnamed", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("BOD"),
      c = new_rbind_block()
    ),
    links = links(
      ac = new_link("a", "c"),
      bc = new_link("b", "c")
    )
  )

  expect_identical(board_links(board)$input, c("", ""))
})

test_that("boards check links against the inputs of block classes", {

  board <- new_board(
    c(
      a = new_dataset_block(),
      b = new_class_head_block(),
      c = new_class_merge_block(),
      d = new_class_rbind_block()
    ),
    links(
      from = c("a", "a", "b", "a", "b"),
      to = c("b", "c", "c", "d", "d"),
      input = c("", "x", "y", "", "b2")
    )
  )

  lnks <- board_links(board)

  expect_identical(lnks$input[lnks$to == "b"], "data")
  expect_identical(lnks$input[lnks$to == "d"], c("", "b2"))

  expect_error(
    new_board(
      c(a = new_dataset_block(), b = new_class_merge_block()),
      links(from = "a", to = "b", input = "z")
    ),
    class = "board_block_link_input_mismatch"
  )

  expect_error(
    new_board(
      c(a = new_dataset_block(), b = new_class_head_block()),
      links(from = c("a", "a"), to = c("b", "b"), input = c("data", "other"))
    ),
    class = "board_block_link_arity_mismatch"
  )
})
