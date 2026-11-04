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

test_that("a board class's method names its front-end and its package", {

  expect_identical(board_front_end(new_board()), "blockr.core-basic")
  expect_identical(class_package("board"), "blockr.core")

  local_board_front_end("one_board", "blockr.one-grid", "blockr.one")

  expect_identical(
    board_front_end(new_board(class = "one_board")),
    "blockr.one-grid"
  )
  expect_identical(class_package("one_board"), "blockr.one")

  # A class without a method of its own is drawn by its parent's front-end,
  # and comes from no package
  expect_identical(
    board_front_end(new_board(class = c("my_board", "one_board"))),
    "blockr.one-grid"
  )
  expect_null(class_package("my_board"))
})

test_that("a board class's method gives its version, or it is at version 1", {

  expect_identical(board_version(new_board()), 1L)

  local_board_front_end("one_board", "blockr.one-grid", "blockr.one", 2L)
  local_board_front_end("two_board", "blockr.two-grid", "blockr.two")

  expect_identical(class_version("one_board"), 2L)
  expect_identical(class_version("two_board"), 1L)

  # The versions of the classes that name a front-end, by class
  expect_identical(
    board_versions(c("my_board", "one_board", "two_board", "board")),
    list(one_board = 2L, two_board = 1L, board = 1L)
  )
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

  expect_identical(capture.output(str(board))[1L], " <board> \"My board\"")
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
