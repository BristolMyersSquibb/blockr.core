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

  inps <- block_inputs(board)

  expect_type(inps, "list")
  expect_true(all(lgl_ply(inps, is.character)))

  expect_type(board_option_ids(board), "character")
})

test_that("a board has an ID and a name, which R leaves out unless given", {

  board <- new_board(id = "sales_review", name = "Q3 numbers")

  expect_identical(board_id(board), "sales_review")
  expect_identical(board_name(board), "Q3 numbers")

  board <- new_board()

  expect_null(board_id(board))
  expect_null(board_name(board))

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

  board <- new_board(id = "sales_review")
  attr(board, "id") <- "42"

  expect_error(validate_board(board), "\"42\"", class = "board_id_invalid")
})

test_that("blocks, links and stacks without an ID take part in no link", {

  board <- new_board(
    c(a = new_dataset_block(), b = new_head_block(), new_subset_block()),
    links = links(from = "a", to = "b"),
    stacks = list(new_stack("a"))
  )

  expect_identical(board_block_ids(board), c("a", "b", ""))
  expect_identical(board_link_ids(board), "")
  expect_identical(board_stack_ids(board), "")

  expect_true(is_acyclic(board))
  expect_identical(topo_sort(board), c("a", "b"))

  expect_error(
    export_code(list(), board),
    class = "export_code_block_id_missing"
  )

  expect_error(
    new_board(
      c(new_dataset_block(), b = new_head_block()),
      links = links(from = "", to = "b")
    ),
    class = "board_block_link_name_mismatch"
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
  expect_true("  a: <dataset_block> dataset*, package" %in% lines)
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
