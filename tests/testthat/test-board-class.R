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
    list(bc = c("b", "c"))
  )

  expect_s3_class(board, "board")
  expect_snapshot(print(board))

  srt_inc <- sort(board)

  expect_true(
    match("a", board_block_ids(srt_inc)) < match("d", board_block_ids(srt_inc))
  )

  expect_true(
    match("b", board_block_ids(srt_inc)) < match("d", board_block_ids(srt_inc))
  )

  expect_true(
    match("b", board_block_ids(srt_inc)) < match("c", board_block_ids(srt_inc))
  )

  expect_true(
    match("c", board_block_ids(srt_inc)) < match("d", board_block_ids(srt_inc))
  )

  srt_dec <- sort(board, decreasing = TRUE)

  expect_true(
    match("a", board_block_ids(srt_dec)) > match("d", board_block_ids(srt_dec))
  )

  expect_true(
    match("b", board_block_ids(srt_dec)) > match("d", board_block_ids(srt_dec))
  )

  expect_true(
    match("b", board_block_ids(srt_dec)) > match("c", board_block_ids(srt_dec))
  )

  expect_true(
    match("c", board_block_ids(srt_dec)) > match("d", board_block_ids(srt_dec))
  )

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
    rm_blocks(board, "e"),
    class = "invalid_removal_of_used_block"
  )

  lnks <- board_links(board)
  board_links(board) <- lnks[setdiff(names(lnks), "de")]

  expect_snapshot(print(rm_blocks(board, "e")))

  upd <- reactiveVal(list(blocks = list(rm = "b")))

  isolate(preprocess_board_update(upd, board))

  upd <- isolate(upd())

  expect_type(upd, "list")

  expect_named(upd, c("blocks", "links", "stacks"), ignore.order = TRUE)

  expect_length(upd$links, 1L)
  expect_named(upd$links, "rm")
  expect_identical(upd$links$rm, "bc")

  expect_length(upd$stacks, 1L)
  expect_named(upd$stacks, "mod")
  expect_identical(stack_blocks(upd$stacks$mod[[1L]]), "c")

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

  expect_error(
    rm_blocks(
      new_board(
        blocks(a = new_dataset_block()),
        stacks = stacks(a = "a")
      ),
      blocks(a = new_dataset_block())
    ),
    class = "invalid_removal_of_used_block"
  )

  inps <- block_inputs(board)

  expect_type(inps, "list")
  expect_true(all(lgl_ply(inps, is.character)))

  expect_type(board_option_ids(board), "character")

  opts <- board_options(board)
  board_options(board) <- opts[-1L]

  expect_length(board_options(board), length(opts) - 1L)
})
