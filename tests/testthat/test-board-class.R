test_that("block constructor", {

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

  sorted <- sort(board)

  expect_true(
    match("a", board_block_ids(sorted)) < match("d", board_block_ids(sorted))
  )

  expect_true(
    match("b", board_block_ids(sorted)) < match("d", board_block_ids(sorted))
  )

  expect_true(
    match("b", board_block_ids(sorted)) < match("c", board_block_ids(sorted))
  )

  expect_true(
    match("c", board_block_ids(sorted)) < match("d", board_block_ids(sorted))
  )

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
})
