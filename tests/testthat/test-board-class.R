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

  empty <- clear_board(board)

  expect_length(board_blocks(empty), 0L)
  expect_length(board_links(empty), 0L)
  expect_length(board_stacks(empty), 0L)

  expect_identical(
    board_options(board),
    board_options(empty)
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
    stacks = list(s1 = new_stack(c("a", "b"), name = "my stack"))
  )

  res <- str_value(board)
  lines <- strsplit(res, "\n")[[1L]]

  expect_length(res, 1L)
  expect_identical(lines[1L], "<board>")
  expect_true("<blocks[2]>" %in% lines)
  expect_true("  a: <dataset_block> dataset*, package" %in% lines)
  expect_true("<links[0]>" %in% lines)
  expect_true("<stacks[1]>" %in% lines)
  expect_true("  s1: <stack> \"my stack\": a, b" %in% lines)
  expect_true(any(startsWith(lines, "<board_options[")))

  expect_identical(capture.output(str(board))[1L], " <board>")
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

test_that("editing a link via add + rm overlap preserves its position", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("BOD"),
      c = new_rbind_block()
    ),
    links = links(
      ac = new_link("a", "c", "left"),
      bc = new_link("b", "c")
    )
  )

  edited <- modify_board_links(
    board,
    add = links(ac = new_link("a", "c", "x")),
    rm = "ac"
  )

  expect_identical(names(board_links(edited)), c("ac", "bc"))
  expect_identical(board_links(edited)[["ac"]][["input"]], "x")
})

test_that("added links can be placed before or after another link", {

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

  place <- function(...) {
    names(
      board_links(
        modify_board_links(board, add = links(xc = new_link("a", "c")), ...)
      )
    )
  }

  expect_identical(place(), c("ac", "bc", "xc"))
  expect_identical(place(before = c(xc = "ac")), c("xc", "ac", "bc"))
  expect_identical(place(after = c(xc = "ac")), c("ac", "xc", "bc"))
  expect_identical(place(before = c(xc = "bc")), c("ac", "xc", "bc"))
  expect_identical(place(after = c(xc = "bc")), c("ac", "bc", "xc"))

  # Positions are an alternative spelling of the same anchors.
  expect_identical(place(before = c(xc = 1L)), place(before = c(xc = "ac")))
  expect_identical(place(after = c(xc = 2L)), place(after = c(xc = "bc")))

  # Several links anchored to the same one keep the order they arrive in.
  expect_identical(
    names(
      board_links(
        modify_board_links(
          board,
          add = links(xc = new_link("a", "c"), yc = new_link("b", "c")),
          before = c(xc = "bc", yc = "bc")
        )
      )
    ),
    c("ac", "xc", "yc", "bc")
  )
})

test_that("splicing a block into a link preserves variadic input order", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      z = new_dataset_block("BOD"),
      b = new_head_block(),
      m = new_rbind_block()
    ),
    links = links(
      ac = new_link("a", "m"),
      zm = new_link("z", "m")
    )
  )

  sources <- function(x) {
    lnk <- board_links(x)
    field(lnk[field(lnk, "to") == "m"], "from")
  }

  expect_identical(sources(board), c("a", "z"))

  splice <- function(...) {
    modify_board_links(
      board,
      add = links(ab = new_link("a", "b", "data"), bm = new_link("b", "m")),
      rm = "ac",
      ...
    )
  }

  # Appending is what re-orders the target block's inputs.
  expect_identical(sources(splice()), c("z", "b"))

  # The link being removed is a legitimate anchor: it is still there when
  # anchors are resolved.
  expect_identical(sources(splice(after = c(bm = "ac"))), c("b", "z"))
  expect_identical(sources(splice(before = c(bm = "ac"))), c("b", "z"))
})

test_that("link placement rejects contradictory or unknown anchors", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("BOD"),
      c = new_rbind_block()
    ),
    links = links(ac = new_link("a", "c"), bc = new_link("b", "c"))
  )

  add <- links(xc = new_link("a", "c"))

  expect_error(
    modify_board_links(
      board, add = add, before = c(xc = "ac"), after = c(xc = "bc")
    ),
    class = "links_insert_position_clash"
  )

  expect_error(
    modify_board_links(board, add = add, after = c(bc = "ac")),
    class = "links_insert_names_invalid"
  )

  expect_error(
    modify_board_links(board, add = add, after = "ac"),
    class = "links_insert_names_invalid"
  )

  expect_error(
    modify_board_links(board, add = add, after = c(xc = "nope")),
    class = "vctrs_error_subscript_oob"
  )

  expect_error(
    modify_board_links(board, add = add, after = c(xc = 5L)),
    class = "vctrs_error_subscript_oob"
  )
})

test_that("link removal is applied before the add + rm overlap assignment", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      z = new_dataset_block("BOD"),
      h = new_head_block(),
      k = new_head_block()
    ),
    links = links(
      old = new_link("a", "h", "data"),
      keepme = new_link("z", "k", "data")
    )
  )

  # Here `keepme` claims the input `old` currently holds; the payload removes
  # `old` in the same call, so the end state is valid.
  edited <- modify_board_links(
    board,
    add = links(keepme = new_link("z", "h", "data")),
    rm = c("old", "keepme")
  )

  expect_identical(names(board_links(edited)), "keepme")
  expect_identical(board_links(edited)[["keepme"]][["to"]], "h")
})
