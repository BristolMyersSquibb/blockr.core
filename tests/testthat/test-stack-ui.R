test_that("dummy stack ui tests", {

  x <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("ChickWeight"),
      c = new_merge_block("Time")
    ),
    links = c(
      ac = new_link("a", "c", "x"),
      bc = new_link("b", "c", "y")
    ),
    stacks = list(ac = c("a", "c"))
  )

  expect_s3_class(
    stack_ui("stacks", x),
    "shiny.tag.list"
  )

  plugins <- board_plugins(new_board())

  expect_s3_class(
    stack_ui("stacks", x, "ac", plugins[["edit_stack"]]),
    "shiny.tag.list"
  )

  expect_null(
    insert_stack_ui(
      "board",
      board_stacks(x),
      x,
      session = MockShinySession$new()
    )
  )

  expect_null(
    insert_stack_ui(
      "board",
      board_stacks(x),
      x,
      plugins[["edit_stack"]],
      MockShinySession$new()
    )
  )

  expect_null(
    remove_stack_ui("board", x, MockShinySession$new())
  )

  expect_null(
    add_block_to_stack(x, "a", "a", MockShinySession$new())
  )

  expect_null(
    remove_block_from_stack(x, "a", "board", MockShinySession$new())
  )
})

test_that("the accordion is board-namespaced and names its panels", {

  x <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("ChickWeight")
    ),
    stacks = list(s1 = "a", s2 = "b")
  )

  acc <- stack_ui("brd", x)[[1L]]

  # Container ID and panel values are what gate_stacks() reads back.
  expect_identical(acc$attribs$id, "brd-stacks")

  items <- htmltools::tagQuery(acc)$find(".accordion-item")$selectedTags()

  expect_identical(
    chr_xtr(lst_xtr(items, "attribs"), "data-value"),
    c("brd-stack_s1", "brd-stack_s2")
  )
})
