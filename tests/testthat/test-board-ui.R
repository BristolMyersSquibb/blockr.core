test_that("dummy board ui test", {

  expect_s3_class(board_ui("board", new_board()), "shiny.tag.list")

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(from = "a", to = "b")
  )

  expect_s3_class(board_ui("board", board), "shiny.tag.list")
})

painted_block_ids <- function(ui) {

  cards <- htmltools::tagQuery(ui)$find("#board_blocks")$children()

  sub("_block$", "", unlst(lst_xtr(cards$selectedTags(), "attribs", "id")))
}

test_that("board_ui paints the blocks initial_block_ids declares", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block(),
      c = new_head_block()
    ),
    links = links(from = "a", to = "b")
  )

  expect_setequal(initial_block_ids(board), c("a", "b", "c"))

  expect_setequal(
    painted_block_ids(board_ui("board", board)),
    c("a", "b", "c")
  )

  registerS3method(
    "initial_block_ids", "test_partial_board",
    function(x, ...) c("a", "c"),
    envir = asNamespace("blockr.core")
  )

  partial <- new_board(
    blocks = board_blocks(board),
    links = board_links(board),
    class = "test_partial_board"
  )

  expect_setequal(
    painted_block_ids(board_ui("board", partial)),
    c("a", "c")
  )
})
