test_that("generate code", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("BOD"),
      c = new_merge_block(by = "Time")
    ),
    links = links(
      from = c("a", "b"),
      to = c("c", "c"),
      input = c("x", "y")
    )
  )

  testServer(
    generate_code_server,
    {
      res <- code()

      expect_type(res, "character")
      expect_length(res, 1L)
    },
    args = generate_plugin_args(board)
  )
})

test_that("dummy add/rm block ui test", {
  expect_s3_class(generate_code_ui("gen", new_board()), "shiny.tag.list")
})
