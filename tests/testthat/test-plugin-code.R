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
    get_s3_method("board_server", board),
    {
      code_gen <- session$makeScope("generate_code")
      code_gen$setInputs(code_mod = 1)

      session$flushReact()

      res <- code_gen$output$code_out

      expect_type(res, "character")
      expect_length(res, 1L)
    },
    args = list(
      x = board,
      plugins = list(generate_code())
    )
  )
})

test_that("gen_code return validation", {
  with_mock_session(
    {
      check_gen_code_val(list(a = 1))
      sink_msg(
        expect_warning(
          session$flushReact(),
          "Expecting `generate_code` to return `NULL`"
        )
      )
    }
  )
})

test_that("dummy add/rm block ui test", {
  expect_s3_class(generate_code_ui("gen", new_board()), "shiny.tag.list")
})
