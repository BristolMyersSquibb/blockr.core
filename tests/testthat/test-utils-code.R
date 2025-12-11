test_that("code generation", {

  board <- generate_plugin_args(
    new_board(
      blocks = c(
        a = new_dataset_block("BOD"),
        b = new_dataset_block("ChickWeight"),
        c = new_merge_block(by = "Time")
      ),
      links = links(
        from = c("a", "b"),
        to = c("c", "c"),
        input = c("x", "y")
      )
    )
  )$board

  code_exp <- export_code(
    isolate(lst_xtr_reval(board$blocks, "server", "expr")),
    isolate(board$board)
  )

  expect_type(code_exp, "list")
  expect_length(code_exp, 3L)
  expect_named(code_exp, c("exprs", "args", "types"))

  expect_named(code_exp[["exprs"]], c("a", "b", "c"), ignore.order = TRUE)
  expect_named(code_exp[["args"]], names(code_exp[["exprs"]]))
  expect_named(code_exp[["types"]], names(code_exp[["exprs"]]))

  code_str <- export_wrapped_code(
    isolate(lst_xtr_reval(board$blocks, "server", "expr")),
    isolate(board$board)
  )

  expect_type(code_str, "character")
  expect_length(code_str, 1L)
})
