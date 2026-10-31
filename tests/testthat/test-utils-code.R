test_that("code generation", {

  board <- new_board(
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

  code_exp <- export_code(
    list(
      a = quote(datasets::BOD),
      b = quote(datasets::ChickWeight),
      c = quote(merge(x, y, by = "Time"))
    ),
    board
  )

  expect_type(code_exp, "list")
  expect_length(code_exp, 3L)
  expect_named(code_exp, c("exprs", "args", "types"))

  expect_named(code_exp[["exprs"]], c("a", "b", "c"), ignore.order = TRUE)
  expect_named(code_exp[["args"]], names(code_exp[["exprs"]]))
  expect_named(code_exp[["types"]], names(code_exp[["exprs"]]))
})
