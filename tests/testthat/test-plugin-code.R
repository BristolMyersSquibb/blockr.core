test_that("generate code", {

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

  testServer(
    generate_code_server,
    {
      res <- code()

      expect_type(res, "character")
      expect_length(res, 1L)

      session$setInputs(code_mod = 1)
    },
    args = generate_plugin_args(board)
  )
})

test_that("code export is gated on all blocks being ready", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      c = new_merge_block(by = "Time")
    )
  )

  testServer(
    generate_code_server,
    {
      expect_false(code_export_ready(board))
      expect_null(code())

      session$setInputs(code_mod = 1)
    },
    args = generate_plugin_args(board)
  )
})

test_that("code_export_ready settles on ready or dormant", {

  reactiveConsole(TRUE)
  on.exit(reactiveConsole(FALSE))

  board <- new_board(
    blocks = c(a = new_dataset_block("BOD"), b = new_dataset_block("BOD"))
  )

  make_rv <- function(...) {
    list(eval = do.call(reactiveValues, list(...)), board = board)
  }

  expect_true(
    code_export_ready(make_rv(a = reactive("ready"), b = reactive("ready")))
  )
  expect_true(
    code_export_ready(make_rv(a = reactive("ready"), b = reactive("dormant")))
  )

  expect_false(
    code_export_ready(make_rv(a = reactive("ready"), b = reactive("waiting")))
  )
  expect_false(
    code_export_ready(make_rv(a = reactive("ready"), b = reactive("unset")))
  )
  expect_false(
    code_export_ready(make_rv(a = reactive("ready"), b = reactive("failed")))
  )

  expect_false(
    code_export_ready(make_rv(a = reactive("ready")))
  )
})

test_that("code modal body shows script or a not-ready note", {

  note <- code_modal_body(NULL, "out")

  expect_s3_class(note, "shiny.tag")
  expect_match(as.character(note), "not ready")

  body <- code_modal_body("y <- 1", "out")

  expect_match(as.character(body), "<pre>")
  expect_match(as.character(body), "y &lt;- 1")
})

test_that("dummy add/rm block ui test", {
  expect_s3_class(generate_code_ui("gen", new_board()), "shiny.tag.list")
})
