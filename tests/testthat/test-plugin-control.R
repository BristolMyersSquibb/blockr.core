test_that("ctrl_block constructor", {

  plg <- ctrl_block()

  expect_s3_class(plg, "ctrl_block")
  expect_s3_class(plg, "plugin")

  expect_identical(plg$server, ctrl_block_server)
  expect_identical(plg$ui, ctrl_block_ui)
})

test_that("ctrl_block_ui generates text inputs and submit button", {

  blk <- new_dataset_block("mtcars")
  ui <- ctrl_block_ui("ctrl", blk)

  expect_s3_class(ui, "shiny.tag.list")
  expect_length(ui, 2L)

  html <- paste(vapply(ui, as.character, character(1L)), collapse = "")
  expect_match(html, "ctrl-dataset")
  expect_match(html, "Dataset")
  expect_match(html, "ctrl-submit")
  expect_match(html, "Submit")

  blk2 <- new_subset_block()
  ui2 <- ctrl_block_ui("ctrl", blk2)

  expect_s3_class(ui2, "shiny.tag.list")
  expect_length(ui2, 3L)

  html2 <- paste(vapply(ui2, as.character, character(1L)), collapse = "")
  expect_match(html2, "ctrl-subset")
  expect_match(html2, "ctrl-select")
  expect_match(html2, "ctrl-submit")
})

test_that("ctrl_block_ui is empty for blocks without external ctrl", {

  blk <- new_dataset_block("mtcars")
  attr(blk, "external_ctrl") <- FALSE

  ui <- ctrl_block_ui("ctrl", blk)
  expect_s3_class(ui, "shiny.tag.list")
  expect_length(ui, 0L)
})

test_that("block_supports_external_ctrl correctly distinguishes blocks", {

  blk_with <- new_dataset_block("mtcars")
  expect_true(block_supports_external_ctrl(blk_with))

  blk_without <- new_dataset_block("mtcars")
  attr(blk_without, "external_ctrl") <- FALSE
  expect_false(block_supports_external_ctrl(blk_without))
})

test_that("ctrl_block_server syncs input to reactive state on submit", {

  blk <- new_dataset_block("mtcars")
  dataset_rv <- reactiveVal("mtcars")

  testServer(
    ctrl_block_server,
    {
      session$flushReact()

      expect_equal(vars$dataset(), "mtcars")

      session$setInputs(dataset = "iris")
      expect_equal(vars$dataset(), "mtcars")

      session$setInputs(submit = 1L)
      expect_equal(vars$dataset(), "iris")
    },
    args = list(
      x = blk,
      vars = list(dataset = dataset_rv),
      dat = reactive(list()),
      expr = reactive(quote(TRUE))
    )
  )
})

test_that("ctrl_block_server syncs multiple inputs on submit", {

  blk <- new_subset_block(subset = "cyl > 4", select = "mpg")
  subset_rv <- reactiveVal("cyl > 4")
  select_rv <- reactiveVal("mpg")

  testServer(
    ctrl_block_server,
    {
      session$flushReact()

      expect_equal(vars$subset(), "cyl > 4")
      expect_equal(vars$select(), "mpg")

      session$setInputs(subset = "cyl > 6", select = "disp")
      expect_equal(vars$subset(), "cyl > 4")
      expect_equal(vars$select(), "mpg")

      session$setInputs(submit = 1L)
      expect_equal(vars$subset(), "cyl > 6")
      expect_equal(vars$select(), "disp")
    },
    args = list(
      x = blk,
      vars = list(subset = subset_rv, select = select_rv),
      dat = reactive(list()),
      expr = reactive(quote(TRUE))
    )
  )
})

test_that("ctrl_block_server returns a reactive gate", {

  blk <- new_dataset_block("mtcars")

  testServer(
    ctrl_block_server,
    {
      session$flushReact()
      expect_true(is.reactive(session$returned))
      expect_true(session$returned())
    },
    args = list(
      x = blk,
      vars = list(dataset = reactiveVal("mtcars")),
      dat = reactive(list()),
      expr = reactive(quote(TRUE))
    )
  )
})

test_that("ctrl_block_server does not update when input matches state", {

  blk <- new_dataset_block("mtcars")

  testServer(
    ctrl_block_server,
    {
      session$flushReact()

      session$setInputs(dataset = "mtcars")
      session$setInputs(submit = 1L)
      expect_equal(vars$dataset(), "mtcars")
    },
    args = list(
      x = blk,
      vars = list(dataset = reactiveVal("mtcars")),
      dat = reactive(list()),
      expr = reactive(quote(TRUE))
    )
  )
})

test_that("ctrl_block_server reverts vars and blocks gate on eval error", {

  blk <- new_dataset_block("mtcars")
  dataset_rv <- reactiveVal("mtcars")
  fail <- reactiveVal(FALSE)

  testServer(
    ctrl_block_server,
    {
      session$flushReact()

      expect_true(session$returned())
      expect_equal(vars$dataset(), "mtcars")

      fail(TRUE)
      session$setInputs(dataset = "bad_value")
      session$setInputs(submit = 1L)

      expect_equal(vars$dataset(), "mtcars")
      expect_false(session$returned())
    },
    args = list(
      x = blk,
      vars = list(dataset = dataset_rv),
      dat = reactive(list()),
      expr = reactive(if (fail()) stop("eval failed") else quote(TRUE))
    )
  )
})

test_that("ctrl_block_server recovers gate after successful submit", {

  blk <- new_dataset_block("mtcars")
  dataset_rv <- reactiveVal("mtcars")
  fail <- reactiveVal(FALSE)

  testServer(
    ctrl_block_server,
    {
      session$flushReact()

      fail(TRUE)
      session$setInputs(dataset = "bad")
      session$setInputs(submit = 1L)

      expect_false(session$returned())
      expect_equal(vars$dataset(), "mtcars")

      fail(FALSE)
      session$setInputs(dataset = "iris")
      session$setInputs(submit = 2L)

      expect_true(session$returned())
      expect_equal(vars$dataset(), "iris")
    },
    args = list(
      x = blk,
      vars = list(dataset = dataset_rv),
      dat = reactive(list()),
      expr = reactive(if (fail()) stop("eval failed") else quote(TRUE))
    )
  )
})

test_that("validate_ctrl accepts TRUE and reactives", {

  expect_invisible(validate_ctrl(TRUE))
  expect_invisible(validate_ctrl(reactive(NULL)))
  expect_invisible(validate_ctrl(reactiveVal(TRUE)))

  expect_error(validate_ctrl(FALSE), class = "expect_true_or_rv")
  expect_error(validate_ctrl("abc"), class = "expect_true_or_rv")
  expect_error(validate_ctrl(42), class = "expect_true_or_rv")
})

test_that("ctrl_block integrates with block_server", {

  blk <- new_dataset_block("mtcars")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      expect_identical(session$returned$result(), datasets::mtcars)

      session$makeScope("ctrl_block")$setInputs(dataset = "iris")
      session$makeScope("ctrl_block")$setInputs(submit = 1L)

      expect_identical(session$returned$result(), datasets::iris)
    },
    args = list(x = blk, ctrl_block = ctrl_block())
  )
})
