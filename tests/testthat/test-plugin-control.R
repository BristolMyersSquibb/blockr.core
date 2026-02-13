test_that("ctrl_block constructor", {

  plg <- ctrl_block()

  expect_s3_class(plg, "ctrl_block")
  expect_s3_class(plg, "plugin")

  expect_identical(plg$server, ctrl_block_server)
  expect_identical(plg$ui, ctrl_block_ui)
})

test_that("ctrl_block_ui generates text inputs for external ctrl fields", {

  blk <- new_dataset_block("mtcars")
  ui <- ctrl_block_ui("ctrl", blk)

  expect_s3_class(ui, "shiny.tag.list")
  expect_length(ui, 1L)

  html <- as.character(ui[[1L]])
  expect_match(html, "ctrl-dataset")
  expect_match(html, "Dataset")

  blk2 <- new_subset_block()
  ui2 <- ctrl_block_ui("ctrl", blk2)

  expect_s3_class(ui2, "shiny.tag.list")
  expect_length(ui2, 2L)

  html2 <- paste(vapply(ui2, as.character, character(1L)), collapse = "")
  expect_match(html2, "ctrl-subset")
  expect_match(html2, "ctrl-select")
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

test_that("ctrl_block_server syncs input to reactive state", {

  blk <- new_dataset_block("mtcars")

  testServer(
    ctrl_block_server,
    {
      session$flushReact()

      expect_equal(vars$dataset(), "mtcars")

      session$setInputs(dataset = "iris")

      expect_equal(vars$dataset(), "iris")
    },
    args = list(
      x = blk,
      vars = list(dataset = reactiveVal("mtcars")),
      dat = reactive(NULL),
      expr = reactive(NULL)
    )
  )
})

test_that("ctrl_block_server syncs multiple inputs", {

  blk <- new_subset_block(subset = "cyl > 4", select = "mpg")

  testServer(
    ctrl_block_server,
    {
      session$flushReact()

      expect_equal(vars$subset(), "cyl > 4")
      expect_equal(vars$select(), "mpg")

      session$setInputs(subset = "cyl > 6")
      expect_equal(vars$subset(), "cyl > 6")
      expect_equal(vars$select(), "mpg")

      session$setInputs(select = "disp")
      expect_equal(vars$select(), "disp")
    },
    args = list(
      x = blk,
      vars = list(
        subset = reactiveVal("cyl > 4"),
        select = reactiveVal("mpg")
      ),
      dat = reactive(NULL),
      expr = reactive(NULL)
    )
  )
})

test_that("ctrl_block_server returns TRUE", {

  blk <- new_dataset_block("mtcars")

  testServer(
    ctrl_block_server,
    {
      session$flushReact()
      expect_true(session$returned)
    },
    args = list(
      x = blk,
      vars = list(dataset = reactiveVal("mtcars")),
      dat = reactive(NULL),
      expr = reactive(NULL)
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
      expect_equal(vars$dataset(), "mtcars")
    },
    args = list(
      x = blk,
      vars = list(dataset = reactiveVal("mtcars")),
      dat = reactive(NULL),
      expr = reactive(NULL)
    )
  )
})

test_that("validate_ctrl accepts TRUE and reactives", {

  expect_invisible(validate_ctrl(TRUE))
  expect_invisible(validate_ctrl(reactive(NULL)))

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

      expect_identical(session$returned$result(), datasets::iris)
    },
    args = list(x = blk, ctrl_block = ctrl_block())
  )
})
