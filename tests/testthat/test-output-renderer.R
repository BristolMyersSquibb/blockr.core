test_that("core's families are drawn by its plain renderers", {

  families <- list(
    table = table_output,
    text = text_output,
    plot = plot_output
  )

  for (what in names(families)) {

    # A family's renderer, which R knows before any output exists, draws the
    # outputs of its class
    cls <- paste0(what, "_output")
    drawn <- family_renderers(families[[what]])[[cls]]

    expect_identical(
      drawing_renderer(families[[what]], new_block_output(list(), cls)),
      drawn
    )

    expect_s3_class(drawn, "renderer")
    expect_identical(drawn$id, paste0("blockr.core-", what, "_output"))
    expect_identical(drawn$package, "blockr.core")
    expect_identical(
      drawn$module,
      test_module(paste0("js/basic-", what, ".js"), "blockr.core")
    )
    expect_null(drawn$option)
    expect_identical(drawn$requests, list())
  }
})

test_that("a renderer holds its ID, its module and its own requests", {

  drawn <- new_renderer(
    "preview",
    test_module("preview.js", "blockr.ui"),
    requests = list(labels = function(result) names(result))
  )

  expect_identical(drawn$id, "blockr.ui-preview")
  expect_identical(drawn$name, "preview")
  expect_identical(drawn$package, "blockr.ui")
  expect_named(drawn$requests, "labels")

  rows <- new_renderer("rows", test_module("rows.js"), option = "n_rows")

  expect_identical(rows$option, "n_rows")

  expect_snapshot({
    drawn
    rows
  })
})

test_that("another package's renderer draws where the deployment names it", {

  table <- table_output(datasets::BOD, options = list(n_rows = 2L))
  preview <- new_renderer("preview", test_module("preview.js", "blockr.ui"))

  expect_identical(
    drawing_renderer(table_output, table)$id,
    "blockr.core-table_output"
  )

  withr::local_options(blockr.renderers = list(table_output = preview))

  expect_identical(drawing_renderer(table_output, table), preview)
  expect_identical(
    family_renderers(table_output),
    list(table_output = preview)
  )
  expect_identical(
    family_renderers(text_output)$text_output$id,
    "blockr.core-text_output"
  )
})

test_that("an output is drawn by the renderer of its class's family", {

  words <- new_renderer("words", test_module("words.js"))
  preview <- new_renderer("preview", test_module("preview.js", "blockr.ui"))

  # A family that shows a result as words, and hands a table to table_output
  words_output <- new_output_family(
    function(result, ...) {
      if (is.data.frame(result)) {
        return(table_output(result, ...))
      }
      new_block_output(list(words = as.character(result)), "words_output")
    },
    "words_output",
    renderer = words,
    families = table_output
  )

  expect_named(
    family_renderers(words_output),
    c("words_output", "table_output")
  )

  tbl <- words_output(datasets::BOD, options = list(n_rows = 2L))

  expect_identical(
    drawing_renderer(words_output, tbl)$id,
    "blockr.core-table_output"
  )
  expect_identical(drawing_renderer(words_output, words_output("hi")), words)

  # A subclass of an output class is drawn as its class is, by the
  # deployment's choice for its family where it names one
  lazy <- new_block_output(list(), c("lazy_table_output", "table_output"))

  withr::local_options(blockr.renderers = list(table_output = preview))

  expect_identical(drawing_renderer(words_output, lazy), preview)
  expect_identical(drawing_renderer(words_output, words_output("hi")), words)

  expect_error(
    drawing_renderer(words_output, new_block_output(list(), "chart_output")),
    "neither it nor a family that it names",
    class = "output_renderer_missing"
  )
})

test_that("a family is drawn by a renderer of outputs", {

  rows <- new_renderer("rows", test_module("rows.js"), option = "n_rows")

  expect_error(validate_renderer(rows, table_output),
               class = "renderer_invalid")
  expect_error(validate_renderer("rows.js", table_output),
               class = "renderer_invalid")
})

test_that("the blockr option names renderers for what they draw", {

  check <- function(value, regexp = NULL) {
    withr::local_options(blockr.renderers = value)
    expect_error(
      chosen_renderers(),
      class = "renderers_blockr_option_invalid",
      regexp = regexp
    )
  }

  rows <- new_renderer("rows", test_module("rows.js"), option = "n_rows")

  check("blockr.core-table_output")
  check(c(table_output = "blockr.core-table_output"))
  check(list(table_output = "blockr.core-table_output"))
  check(list(rows))
  check(list(dark_mode = rows), "draws board option n_rows")
})

test_that("a board option keeps its UI unless the app names a renderer", {

  expect_identical(option_renderers(), set_names(list(), character()))

  rows <- new_renderer("rows", test_module("rows.js", "blockr.ui"),
                       option = "n_rows")
  preview <- new_renderer("preview", test_module("preview.js", "blockr.ui"))

  withr::local_options(
    blockr.renderers = list(n_rows = rows, table_output = preview)
  )

  expect_identical(option_renderers(), list(`blockr.ui-rows` = rows))
})

test_that("a request is the drawing renderer's own or else its class's", {

  local_output_class(
    "base_output",
    requests = list(
      size = function(result) length(result),
      label = function(result) "base"
    )
  )

  labels <- new_renderer(
    "labels",
    test_module("labels.js"),
    requests = list(
      label = function(result, params) paste("labels", params$page),
      first = function(result) result[1L]
    )
  )

  out <- new_block_output(list(), c("sub_output", "base_output"))
  run <- function(name, ...) {
    run_output_class_request(out, name, letters, ..., renderer = labels)
  }

  expect_identical(run("label", params = list(page = 2L)), "labels 2")
  expect_identical(run("first"), "a")

  expect_identical(run_output_class_request(out, "label", letters), "base")
  expect_identical(run_output_class_request(out, "size", letters), 26L)

  expect_error(
    run_output_class_request(out, "first", letters),
    class = "output_class_request_unknown"
  )
  expect_error(
    run("size"),
    class = "renderer_request_unknown",
    regexp = "Renderer blockr.test-labels has no request size"
  )
  expect_error(
    run_output_class_request(out, "first", letters,
                             renderer = "blockr.test-labels"),
    class = "output_class_request_unknown"
  )
})

test_that("a renderer's ID is its name in the package of its module", {

  make <- function() new_renderer("pkg", js_module("pkg.js"))

  environment(make) <- asNamespace("blockr.core")

  drawn <- make()

  expect_identical(drawn$id, "blockr.core-pkg")
  expect_identical(drawn$package, "blockr.core")

  # A renderer beside an app script comes from no package
  script <- new_renderer("chart", js_module("chart.js", package = NULL))

  expect_identical(script$id, "chart")
  expect_null(script$package)
})

test_that("a renderer's construction is checked", {

  mod <- test_module("test.js")

  expect_error(new_renderer("test", "test.js"), class = "renderer_invalid")
  expect_error(new_renderer("", mod), class = "renderer_invalid")
  expect_error(new_renderer("test", mod, option = ""),
               class = "renderer_invalid")
  expect_error(new_renderer("test", mod, requests = list(function() 1)),
               class = "renderer_requests_invalid")
  expect_error(new_renderer("test", mod, requests = list(a = 1)),
               class = "renderer_requests_invalid")
  expect_error(
    new_renderer("test", mod, requests = list(a = function() 1),
                 option = "n_rows"),
    class = "renderer_requests_invalid",
    regexp = "board option"
  )
})
