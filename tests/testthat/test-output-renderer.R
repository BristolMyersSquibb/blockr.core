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
    expect_identical(drawn$name, cls)
    expect_identical(drawn$package, "blockr.core")
    expect_identical(
      drawn$module,
      test_module(paste0("js/basic-", what, ".js"), "blockr.core")
    )
    expect_identical(drawn$requests, list())
  }
})

test_that("a renderer holds its name, its module and its own requests", {

  drawn <- new_renderer(
    "preview",
    test_module("preview.js", "blockr.ui"),
    requests = list(labels = function(result) names(result))
  )

  expect_identical(drawn$name, "preview")
  expect_identical(drawn$package, "blockr.ui")
  expect_identical(
    renderer_id(drawn),
    list(package = "blockr.ui", id = "preview")
  )
  expect_named(drawn$requests, "labels")

  # What it draws is where the app or a family names it, not a member of its
  # own, and it holds no id that joins its package and its name
  expect_named(drawn, c("name", "package", "module", "requests"))

  expect_snapshot(drawn)
})

test_that("another package's renderer draws where the deployment names it", {

  table <- table_output(datasets::BOD, options = list(n_rows = 2L))
  preview <- new_renderer("preview", test_module("preview.js", "blockr.ui"))

  expect_identical(
    renderer_labels(list(drawing_renderer(table_output, table))),
    "table_output of blockr.core"
  )

  withr::local_options(blockr.renderers = list(table_output = preview))

  expect_identical(drawing_renderer(table_output, table), preview)
  expect_identical(
    family_renderers(table_output),
    list(table_output = preview)
  )
  expect_identical(
    renderer_labels(family_renderers(text_output)),
    "text_output of blockr.core"
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
    renderer_labels(list(drawing_renderer(words_output, tbl))),
    "table_output of blockr.core"
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

test_that("an open family's outputs are drawn by the listed families", {

  words <- new_renderer("words", test_module("words.js"))
  preview <- new_renderer("preview", test_module("preview.js", "blockr.ui"))

  words_output <- new_output_family(
    function(result, ...) {
      new_block_output(list(words = as.character(result)), "words_output")
    },
    "words_output",
    renderer = words
  )

  # A family that hands text to words_output and the rest to table_output,
  # both of which the available packages list
  any_output <- new_output_family(
    function(result, ...) {
      if (is.character(result)) {
        return(words_output(result, ...))
      }
      table_output(result, ...)
    },
    "any_output",
    families = TRUE
  )

  listed <- list(words_output = words_output, table_output = table_output)

  expect_named(
    family_renderers(any_output, listed),
    c("words_output", "table_output")
  )

  tbl <- any_output(datasets::BOD, options = list(n_rows = 2L))

  expect_identical(drawing_renderer(any_output, any_output("hi"), listed),
                   words)
  expect_identical(
    renderer_labels(list(drawing_renderer(any_output, tbl, listed))),
    "table_output of blockr.core"
  )

  # The deployment's choice for a listed family comes before its own
  withr::local_options(blockr.renderers = list(table_output = preview))

  expect_identical(drawing_renderer(any_output, tbl, listed), preview)

  # An output of a class that no listed family draws is an error
  expect_error(
    drawing_renderer(any_output, any_output("hi"), listed["table_output"]),
    "neither it nor a family that the available packages list draws",
    class = "output_renderer_missing"
  )
})

test_that("the blockr option names renderers for what they draw", {

  check <- function(value) {
    withr::local_options(blockr.renderers = value)
    expect_error(chosen_renderers(), class = "renderers_blockr_option_invalid")
  }

  rows <- new_renderer("rows", test_module("rows.js"))

  check("table_output")
  check(c(table_output = "table_output"))
  check(list(table_output = list(package = "blockr.core", id = "table_output")))
  check(list(rows))
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
    regexp = "Renderer labels of blockr.test has no request size"
  )
  expect_error(
    run_output_class_request(out, "first", letters,
                             renderer = renderer_id(labels)),
    class = "output_class_request_unknown"
  )
})

test_that("a renderer is its name in the package of its module", {

  make <- function() new_renderer("pkg", js_module("pkg.js"))

  environment(make) <- asNamespace("blockr.core")

  drawn <- make()

  expect_identical(
    renderer_id(drawn),
    list(package = "blockr.core", id = "pkg")
  )

  # A renderer beside an app script comes from no package
  script <- new_renderer("chart", js_module("chart.js", package = NULL))

  expect_identical(renderer_id(script), list(package = NULL, id = "chart"))
})

test_that("a renderer's construction is checked", {

  mod <- test_module("test.js")

  expect_error(new_renderer("test", "test.js"), class = "renderer_invalid")
  expect_error(new_renderer("", mod), class = "renderer_invalid")
  expect_error(new_renderer("test", mod, requests = list(function() 1)),
               class = "renderer_requests_invalid")
  expect_error(new_renderer("test", mod, requests = list(a = 1)),
               class = "renderer_requests_invalid")
})
