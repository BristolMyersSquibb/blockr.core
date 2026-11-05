test_that("core's output classes are drawn by its plain renderers", {

  for (what in c("table", "text", "plot")) {

    out <- new_block_output(list(), paste0(what, "_output"))
    drawn <- output_renderer(out)

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

test_that("another package's renderer draws only where the app names it", {

  table <- new_block_output(list(), "table_output")
  text <- new_block_output(list(), "text_output")
  preview <- new_renderer("preview", test_module("preview.js", "blockr.ui"))

  expect_identical(output_renderer(table)$id, "blockr.core-table_output")
  expect_identical(chosen_renderer("table_output"), NULL)
  expect_identical(chosen_renderer("table_output", preview), preview)

  withr::local_options(blockr.renderers = list(table_output = preview))

  expect_identical(output_renderer(table), preview)
  expect_identical(output_renderer(text)$id, "blockr.core-text_output")
  expect_identical(chosen_renderer("table_output"), preview)

  expect_error(chosen_renderer(""), class = "renderer_invalid")
})

test_that("a subclass is drawn by its class's renderer unless it has one", {

  lazy <- new_block_output(list(), c("lazy_table_output", "table_output"))
  preview <- new_renderer("preview", test_module("preview.js", "blockr.ui"))

  expect_identical(output_renderer(lazy)$id, "blockr.core-table_output")

  withr::local_options(blockr.renderers = list(table_output = preview))

  expect_identical(output_renderer(lazy), preview)

  # A method of the subclass's own takes the app's choice for the subclass
  local_output_class(
    "lazy_table_output",
    renderer = function(x, ...) {
      chosen_renderer(
        "lazy_table_output",
        new_renderer("lazy_table", test_module("lazy.js"))
      )
    }
  )

  expect_identical(output_renderer(lazy)$id, "blockr.test-lazy_table")
  expect_identical(
    output_renderer(new_block_output(list(), "table_output")),
    preview
  )

  withr::local_options(blockr.renderers = list(lazy_table_output = preview))

  expect_identical(output_renderer(lazy), preview)

  expect_error(
    output_renderer(new_block_output(list(), "words_output")),
    class = "output_renderer_missing"
  )
})

test_that("an output is drawn by what its class's method gives", {

  local_output_class("x_output", renderer = "x.js")

  expect_error(
    renderer_of(new_block_output(list(), "x_output")),
    class = "renderer_invalid"
  )

  local_output_class(
    "x_output",
    renderer = new_renderer("x", test_module("x.js"), option = "n_rows")
  )

  expect_error(
    renderer_of(new_block_output(list(), "x_output")),
    class = "renderer_invalid"
  )
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
    ),
    renderer = new_renderer(
      "labels",
      test_module("labels.js"),
      requests = list(
        label = function(result, params) paste("labels", params$page),
        first = function(result) result[1L]
      )
    )
  )

  out <- new_block_output(list(), c("sub_output", "base_output"))
  run <- function(name, ...) {
    run_output_class_request(out, name, letters, ...,
                             renderer = "blockr.test-labels")
  }

  expect_identical(run("label", params = list(page = 2L)), "labels 2")
  expect_identical(run("first"), "a")

  expect_identical(run_output_class_request(out, "label", letters), "base")
  expect_identical(run_output_class_request(out, "size", letters), 26L)

  expect_false(
    output_class_request(class(out), "first", "blockr.test-labels")$volatile
  )

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
    run_output_class_request("text_output", "first", letters,
                             renderer = "blockr.test-labels"),
    class = "renderer_class_mismatch"
  )
  expect_error(
    run_output_class_request(out, "first", letters, renderer = 1),
    class = "output_class_request_unknown"
  )
  expect_error(
    run_output_class_request("table_output", "first", letters,
                             renderer = "blockr.core-table_output"),
    class = "renderer_request_unknown"
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
