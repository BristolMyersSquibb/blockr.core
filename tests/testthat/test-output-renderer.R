test_that("core declares a plain renderer for each of its output classes", {

  for (what in c("table", "text", "plot")) {

    entry <- get_renderer(paste0("blockr.core-basic_", what))

    expect_identical(entry$class, paste0(what, "_output"))
    expect_null(entry$option)
    expect_identical(entry$package, "blockr.core")
    expect_identical(
      entry$module,
      test_module(paste0("js/basic-", what, ".js"), "blockr.core")
    )
    expect_identical(entry$requests, list())
  }
})

test_that("core's renderers are the defaults of its output classes", {

  picked <- pick_renderers()

  expect_setequal(names(picked), names(proto_renderers()))
  expect_identical(
    lst_xtr(picked, "class")[names(proto_renderers())],
    lst_xtr(proto_renderers(), "class")
  )
})

test_that("another package's renderer draws only where the app names it", {

  local_declared(declared_renderers)

  table_renderer <- function() {
    output_renderer("table_output", pick_renderers())
  }

  expect_identical(table_renderer(), "blockr.core-basic_table")

  register_renderer(
    "preview",
    test_module("preview.js", "blockr.ui"),
    class = "table_output"
  )
  register_renderer(
    "table",
    test_module("table.js", "blockr.viz"),
    class = "table_output"
  )

  expect_identical(table_renderer(), "blockr.core-basic_table")
  expect_false("blockr.ui-preview" %in% names(pick_renderers()))

  withr::local_options(blockr.renderers = c(table_output = "blockr.ui-preview"))

  expect_identical(table_renderer(), "blockr.ui-preview")

  withr::local_options(
    blockr.renderers = c(text_output = "blockr.core-basic_text")
  )

  expect_identical(table_renderer(), "blockr.core-basic_table")
})

test_that("two renderers of the defining package leave no default", {

  local_declared(declared_renderers)

  register_renderer(
    "fancy",
    test_module("fancy.js", "blockr.core"),
    class = "table_output"
  )

  expect_error(
    pick_renderers(),
    class = "renderers_ambiguous",
    regexp = "blockr.core-basic_table and blockr.core-fancy"
  )

  withr::local_options(
    blockr.renderers = c(table_output = "blockr.core-fancy")
  )

  expect_identical(
    output_renderer("table_output", pick_renderers()),
    "blockr.core-fancy"
  )
})

test_that("a board option keeps its UI unless the app names a renderer", {

  local_declared(declared_renderers)

  register_renderer("rows", test_module("rows.js", "blockr.ui"),
                    option = "n_rows")
  register_renderer("rows", test_module("rows.js"), option = "n_rows")

  expect_null(unlst(lst_xtr(pick_renderers(), "option")))

  withr::local_options(blockr.renderers = c(n_rows = "blockr.ui-rows"))

  picked <- pick_renderers()

  expect_identical(picked[["blockr.ui-rows"]], get_renderer("blockr.ui-rows"))
  expect_false("blockr.test-rows" %in% names(picked))
})

test_that("an output names the renderer of its most specific class", {

  local_declared(declared_renderers)

  withr::defer(unregister_output_classes("lazy_table_output"))

  register_output_class("lazy_table_output", list(), package = "blockr.test")

  register_renderer(
    "lazy",
    test_module("lazy.js"),
    class = "lazy_table_output"
  )

  picked <- pick_renderers()

  expect_identical(
    output_renderer(
      new_block_output(list(), c("lazy_table_output", "table_output")),
      picked
    ),
    "blockr.test-lazy"
  )

  expect_identical(
    output_renderer(
      c("other_table_output", "table_output", "block_output"),
      picked
    ),
    "blockr.core-basic_table"
  )

  expect_error(
    output_renderer(new_block_output(list(), "words_output"), picked),
    class = "output_renderer_missing"
  )
})

test_that("the blockr option names declared renderers for what they draw", {

  check <- function(value, regexp = NULL) {
    withr::local_options(blockr.renderers = value)
    expect_error(
      pick_renderers(),
      class = "renderers_blockr_option_invalid",
      regexp = regexp
    )
  }

  check("blockr.core-basic_table")
  check(list(table_output = "blockr.core-basic_table"))
  check(c(table_output = NA_character_))
  check(c(table_output = "blockr.none-table"), "blockr.none-table")
  check(c(text_output = "blockr.core-basic_table"), "does not draw")
  check(c(table_output = ""))
})

test_that("a request is the named renderer's own or else its class's", {

  local_declared(declared_renderers)

  withr::defer(unregister_output_classes(c("base_output", "sub_output")))

  register_output_class(
    "base_output",
    list(),
    requests = list(
      size = function(result) length(result),
      label = function(result) "base"
    ),
    package = NULL
  )

  register_output_class(
    "sub_output",
    list(),
    requests = list(label = function(result) "sub"),
    package = NULL
  )

  register_renderer(
    "labels",
    test_module("labels.js"),
    class = "base_output",
    requests = list(
      label = function(result, params) paste("labels", params$page),
      first = function(result) result[1L]
    )
  )

  out <- new_block_output(list(), c("sub_output", "base_output"))
  run <- function(name, ...) {
    run_output_class_request(out, name, letters, ...,
                             renderer = "blockr.test-labels")
  }

  expect_identical(run("label", params = list(page = 2L)), "labels 2")
  expect_identical(run("first"), "a")

  expect_identical(run_output_class_request(out, "label", letters), "sub")
  expect_identical(run_output_class_request(out, "size", letters), 26L)

  req <- output_class_request(class(out), "first", "blockr.test-labels")

  expect_identical(req$renderer, "blockr.test-labels")
  expect_false(req$volatile)

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
    run_output_class_request(out, "first", letters,
                             renderer = "blockr.test-none"),
    class = "renderer_missing"
  )
  expect_error(
    run_output_class_request(out, "first", letters, renderer = 1),
    class = "output_class_request_unknown"
  )
})

test_that("a renderer is declared by the package of its module", {

  local_declared(declared_renderers)

  declare <- function() {
    register_renderer("pkg", js_module("pkg.js"), class = "pkg_output")
  }

  environment(declare) <- asNamespace("blockr.core")

  entry <- declare()

  expect_identical(entry$id, "blockr.core-pkg")
  expect_identical(entry$package, "blockr.core")
  expect_identical(get_renderer("blockr.core-pkg"), entry)
  expect_true("blockr.core-pkg" %in% list_renderers())

  entry <- register_renderer(
    "pkg",
    test_module("pkg-again.js", "blockr.core"),
    option = "pkg_option"
  )

  expect_identical(get_renderer("blockr.core-pkg"), entry)
  expect_null(entry$class)
})

test_that("a renderer's declaration is checked", {

  local_declared(declared_renderers)

  mod <- test_module("test.js")

  expect_error(
    register_renderer("test", "test.js", class = "test_output"),
    class = "renderer_invalid"
  )
  expect_error(
    register_renderer("test", js_module("test.js", package = NULL),
                      class = "test_output"),
    class = "renderer_invalid",
    regexp = "declared by a package"
  )
  expect_error(
    register_renderer("", mod, class = "test_output"),
    class = "renderer_invalid"
  )
  expect_error(register_renderer("test", mod), class = "renderer_invalid")
  expect_error(
    register_renderer("test", mod, class = "test_output", option = "n_rows"),
    class = "renderer_invalid"
  )
  expect_error(
    register_renderer("test", mod, class = c("a_output", "b_output")),
    class = "renderer_invalid"
  )
  expect_error(
    register_renderer("test", mod, class = "block_output"),
    class = "renderer_invalid"
  )
  expect_error(
    register_renderer("test", mod, option = ""),
    class = "renderer_invalid"
  )
  expect_error(
    register_renderer("test", mod, class = "test_output",
                      requests = list(function() 1)),
    class = "renderer_requests_invalid"
  )
  expect_error(
    register_renderer("test", mod, class = "test_output",
                      requests = list(a = 1)),
    class = "renderer_requests_invalid"
  )
  expect_error(
    register_renderer("test", mod, option = "n_rows",
                      requests = list(a = function() 1)),
    class = "renderer_requests_invalid"
  )

  expect_error(get_renderer("blockr.test-test"), class = "renderer_missing")
})
