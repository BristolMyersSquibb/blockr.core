# The source of a block constructor below the roxygen tags of a roclet test:
# `ctor`, made from a class with the id `id`, as a package defines one.
block_ctor_src <- function(ctor = "new_demo_block", id = "demo_block") {
  paste0(
    ctor, " <- blockr.core::block_constructor(\n",
    "  blockr.core::new_block_class(\n",
    "    \"", id, "\",\n",
    "    expr = function() quote(seq_len(1L)),\n",
    "    ui = blockr.core::js_module(\"demo.js\", package = NULL)\n",
    "  )\n",
    ")\n"
  )
}

test_that("the roclet reads block metadata off constructor tags", {

  skip_if_not_installed("roxygen2")

  txt <- paste0("
#' Title
#' @block demo block
#' @blockDescr A demo
#' @blockCategory transform
#' @blockIcon funnel
#' @blockGuidance Keeps the first `n` rows;
#'   `direction` picks the end.
#' @blockKeywords head, tail, multi word
#' @blockArg n Number of rows to keep
#'   [example] 10L
#'   [type] arg_integer()
#' @blockArg direction Which end, head or tail
#'   [example] \"tail\"
#'   [type] arg_enum(c(\"head\", \"tail\"))
#' @export
", block_ctor_src())

  res <- roxygen2::roc_proc_text(block_registration_roclet(), txt)[[1L]]

  expect_identical(res[["id"]], "demo_block")
  expect_identical(res[["name"]], "demo block")
  expect_identical(res[["description"]], "A demo")
  expect_identical(res[["category"]], "transform")
  expect_identical(res[["icon"]], "funnel")
  expect_identical(
    res[["guidance"]],
    "Keeps the first `n` rows; `direction` picks the end."
  )
  expect_identical(res[["keywords"]], c("head", "tail", "multi word"))

  args <- res[["arguments"]]
  expect_identical(args[["n"]][["description"]], "Number of rows to keep")
  expect_identical(args[["n"]][["example"]], 10L)
  expect_identical(args[["n"]][["type"]][["type"]], "integer")
  expect_identical(
    args[["direction"]][["description"]],
    "Which end, head or tail"
  )
  expect_identical(args[["direction"]][["example"]], "tail")
  expect_identical(
    as.character(args[["direction"]][["type"]][["enum"]]),
    c("head", "tail")
  )
})

test_that("@blockArg allows an explicit description and type-only args", {

  skip_if_not_installed("roxygen2")

  txt <- paste0("
#' Title
#' @block demo block
#' @blockDescr A demo
#' @blockCategory transform
#' @blockArg x
#'   [type] arg_string()
#'   [description] The x column
#' @export
", block_ctor_src())

  args <- roxygen2::roc_proc_text(
    block_registration_roclet(), txt
  )[[1L]][["arguments"]]

  expect_named(args, "x")
  expect_identical(args[["x"]][["description"]], "The x column")
  expect_identical(args[["x"]][["type"]][["type"]], "string")
  expect_null(args[["x"]][["example"]])
})

test_that("@blockExamples yields block-level worked configs", {

  skip_if_not_installed("roxygen2")

  txt <- paste0("
#' Title
#' @block demo block
#' @blockDescr A demo
#' @blockCategory transform
#' @blockExamples list(
#'   list(n = 10L),
#'   list(n = 3L)
#' )
#' @export
", block_ctor_src())

  res <- roxygen2::roc_proc_text(block_registration_roclet(), txt)[[1L]]

  expect_identical(res[["examples"]], list(list(n = 10L), list(n = 3L)))
})

test_that("details falls back to @section and @blockLink is captured", {

  skip_if_not_installed("roxygen2")

  txt <- paste0("
#' Title
#' @block demo block
#' @blockDescr A demo
#' @blockCategory transform
#' @blockLink https://example.com/demo
#' @section Demo block: Prose about the block.
#' @export
", block_ctor_src())

  res <- roxygen2::roc_proc_text(block_registration_roclet(), txt)[[1L]]

  expect_identical(res[["details"]], "Prose about the block.")
  expect_identical(res[["link"]], "https://example.com/demo")
})

test_that("@blockDetails overrides the @section fallback", {

  skip_if_not_installed("roxygen2")

  txt <- paste0("
#' Title
#' @block demo block
#' @blockDescr A demo
#' @blockCategory transform
#' @blockDetails An explicit detail.
#' @section Demo block: Ignored prose.
#' @export
", block_ctor_src())

  res <- roxygen2::roc_proc_text(block_registration_roclet(), txt)[[1L]]

  expect_identical(res[["details"]], "An explicit detail.")
})

test_that("block_link derives a pkgdown URL, gated on the URL existing", {

  skip_if_not_installed("roxygen2")

  block <- roxygen2::parse_text("
#' Title
#' @rdname some_topic
#' @export
new_x_block <- function() NULL
")[[1L]]

  expect_identical(
    derived_link(block, "https://example.com/pkg"),
    "https://example.com/pkg/reference/some_topic.html"
  )
  expect_null(derived_link(block, NULL))

  local_mocked_bindings(url_exists = function(url) TRUE)
  expect_identical(
    block_link(block, "https://example.com/pkg"),
    "https://example.com/pkg/reference/some_topic.html"
  )

  local_mocked_bindings(url_exists = function(url) FALSE)
  expect_null(block_link(block, "https://example.com/pkg"))
})

test_that("an explicit @blockLink is used without an HTTP check", {

  skip_if_not_installed("roxygen2")

  block <- roxygen2::parse_text("
#' Title
#' @blockLink https://example.com/custom
#' @export
new_x_block <- function() NULL
")[[1L]]

  local_mocked_bindings(url_exists = function(url) stop("must not be called"))

  expect_identical(
    block_link(block, "https://example.com/pkg"),
    "https://example.com/custom"
  )
})

test_that("only constructors carrying @block are picked up", {

  skip_if_not_installed("roxygen2")

  txt <- "
#' Title
#' @export
untagged <- function() NULL
"

  expect_length(roxygen2::roc_proc_text(block_registration_roclet(), txt), 0L)
})

test_that("@blockCtor overrides the derived constructor name", {

  skip_if_not_installed("roxygen2")

  txt <- paste0("
#' Title
#' @block demo
#' @blockDescr A demo
#' @blockCategory transform
#' @blockCtor make_demo
#' @export
new_demo_block <- function() NULL

", block_ctor_src("make_demo"))

  expect_named(
    roxygen2::roc_proc_text(block_registration_roclet(), txt),
    "make_demo"
  )
})

test_that("the roclet reads each class's id off its constructor", {

  skip_if_not_installed("roxygen2")

  txt <- paste0("
#' Title
#' @block shown block
#' @blockDescr A demo
#' @blockCategory transform
#' @export
", block_ctor_src("new_shown_block", "other_block"))

  res <- roxygen2::roc_proc_text(block_registration_roclet(), txt)

  # The class's own id, whatever the constructor's name, first in its entry
  expect_named(res, "new_shown_block")
  expect_identical(names(res[["new_shown_block"]])[1L], "id")
  expect_identical(res[["new_shown_block"]][["id"]], "other_block")
})

test_that("a @block that documents no block constructor aborts", {

  skip_if_not_installed("roxygen2")

  txt <- "
#' Title
#' @block demo
#' @blockDescr A demo
#' @blockCategory transform
#' @export
new_demo_block <- function() NULL
"

  expect_error(
    roxygen2::roc_proc_text(block_registration_roclet(), txt),
    class = "block_roclet_invalid"
  )
})

test_that("a @block missing a required companion tag aborts", {

  skip_if_not_installed("roxygen2")

  txt <- "
#' Title
#' @block demo
#' @export
new_demo_block <- function() NULL
"

  expect_error(
    roxygen2::roc_proc_text(block_registration_roclet(), txt),
    class = "block_roclet_invalid"
  )
})

test_that("YAML arguments become an arg_specs collection, examples included", {

  expect_null(yaml_block_arguments(list()))
  expect_null(yaml_block_arguments(list(arguments = list())))

  spec <- yaml_block_arguments(
    list(
      arguments = list(
        x = list(
          description = "d",
          example = "e",
          type = list(type = "integer")
        )
      )
    )
  )

  expect_s3_class(spec, "arg_specs")
  expect_identical(arg_spec_description(spec[["x"]]), "d")
  expect_identical(arg_spec_example(spec[["x"]]), "e")
  expect_identical(arg_spec_type(spec[["x"]]), list(type = "integer"))

  desc_only <- yaml_block_arguments(
    list(arguments = list(x = list(description = "d")))
  )

  expect_identical(arg_spec_description(desc_only[["x"]]), "d")
  expect_null(arg_spec_example(desc_only[["x"]]))
  expect_null(arg_spec_type(desc_only[["x"]]))
})

test_that("a package that ships no YAML registry registers no block class", {

  before <- list_blocks()

  expect_invisible(register_yaml_blocks("utils"))
  expect_identical(list_blocks(), before)
})

test_that("the shipped core registry is complete", {

  file <- system.file("registry/blocks.yml", package = "blockr.core")

  expect_true(nzchar(file))

  reg <- yaml::read_yaml(file)

  expect_length(reg, 10L)

  # Each with the id of its class, which core's classes take from their
  # constructors' names
  expect_identical(chr_xtr(reg, "id"), sub("^new_", "", names(reg)))
  expect_identical(
    reg[["new_dataset_block"]][["arguments"]][["dataset"]][["example"]],
    "iris"
  )

  head <- reg[["new_head_block"]]
  expect_identical(head[["arguments"]][["n"]][["type"]][["type"]], "integer")
  expect_identical(
    head[["arguments"]][["direction"]][["type"]][["enum"]],
    c("head", "tail")
  )
  expect_true(is_string(head[["guidance"]]))
  expect_gt(length(head[["keywords"]]), 0L)

  expect_true(is_string(reg[["new_dataset_block"]][["details"]]))
  expect_length(reg[["new_merge_block"]][["examples"]], 2L)
  expect_match(
    reg[["new_dataset_block"]][["link"]],
    "/reference/new_dataset_block\\.html$"
  )
})
