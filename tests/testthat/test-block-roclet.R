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
})

test_that("@blockLink is captured", {

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

  expect_identical(res[["link"]], "https://example.com/demo")

  # The help page's prose stays there, rather than in the class's entry
  expect_null(res[["details"]])
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

test_that("a package that lists no block classes registers none", {

  registry <- new.env()

  expect_invisible(register_listed_blocks("utils", registry))
  expect_length(ls(registry), 0L)
})

test_that("the shipped core registry is complete", {

  file <- system.file("registry/blocks.yml", package = "blockr.core")

  expect_true(nzchar(file))

  reg <- yaml::read_yaml(file)

  expect_length(reg, 12L)

  # Each with the id of its class, which core's classes take from their
  # constructors' names
  expect_identical(chr_xtr(reg, "id"), sub("^new_", "", names(reg)))

  head <- reg[["new_head_block"]]
  expect_true(is_string(head[["guidance"]]))
  expect_gt(length(head[["keywords"]]), 0L)

  expect_match(
    reg[["new_dataset_block"]][["link"]],
    "/reference/new_dataset_block\\.html$"
  )
})
