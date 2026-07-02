test_that("the roclet reads block metadata off constructor tags", {

  skip_if_not_installed("roxygen2")

  txt <- "
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
new_demo_block <- function(n = 1, direction = \"head\") n
"

  res <- roxygen2::roc_proc_text(block_registration_roclet(), txt)[[1L]]

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

  txt <- "
#' Title
#' @block demo block
#' @blockDescr A demo
#' @blockCategory transform
#' @blockArg x
#'   [type] arg_string()
#'   [description] The x column
#' @export
new_demo_block <- function(x = \"\") x
"

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

  txt <- "
#' Title
#' @block demo block
#' @blockDescr A demo
#' @blockCategory transform
#' @blockExamples list(
#'   list(n = 10L),
#'   list(n = 3L)
#' )
#' @export
new_demo_block <- function(n = 1) n
"

  res <- roxygen2::roc_proc_text(block_registration_roclet(), txt)[[1L]]

  expect_identical(res[["examples"]], list(list(n = 10L), list(n = 3L)))
})

test_that("details falls back to @section and @blockLink is captured", {

  skip_if_not_installed("roxygen2")

  txt <- "
#' Title
#' @block demo block
#' @blockDescr A demo
#' @blockCategory transform
#' @blockLink https://example.com/demo
#' @section Demo block: Prose about the block.
#' @export
new_demo_block <- function() NULL
"

  res <- roxygen2::roc_proc_text(block_registration_roclet(), txt)[[1L]]

  expect_identical(res[["details"]], "Prose about the block.")
  expect_identical(res[["link"]], "https://example.com/demo")
})

test_that("@blockDetails overrides the @section fallback", {

  skip_if_not_installed("roxygen2")

  txt <- "
#' Title
#' @block demo block
#' @blockDescr A demo
#' @blockCategory transform
#' @blockDetails An explicit detail.
#' @section Demo block: Ignored prose.
#' @export
new_demo_block <- function() NULL
"

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

  txt <- "
#' Title
#' @block demo
#' @blockDescr A demo
#' @blockCategory transform
#' @blockCtor make_demo
#' @export
new_demo_block <- function() NULL
"

  expect_named(
    roxygen2::roc_proc_text(block_registration_roclet(), txt),
    "make_demo"
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

test_that("YAML arguments become a block_args spec, examples included", {

  expect_identical(yaml_block_arguments(list()), character())
  expect_identical(yaml_block_arguments(list(arguments = list())), character())

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

  expect_s3_class(spec, "block_args")
  expect_identical(block_arg_description(spec[["x"]]), "d")
  expect_identical(block_arg_example(spec[["x"]]), "e")
  expect_identical(block_arg_type(spec[["x"]]), list(type = "integer"))

  desc_only <- yaml_block_arguments(
    list(arguments = list(x = list(description = "d")))
  )

  expect_identical(block_arg_description(desc_only[["x"]]), "d")
  expect_null(block_arg_example(desc_only[["x"]]))
  expect_null(block_arg_type(desc_only[["x"]]))
})

test_that("register_package_blocks is a no-op when no registry ships", {

  before <- list_blocks()

  expect_invisible(register_package_blocks(package = "utils"))
  expect_identical(list_blocks(), before)
})

test_that("register_package_blocks registers a subset and keeps examples", {

  curr <- list_blocks()

  withr::defer(
    {
      unregister_blocks()
      register_core_blocks(curr)
    }
  )

  unregister_blocks()
  register_package_blocks(package = "blockr.core", which = "new_dataset_block")

  expect_identical(list_blocks(), "dataset_block")

  args <- block_meta_arguments("dataset_block")

  expect_identical(
    block_arg_description(args[["dataset"]]),
    "Selects the dataset to use."
  )
  expect_identical(block_arg_example(args[["dataset"]]), "iris")

  expect_error(
    register_package_blocks(package = "blockr.core", which = "not_a_block")
  )
})

test_that("the shipped core registry is complete", {

  file <- system.file("registry/blocks.yml", package = "blockr.core")

  expect_true(nzchar(file))

  reg <- yaml::read_yaml(file)

  expect_length(reg, 10L)
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
    "/reference/new_data_block\\.html$"
  )
})

test_that("core blocks register with types, guidance and keywords", {

  args <- block_meta_arguments("head_block")

  expect_identical(block_arg_type(args[["n"]])[["type"]], "integer")

  direction <- block_arg_type(args[["direction"]])
  expect_identical(direction[["type"]], "string")
  expect_identical(as.character(direction[["enum"]]), c("head", "tail"))

  expect_true(is_string(block_meta_guidance("head_block")))
  expect_gt(length(block_meta_keywords("head_block")), 0L)

  merge_args <- block_meta_arguments("merge_block")
  expect_identical(block_arg_type(merge_args[["by"]])[["type"]], "array")
  expect_identical(block_arg_type(merge_args[["all_x"]])[["type"]], "boolean")

  expect_true(is_string(block_meta_details("dataset_block")))
  expect_match(
    block_meta_link("dataset_block"),
    "/reference/new_data_block\\.html$"
  )
  expect_identical(
    block_meta_examples("merge_block"),
    list(list(all_x = TRUE, all_y = FALSE), list(all_x = TRUE, all_y = TRUE))
  )
})
