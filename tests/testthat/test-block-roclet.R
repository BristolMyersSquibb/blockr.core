test_that("the roclet reads block metadata off constructor tags", {

  skip_if_not_installed("roxygen2")

  txt <- "
#' Title
#' @block demo block
#' @blockDescr A demo
#' @blockCategory transform
#' @blockIcon funnel
#' @blockParam n Number of rows
#' @blockParamExample n 5
#' @export
new_demo_block <- function(n = 1) n
"

  res <- roxygen2::roc_proc_text(block_registration_roclet(), txt)

  expect_named(res, "new_demo_block")
  expect_identical(res[["new_demo_block"]][["name"]], "demo block")
  expect_identical(res[["new_demo_block"]][["description"]], "A demo")
  expect_identical(res[["new_demo_block"]][["category"]], "transform")
  expect_identical(res[["new_demo_block"]][["icon"]], "funnel")
  expect_identical(
    res[["new_demo_block"]][["arguments"]],
    list(n = list(description = "Number of rows", example = "5"))
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
    list(arguments = list(x = list(description = "d", example = "e")))
  )

  expect_s3_class(spec, "block_args")
  expect_identical(block_arg_description(spec[["x"]]), "d")
  expect_identical(block_arg_example(spec[["x"]]), "e")

  desc_only <- yaml_block_arguments(
    list(arguments = list(x = list(description = "d")))
  )

  expect_identical(block_arg_description(desc_only[["x"]]), "d")
  expect_null(block_arg_example(desc_only[["x"]]))
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
})
