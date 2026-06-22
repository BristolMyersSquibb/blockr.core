test_that("block_arg and block_args constructors", {

  a <- block_arg(description = "d", example = 1L, type = NULL)

  expect_s3_class(a, "block_arg")
  expect_identical(a[["description"]], "d")
  expect_identical(a[["example"]], 1L)
  expect_null(a[["type"]])

  spec <- block_args(n = "just a description")

  expect_s3_class(spec, "block_args")
  expect_s3_class(spec[["n"]], "block_arg")
  expect_identical(spec[["n"]][["description"]], "just a description")

  expect_length(block_args(), 0L)

  expect_error(block_arg(description = 1L), class = "block_arg_invalid")
  expect_error(block_args(block_arg("x")), class = "block_args_unnamed")
  expect_error(block_args(n = 1L), class = "block_arg_invalid")
})

test_that("bare and empty arguments normalize without warning", {

  expect_silent(
    bare <- normalize_arguments(c(n = "rows", direction = "end"), NULL)
  )

  expect_s3_class(bare[["arguments"]], "block_args")
  expect_identical(bare[["arguments"]][["n"]][["description"]], "rows")
  expect_null(bare[["guidance"]])

  empty <- normalize_arguments(character(), NULL)

  expect_length(empty[["arguments"]], 0L)
})

test_that("legacy examples/prompt attributes are absorbed with a deprecation", {

  withr::local_options(rlib_warning_verbosity = "verbose")

  legacy <- structure(
    c(direction = "head or tail"),
    examples = list(direction = "tail"),
    prompt = "Pick head for the first rows."
  )

  expect_warning(
    norm <- normalize_arguments(legacy, NULL),
    class = "deprecated_arg_attrs"
  )

  expect_identical(norm[["guidance"]], "Pick head for the first rows.")
  expect_identical(norm[["arguments"]][["direction"]][["example"]], "tail")

  expect_identical(
    block_examples_list(norm[["arguments"]], list()),
    list(list(direction = "tail"))
  )
})

test_that("per-argument examples assemble into one whole-block configuration", {

  spec <- block_args(
    n = block_arg("count", example = 10L),
    direction = block_arg("end", example = "tail")
  )

  expect_identical(
    block_examples_list(spec, list()),
    list(list(n = 10L, direction = "tail"))
  )
})

test_that("block-level examples supersede the per-argument assembly", {

  spec <- block_args(direction = block_arg("end", example = "head"))

  authored <- list(
    list(direction = "head"),
    list(direction = "tail")
  )

  expect_identical(block_examples_list(spec, authored), authored)
})

test_that("registration hard-validates the structured spec form", {

  expect_error(
    register_block(
      new_head_block,
      name = "t",
      description = "t",
      uid = "ut_head_bad_arg",
      arguments = block_args(not_a_formal = block_arg("x"))
    ),
    class = "block_arg_unknown"
  )

  expect_error(
    register_block(
      new_head_block,
      name = "t",
      description = "t",
      uid = "ut_head_bad_example",
      arguments = block_args(direction = block_arg("end", example = "sideways"))
    ),
    class = "block_example_invalid"
  )

  expect_false("ut_head_bad_arg" %in% list_blocks())
  expect_false("ut_head_bad_example" %in% list_blocks())
})

test_that("block-level examples validate without a user-supplied spec", {

  withr::defer(unregister_blocks("ut_ex_only"))

  expect_no_error(
    register_block(
      new_dataset_block,
      name = "t",
      description = "t",
      uid = "ut_ex_only",
      examples = list(list(dataset = "iris"))
    )
  )

  expect_error(
    register_block(
      new_head_block,
      name = "t",
      description = "t",
      uid = "ut_ex_only_bad",
      examples = list(list(direction = "sideways"))
    ),
    class = "block_example_invalid"
  )
})

test_that("a valid structured spec registers and is exposed via accessors", {

  withr::defer(unregister_blocks("ut_head_ok"))

  register_block(
    new_head_block,
    name = "Head",
    description = "First or last rows.",
    uid = "ut_head_ok",
    arguments = block_args(
      n = block_arg("How many rows", example = 10L),
      direction = block_arg("head or tail", example = "tail")
    ),
    details = "Defaults to the first six rows.",
    link = "https://example.com/head",
    guidance = "Use direction='tail' for the bottom of the table.",
    keywords = c("first", "last", "top", "rows")
  )

  expect_s3_class(block_arg_specs("ut_head_ok"), "block_args")

  expect_identical(
    block_examples("ut_head_ok"),
    list(list(n = 10L, direction = "tail"))
  )

  meta <- registry_metadata(
    "ut_head_ok",
    c("details", "link", "guidance", "keywords")
  )

  expect_identical(meta[["details"]], "Defaults to the first six rows.")
  expect_identical(meta[["link"]], "https://example.com/head")
  expect_identical(meta[["guidance"]],
                   "Use direction='tail' for the bottom of the table.")
  expect_identical(meta[["keywords"]][[1L]], c("first", "last", "top", "rows"))
})

test_that("registry_metadata projects arguments to the legacy shape", {

  withr::defer(unregister_blocks("ut_head_legacy"))

  register_block(
    new_head_block,
    name = "Head",
    description = "First or last rows.",
    uid = "ut_head_legacy",
    arguments = block_args(
      direction = block_arg("head or tail", example = "tail")
    ),
    guidance = "Pick head for the first rows."
  )

  args <- registry_metadata("ut_head_legacy", "arguments")[[1L]]

  expect_type(args, "character")
  expect_identical(as.character(args), "head or tail")
  expect_identical(names(args), "direction")
  expect_identical(attr(args, "examples"), list(direction = "tail"))
  expect_identical(attr(args, "prompt"), "Pick head for the first rows.")
})

test_that("absent optional fields are NA; bare arguments carry no attrs", {

  withr::defer(unregister_blocks("ut_head_bare"))

  register_block(
    new_head_block,
    name = "Head",
    description = "First or last rows.",
    uid = "ut_head_bare",
    arguments = c(direction = "head or tail")
  )

  meta <- registry_metadata("ut_head_bare", c("details", "guidance"))

  expect_true(is.na(meta[["details"]]))
  expect_true(is.na(meta[["guidance"]]))

  args <- registry_metadata("ut_head_bare", "arguments")[[1L]]

  expect_null(attr(args, "examples"))
  expect_null(attr(args, "prompt"))
})
