test_that("new_block_arg and new_block_args constructors", {

  a <- new_block_arg(description = "d", example = 1L, type = NULL)

  expect_s3_class(a, "block_arg")
  expect_identical(a[["description"]], "d")
  expect_identical(a[["example"]], 1L)
  expect_null(a[["type"]])

  spec <- new_block_args(n = "just a description")

  expect_s3_class(spec, "block_args")
  expect_s3_class(spec[["n"]], "block_arg")
  expect_identical(spec[["n"]][["description"]], "just a description")

  expect_length(new_block_args(), 0L)

  expect_error(new_block_arg(description = 1L), class = "block_arg_invalid")
  expect_error(new_block_args(new_block_arg("x")), class = "block_args_unnamed")
  expect_error(new_block_args(n = 1L), class = "block_arg_invalid")
})

test_that("block_arg field getters expose the spec without indexing", {

  a <- new_block_arg(description = "rows", example = 10L, type = NULL)

  expect_identical(arg_description(a), "rows")
  expect_identical(arg_example(a), 10L)
  expect_null(arg_type(a))

  expect_error(arg_description("not a block_arg"))

  spec <- new_block_args(
    n = new_block_arg("rows", example = 10L),
    direction = new_block_arg("end", example = "tail")
  )

  expect_identical(
    vapply(spec, arg_description, character(1)),
    c(n = "rows", direction = "end")
  )
})

test_that("as_block_args dispatches by input type", {

  spec <- new_block_args(n = new_block_arg("rows"))

  expect_identical(as_block_args(spec), spec)
  expect_s3_class(as_block_args(list(n = new_block_arg("rows"))), "block_args")
  expect_s3_class(as_block_args(c(n = "rows")), "block_args")

  expect_error(as_block_args(1L), class = "block_args_invalid")
  expect_error(as_block_arg(1L), class = "block_arg_invalid")
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

  spec <- new_block_args(
    n = new_block_arg("count", example = 10L),
    direction = new_block_arg("end", example = "tail")
  )

  expect_identical(
    block_examples_list(spec, list()),
    list(list(n = 10L, direction = "tail"))
  )
})

test_that("block-level examples supersede the per-argument assembly", {

  spec <- new_block_args(direction = new_block_arg("end", example = "head"))

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
      arguments = new_block_args(not_a_formal = new_block_arg("x"))
    ),
    class = "block_arg_unknown"
  )

  expect_error(
    register_block(
      new_head_block,
      name = "t",
      description = "t",
      uid = "ut_head_bad_example",
      arguments = new_block_args(
        direction = new_block_arg("end", example = "sideways")
      )
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

test_that("construction-metadata accessors dispatch on id, entry and block", {

  withr::defer(unregister_blocks("ut_head_acc"))

  register_block(
    new_head_block,
    name = "Head",
    description = "First or last rows.",
    uid = "ut_head_acc",
    arguments = new_block_args(
      n = new_block_arg("How many rows", example = 10L),
      direction = new_block_arg("head or tail", example = "tail")
    ),
    guidance = c("Use direction='tail' for the bottom.", "n counts rows."),
    keywords = c("first", "last", "top")
  )

  expect_s3_class(block_args("ut_head_acc"), "block_args")
  expect_identical(
    block_examples("ut_head_acc"),
    list(list(n = 10L, direction = "tail"))
  )
  expect_identical(
    block_guidance("ut_head_acc"),
    c("Use direction='tail' for the bottom.", "n counts rows.")
  )
  expect_identical(block_keywords("ut_head_acc"), c("first", "last", "top"))

  entry <- available_blocks()[["ut_head_acc"]]
  expect_identical(block_keywords(entry), c("first", "last", "top"))

  blk <- new_head_block()
  expect_identical(block_args(blk), block_args("head_block"))
  expect_identical(block_guidance(blk), block_guidance("head_block"))

  expect_identical(
    vapply(block_args("ut_head_acc"), arg_description, character(1)),
    c(n = "How many rows", direction = "head or tail")
  )
})

test_that("block_keywords and block_guidance default to empty character", {

  withr::defer(unregister_blocks("ut_head_empty"))

  register_block(
    new_head_block,
    name = "Head",
    description = "First or last rows.",
    uid = "ut_head_empty"
  )

  expect_identical(block_keywords("ut_head_empty"), character())
  expect_identical(block_guidance("ut_head_empty"), character())
})

test_that("block_metadata tabulates scalar catalog fields", {

  withr::defer(unregister_blocks("ut_head_cat"))

  register_block(
    new_head_block,
    name = "Head",
    description = "First or last.",
    uid = "ut_head_cat",
    category = "transform",
    details = "Defaults to six rows.",
    link = "https://example.com/head"
  )

  meta <- block_metadata("ut_head_cat")

  expect_s3_class(meta, "data.frame")
  expect_identical(meta$name, "Head")
  expect_identical(meta$details, "Defaults to six rows.")
  expect_identical(meta$link, "https://example.com/head")
  expect_false("arguments" %in% names(meta))
  expect_false("keywords" %in% names(meta))

  blk <- new_head_block()
  expect_identical(block_metadata(blk)$id, "head_block")
})

test_that("registry_metadata is deprecated but still projects legacy shape", {

  withr::local_options(rlib_warning_verbosity = "verbose")
  withr::defer(unregister_blocks("ut_head_legacy"))

  register_block(
    new_head_block,
    name = "Head",
    description = "First or last.",
    uid = "ut_head_legacy",
    arguments = new_block_args(
      direction = new_block_arg("head or tail", example = "tail")
    ),
    guidance = "Pick head for the first rows."
  )

  expect_warning(
    args <- registry_metadata("ut_head_legacy", "arguments"),
    class = "deprecated_registry_metadata"
  )

  args <- args[[1L]]

  expect_type(args, "character")
  expect_identical(as.character(args), "head or tail")
  expect_identical(attr(args, "examples"), list(direction = "tail"))
  expect_identical(attr(args, "prompt"), "Pick head for the first rows.")
})
