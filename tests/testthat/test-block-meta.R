test_that("block metadata", {

  meta1 <- block_metadata(new_dataset_block())

  expect_s3_class(meta1, "data.frame")
  expect_identical(nrow(meta1), 1L)
  expect_identical(ncol(meta1), 12L)
  expect_named(
    meta1,
    c("id", "name", "description", "details", "link", "guidance", "keywords",
      "category", "icon", "arguments", "examples", "package")
  )

  new_identity_block <- function() {
    new_transform_block(
      function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {
            list(
              expr = reactive(quote(identity(data))),
              state = list()
            )
          }
        )
      },
      function(id) {
        tagList()
      },
      block_metadata = list(),
      class = "identity_block"
    )
  }

  meta2 <- block_metadata(new_identity_block())

  expect_s3_class(meta2, "data.frame")
  expect_identical(nrow(meta2), 1L)
  expect_identical(ncol(meta2), 12L)
  expect_named(
    meta2,
    c("id", "name", "description", "details", "link", "guidance", "keywords",
      "category", "icon", "arguments", "examples", "package")
  )

  meta3 <- block_metadata(
    blocks(a = new_dataset_block(), b = new_identity_block())
  )

  expect_s3_class(meta3, "data.frame")
  expect_identical(nrow(meta3), 2L)
  expect_identical(ncol(meta3), 12L)
  expect_named(
    meta3,
    c("id", "name", "description", "details", "link", "guidance", "keywords",
      "category", "icon", "arguments", "examples", "package")
  )
  expect_identical(rownames(meta3), c("a", "b"))

  meta4 <- block_metadata(blocks())

  expect_s3_class(meta4, "data.frame")
  expect_identical(nrow(meta4), 0L)
  expect_identical(ncol(meta4), 12L)
  expect_named(
    meta4,
    c("id", "name", "description", "details", "link", "guidance", "keywords",
      "category", "icon", "arguments", "examples", "package")
  )

  expect_type(meta1$keywords, "list")
  expect_type(meta1$arguments, "list")
  expect_s3_class(meta1$arguments[[1L]], "arg_specs")

  sel <- block_metadata(new_dataset_block(), fields = c("name", "icon"))

  expect_named(sel, c("name", "icon"))
})

test_that("construction-metadata accessors dispatch on id, entry and block", {

  withr::defer(unregister_blocks("ut_head_acc"))

  register_block(
    new_head_block,
    name = "Head",
    description = "First or last rows.",
    uid = "ut_head_acc",
    arguments = new_arg_specs(
      n = new_arg_spec("How many rows", example = 10L),
      direction = new_arg_spec("head or tail", example = "tail")
    ),
    guidance = "Use direction='tail' for the bottom rows.",
    keywords = c("first", "last", "top")
  )

  expect_s3_class(block_meta_arguments("ut_head_acc"), "arg_specs")
  expect_identical(
    block_meta_examples("ut_head_acc"),
    list(list(n = 10L, direction = "tail"))
  )
  expect_identical(
    block_meta_guidance("ut_head_acc"),
    "Use direction='tail' for the bottom rows."
  )
  expect_identical(
    block_meta_keywords("ut_head_acc"),
    c("first", "last", "top")
  )

  entry <- available_blocks()[["ut_head_acc"]]
  expect_identical(block_meta_keywords(entry), c("first", "last", "top"))

  blk <- new_head_block()
  expect_identical(
    block_meta_arguments(blk),
    block_meta_arguments("head_block")
  )
  expect_identical(block_meta_guidance(blk), block_meta_guidance("head_block"))

  expect_identical(
    vapply(block_meta_arguments("ut_head_acc"), arg_spec_description,
           character(1)),
    c(n = "How many rows", direction = "head or tail")
  )
})

test_that("construction accessors read the block's attached metadata", {

  withr::defer({
    unregister_blocks()
    register_core_blocks()
  })

  register_block(
    new_head_block,
    name = "Head",
    description = "d",
    uid = "head_block",
    overwrite = TRUE,
    arguments = new_arg_specs(n = new_arg_spec("rows", example = 3L)),
    guidance = "be careful"
  )

  blk <- new_head_block()

  unregister_blocks("head_block")

  args <- block_meta_arguments(blk)

  expect_identical(arg_spec_description(args[["n"]]), "rows")
  expect_identical(block_meta_guidance(blk), "be careful")
  expect_identical(block_meta_examples(blk), list(list(n = 3L)))
})

test_that("block metadata accessors fall back when an attribute is absent", {

  withr::defer(unregister_blocks("ut_head_empty"))

  register_block(
    new_head_block,
    name = "Head",
    description = "First or last rows.",
    uid = "ut_head_empty"
  )

  expect_identical(block_meta_keywords("ut_head_empty"), character())
  expect_identical(block_meta_examples("ut_head_empty"), list())
  expect_identical(block_meta_guidance("ut_head_empty"), NA_character_)
})

test_that("block_metadata tabulates catalog fields with list-columns", {

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

  expect_type(meta$arguments, "list")
  expect_type(meta$keywords, "list")
  expect_s3_class(meta$arguments[[1L]], "arg_specs")

  blk <- new_head_block()
  expect_identical(block_metadata(blk)$id, "head_block")
})

test_that("an unregistered block is imputed a class-derived default record", {

  new_orphan_block <- function(block_metadata = NULL) {
    new_transform_block(
      function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {
            list(
              expr = reactive(quote(identity(data))),
              state = list()
            )
          }
        )
      },
      function(id) {
        tagList()
      },
      block_metadata = block_metadata,
      class = "ut_orphan_block"
    )
  }

  expect_warning(
    blk <- new_orphan_block(),
    class = "missing_block_metadata"
  )

  expect_true(is.list(attr(blk, "block_metadata")))
  expect_identical(
    block_metadata_record(blk),
    block_default_metadata(class(blk))
  )
  expect_named(
    block_metadata_record(blk),
    c("id", "name", "description", "details", "link", "guidance", "keywords",
      "category", "icon", "arguments", "package", "examples")
  )

  meta <- block_metadata(blk)

  expect_s3_class(meta, "data.frame")
  expect_identical(nrow(meta), 1L)
  expect_identical(meta$id, NA_character_)
  expect_identical(meta$name, "ut orphan block")
  expect_identical(meta$description, default_description())
  expect_false(is.na(meta$icon))

  expect_identical(block_meta_id(blk), NA_character_)
  expect_identical(block_meta_name(blk), "ut orphan block")
  expect_identical(block_meta_description(blk), default_description())
  expect_identical(block_meta_category(blk), default_category())
  expect_identical(block_meta_icon(blk), default_icon())
  expect_identical(block_meta_keywords(blk), character())
  expect_identical(block_meta_arguments(blk), new_arg_specs())

  mixed <- suppressWarnings(
    block_metadata(
      blocks(good = new_dataset_block(), orphan = new_orphan_block())
    )
  )

  expect_identical(nrow(mixed), 2L)
  expect_identical(rownames(mixed), c("good", "orphan"))
  expect_false(anyNA(mixed$icon))
  expect_identical(mixed["orphan", "description"], default_description())

  bare <- new_orphan_block(block_metadata = FALSE)

  expect_false(is.list(attr(bare, "block_metadata")))
  expect_identical(block_meta_name(bare), "ut orphan block")
})
