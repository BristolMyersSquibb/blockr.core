test_that("registry", {

  curr <- list_blocks()
  withr::defer(unregister_blocks(setdiff(list_blocks(), curr)))

  entry <- register_block(new_class_head_block, "Head", "First rows",
                          category = "transform")

  expect_s3_class(entry, "block_registry_entry")
  expect_identical(entry$id, "head_block")
  expect_true("head_block" %in% list_blocks())
  expect_identical(available_blocks()[["head_block"]], entry)

  expect_identical(entry$block_class, head_class)
  expect_identical(entry$constructor, new_class_head_block)
  expect_identical(entry$inputs, "data")
  expect_identical(entry$state, list(n = 6L, direction = "head"))
  expect_identical(entry$options, character())
  expect_null(entry$schema)
  expect_null(entry$package)
  expect_identical(
    entry$requests,
    list(
      n_max = list(inputs = "data", state_args = character(),
                   options = character(), volatile = FALSE, action = FALSE)
    )
  )

  expect_identical(registry_id_from_block(new_class_head_block()), "head_block")
  expect_identical(registry_id_from_block(head_class), "head_block")
  expect_identical(registry_id_from_block(new_class_merge_block()), character())
  expect_identical(registry_id_from_block(new_dataset_block()), character())

  blk <- create_block("head_block", n = 2L)

  expect_identical(block_state(blk), list(n = 2L, direction = "head"))

  expect_error(
    register_block(new_class_head_block, "Head", "First rows"),
    class = "block_already_in_registry",
    regexp = "overwrite = TRUE"
  )

  expect_error(
    register_block(new_class_head_block, "Head", "First rows", uid = "other"),
    class = "block_class_in_registry"
  )

  expect_s3_class(
    register_block(new_class_head_block, "Head", "Last rows",
                   overwrite = TRUE),
    "block_registry_entry"
  )

  expect_identical(available_blocks()[["head_block"]]$description, "Last rows")

  expect_error(
    register_block(new_head_block, "Head", "First rows"),
    class = "block_constructor_invalid"
  )

  expect_error(
    register_block(new_class_merge_block, "Merge", "Merge", uid = "a-b"),
    class = "registry_id_invalid"
  )

  rlang::reset_warning_verbosity("block_category_test_discouraged")

  expect_warning(
    register_block(new_class_merge_block, "Merge", "Merge", category = "test"),
    class = "block_category_discouraged"
  )

  rlang::reset_warning_verbosity("block_icon_cloud-arrow-up-fill_discouraged")

  expect_warning(
    register_block(new_class_merge_block, "Merge", "Merge",
                   icon = "cloud-arrow-up-fill", overwrite = TRUE),
    class = "block_icon_fill_discouraged"
  )

  expect_error(
    register_block(new_class_merge_block, "Merge", "Merge", icon = "test",
                   overwrite = TRUE),
    class = "block_icon_invalid"
  )

  unregister_blocks("head_block")

  expect_false("head_block" %in% list_blocks())
  expect_identical(registry_id_from_block(new_class_head_block()), character())

  icon <- default_icon(default_category())

  expect_type(icon, "character")
  expect_length(icon, 1L)
})

test_that("a constructor registered by name comes with its package", {

  expect_identical(registry_id("head_block", "blockr.pkg"),
                   "blockr.pkg-head_block")
  expect_identical(registry_id("head_block", NULL), "head_block")

  expect_error(registry_id("head-block", NULL), class = "registry_id_invalid")
  expect_error(registry_id("", "blockr.pkg"), class = "registry_id_invalid")
  expect_error(registry_id(1, NULL), class = "registry_id_invalid")
})

test_that("the arg specs default to every state arg", {

  withr::defer(unregister_blocks("merge_block"))

  entry <- register_block(new_class_merge_block, "Merge", "Merge")

  expect_s3_class(entry$arguments, "arg_specs")
  expect_named(entry$arguments, c("by", "all_x", "all_y"))
  expect_null(arg_spec_description(entry$arguments[["by"]]))
})

test_that("registration reads the schema the module ships", {

  dir <- withr::local_tempdir()

  writeLines("export default {};", file.path(dir, "head.js"))
  writeLines(
    '{"type": "object", "properties": {"n": {"type": "integer"}}}',
    file.path(dir, "head.schema.json")
  )

  cls <- new_block_class(
    "schema_block",
    inputs = "data",
    expr = function(n = 6L) bbquote(utils::head(.(data), n = .(n))),
    ui = js_module(file.path(dir, "head.js"), package = NULL)
  )

  withr::defer(unregister_blocks(intersect("schema_block", list_blocks())))

  entry <- register_block(block_constructor(cls), "Schema", "With a schema")

  expect_identical(
    entry$schema,
    list(type = "object", properties = list(n = list(type = "integer")))
  )

  writeLines(
    '{"type": "object", "properties": {"m": {"type": "integer"}}}',
    file.path(dir, "head.schema.json")
  )

  expect_error(
    register_block(block_constructor(cls), "Schema", "With a schema",
                   overwrite = TRUE),
    class = "block_schema_invalid"
  )
})

test_that("a registry entry's board options are its class's", {

  cls <- new_block_class(
    "opts_block",
    inputs = "data",
    expr = function() bbquote(.(data)),
    ui = js_module("opts.js"),
    options = list(new_n_rows_option)
  )

  withr::defer(unregister_blocks("opts_block"))

  entry <- register_block(block_constructor(cls), "Opts", "With options")

  expect_identical(entry$options, "n_rows")
  expect_identical(names(board_options(entry)), "n_rows")

  board <- new_board(c(a = new_dataset_block()))

  expect_true("n_rows" %in% names(blockr_app_options(board)))
})

test_that("registry entries print", {

  withr::defer(unregister_blocks("head_block"))

  expect_snapshot(
    print(register_block(new_class_head_block, "Head", "First rows"))
  )
})

test_that("core's built-in blocks are not registered yet", {

  expect_error(
    register_package_blocks(package = "blockr.core",
                            which = "new_dataset_block"),
    class = "block_constructor_invalid"
  )
})
