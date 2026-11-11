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

  # The class gives the id, whether or not the block registry holds it
  expect_identical(registry_id_from_block(new_class_merge_block()),
                   "merge_block")
  expect_identical(registry_id_from_block(new_static_block()),
                   "blockr.core-static_block")

  expect_error(registry_id_from_block(list()), class = "block_class_invalid")

  blk <- create_block("head_block", n = 2L)

  expect_identical(block_state(blk), list(n = 2L, direction = "head"))

  expect_error(
    register_block(new_class_head_block, "Head", "First rows"),
    class = "block_already_in_registry",
    regexp = "overwrite = TRUE"
  )

  expect_s3_class(
    register_block(new_class_head_block, "Head", "Last rows",
                   overwrite = TRUE),
    "block_registry_entry"
  )

  expect_identical(available_blocks()[["head_block"]]$description, "Last rows")

  expect_error(
    register_block(function() NULL, "Head", "First rows"),
    class = "block_constructor_invalid"
  )

  expect_error(
    register_block(1, "Head", "First rows"),
    class = "block_constructor_invalid"
  )

  # A constructor is given as itself, never by name
  expect_error(
    register_block("new_class_head_block", "Head", "First rows"),
    class = "block_constructor_invalid"
  )

  expect_error(
    register_block(new_class_merge_block, "Merge", "Merge", category = 1),
    class = "block_category_invalid"
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

  expect_error(
    register_block(new_class_merge_block, "Merge", "Merge", icon = 1,
                   overwrite = TRUE),
    class = "block_icon_invalid"
  )

  unregister_blocks("head_block")

  expect_false("head_block" %in% list_blocks())
  expect_identical(registry_id_from_block(new_class_head_block()), "head_block")

  expect_error(create_block("head_block"), class = "registry_id_unknown")
  expect_error(create_block(1), class = "registry_id_unknown")

  icon <- default_icon(default_category())

  expect_type(icon, "character")
  expect_length(icon, 1L)

  expect_error(default_icon(1), class = "block_category_invalid")
})

test_that("a block class's registry id comes from the class alone", {

  local_declared(block_registry)

  expr <- function(n = 1L) bbquote(seq_len(.(n)))
  ui <- js_module("count-block.js", package = NULL)

  pkg <- test_block_class("count_block", expr = expr, ui = ui,
                          package = "blockr.pkg")
  script <- test_block_class("count_block", expr = expr, ui = ui)

  expect_identical(pkg$package, "blockr.pkg")
  expect_null(script$package)

  # The package that defines the class names it, so a class of one name gives
  # another id in each package, and its name alone outside a package
  expect_identical(registry_id_from_block(pkg), "blockr.pkg-count_block")
  expect_identical(registry_id_from_block(script), "count_block")

  entry <- register_block(block_constructor(pkg), "Count", "Counts up")

  expect_identical(entry$id, "blockr.pkg-count_block")
  expect_identical(entry$package, "blockr.pkg")

  # Another constructor of the class takes the same id, whatever its name
  new_counter_block <- block_constructor(pkg)

  expect_error(
    register_block(new_counter_block, "Counter", "Counts up"),
    class = "block_already_in_registry"
  )

  expect_identical(
    register_block(block_constructor(script), "Count", "Counts up")$id,
    "count_block"
  )

  expect_null(get_registry_entry("count_block")$package)
})

test_that("a package's constructors give one registry id each", {

  expr <- function() bbquote(NULL)
  ui <- js_module("note-block.js", package = NULL)

  note <- test_block_class("note_block", expr = expr, ui = ui,
                           package = "blockr.pkg")
  again <- test_block_class("note_block", expr = expr, ui = ui,
                            package = "blockr.pkg")
  memo <- test_block_class("memo_block", expr = expr, ui = ui,
                           package = "blockr.pkg")

  expect_null(
    check_package_registry_ids(
      list(new_note_block = block_constructor(note),
           new_memo_block = block_constructor(memo)),
      "blockr.pkg"
    )
  )

  expect_error(
    check_package_registry_ids(
      list(new_note_block = block_constructor(note),
           new_memo_block = block_constructor(memo),
           new_text_block = block_constructor(again)),
      "blockr.pkg"
    ),
    class = "package_blocks_invalid",
    regexp = "new_note_block and new_text_block.+blockr.pkg-note_block"
  )
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
    paste(
      '{"type": "object", "properties": {"n": {"type": "integer"}},',
      '"required": ["n"]}'
    ),
    file.path(dir, "head.schema.json")
  )

  cls <- test_block_class(
    "schema_block",
    inputs = "data",
    expr = function(n = 6L) bbquote(utils::head(.(data), n = .(n))),
    ui = js_module(file.path(dir, "head.js"), package = NULL)
  )

  withr::defer(unregister_blocks(intersect("schema_block", list_blocks())))

  entry <- register_block(block_constructor(cls), "Schema", "With a schema")

  expect_identical(
    entry$schema,
    list(
      type = "object",
      properties = list(n = list(type = "integer")),
      required = I("n")
    )
  )

  expect_identical(
    unclass(encode_message(entry$schema)),
    '{"type":"object","properties":{"n":{"type":"integer"}},"required":["n"]}'
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

test_that("a registry entry's board options are those its requests read", {

  cls <- test_block_class(
    "opts_block",
    inputs = "data",
    expr = function() bbquote(.(data)),
    ui = js_module("opts.js"),
    requests = list(
      page = new_block_request(
        function(data, rows) utils::head(data, rows),
        options = list(rows = new_n_rows_option)
      )
    )
  )

  withr::defer(unregister_blocks("opts_block"))

  entry <- register_block(block_constructor(cls), "Opts", "With options")

  expect_identical(entry$requests$page$options, c(rows = "n_rows"))
  expect_identical(names(board_options(entry)), "n_rows")

  expect_identical(
    run_block_request("opts_block", "page", list(data = datasets::iris),
                      options = list(n_rows = 2L)),
    utils::head(datasets::iris, 2L)
  )
})

test_that("registry entries print", {

  withr::defer(unregister_blocks("head_block"))

  expect_snapshot(
    print(register_block(new_class_head_block, "Head", "First rows"))
  )
})

test_that("core registers its built-in blocks as it loads", {

  ids <- paste0(
    "blockr.core-",
    c("csv_block", "dataset_block", "filebrowser_block", "glue_block",
      "head_block", "merge_block", "rbind_block", "scatter_block",
      "subset_block", "upload_block")
  )

  expect_contains(list_blocks(), ids)

  entries <- available_blocks()[ids]

  expect_identical(unique(chr_xtr(entries, "package")), "blockr.core")

  head <- entries[["blockr.core-head_block"]]

  expect_identical(head[["block_class"]], head_block)
  expect_identical(head[["constructor"]], new_head_block)
  expect_identical(head[["state"]], list(n = 6L, direction = "head"))

  expect_identical(
    entries[["blockr.core-merge_block"]][["examples"]],
    list(
      list(by = list("Time"), all_x = TRUE, all_y = FALSE),
      list(by = list("Time"), all_x = TRUE, all_y = TRUE)
    )
  )

  # Core registers neither its static nor its fixed block, whose classes give
  # their ids all the same
  expect_false(any(c("blockr.core-static_block", "blockr.core-fixed_block") %in%
                     list_blocks()))
  expect_identical(registry_id_from_block(new_static_block()),
                   "blockr.core-static_block")
  expect_identical(registry_id_from_block(new_fixed_block()),
                   "blockr.core-fixed_block")

  entry <- register_package_blocks(package = "blockr.core",
                                   which = "new_dataset_block")

  expect_identical(entry[[1L]][["id"]], "blockr.core-dataset_block")

  expect_error(
    register_package_blocks(package = "blockr.core", which = "new_no_block"),
    class = "package_blocks_invalid"
  )

  expect_error(
    register_package_blocks(package = "blockr.core",
                            which = c("new_head_block", "new_head_block")),
    class = "package_blocks_invalid"
  )
})

test_that("YAML examples of an array type are lists", {

  arr <- list(type = "array", items = list(type = "string"))

  expect_identical(yaml_arg_example("a", arr), list("a"))
  expect_identical(yaml_arg_example(c("a", "b"), arr), list("a", "b"))
  expect_identical(yaml_arg_example(list("a"), arr), list("a"))
  expect_null(yaml_arg_example(NULL, arr))
  expect_identical(yaml_arg_example("a", list(type = "string")), "a")
  expect_identical(yaml_arg_example("a", NULL), "a")
})
