# The JSON that R writes for a part of what it tells the client.
json <- function(x) {
  as.character(encode_message(x))
}

test_that("R tells the client what it loaded in the properties of its reply", {

  reply <- declared_fields()

  expect_named(
    reply,
    c("packages", "block_registry", "modules", "plugins", "frontend",
      "options")
  )
  expect_identical(
    json(reply$packages),
    paste0(
      "{\"blockr.core\":\"", pkg_version("blockr.core"), "\",",
      "\"blockr.test\":\"0.1.0\"}"
    )
  )
  expect_named(reply$block_registry, c("count_block", "letters_block"))
})

test_that("an entry has the metadata registration gives, with link as url", {

  entry <- declared_fields()$block_registry$letters_block

  expect_identical(
    entry[c("id", "name", "description", "url", "guidance", "category")],
    list(
      id = "letters_block",
      name = "letters block",
      description = "Pick columns",
      url = "https://example.com/letters",
      guidance = "Set `columns` to the names of columns to keep.",
      category = "transform"
    )
  )
  expect_identical(json(entry$keywords), "[\"letters\",\"columns\"]")
  expect_identical(entry$icon, proto_icon("eye"))
})

test_that("an entry leaves out the metadata that registration leaves unset", {

  entry <- declared_fields()$block_registry$count_block

  expect_named(
    entry,
    c("id", "name", "description", "keywords", "category", "icon", "inputs",
      "module", "version", "schema", "state", "options", "requests",
      "output_options", "renderers")
  )
  expect_identical(json(entry$keywords), "[]")
})

test_that("an entry holds what the client cannot read off a class in R", {

  reply <- declared_fields()

  letters <- reply$block_registry$letters_block

  expect_identical(json(letters$inputs), "[\"data\"]")
  expect_match(letters$module, "^blockr-modules/[0-9a-f]{8}/letters-block.js$")
  expect_match(letters$css, "^blockr-modules/[0-9a-f]{8}/letters-block.css$")
  # The schema describes each state arg, as the module ships it.
  expect_identical(
    json(letters$schema),
    paste0(
      "{\"type\":\"object\",\"properties\":{\"columns\":{\"type\":\"array\",",
      "\"items\":{\"type\":\"string\"},\"description\":\"Columns to keep.\"},",
      "\"sep\":{\"type\":\"string\",\"description\":\"Separator.\"}}}"
    )
  )

  count <- reply$block_registry$count_block

  expect_identical(json(count$inputs), "[]")
  expect_null(count$css)
  expect_identical(json(count$schema), "{}")
})

test_that("an entry holds no block class, constructor or package", {

  entry <- declared_fields()$block_registry$letters_block

  expect_length(
    intersect(
      names(entry),
      c("block_class", "constructor", "package", "ui", "link")
    ),
    0L
  )
})

test_that("the starting state is written by the schema of the class's state", {

  reply <- declared_fields()

  # A vector of length one is an array where the schema has one.
  expect_identical(
    json(reply$block_registry$letters_block$state),
    "{\"columns\":[\"x\"],\"sep\":\",\"}"
  )

  # Without a schema, a value is written bare.
  expect_identical(json(reply$block_registry$count_block$state), "{\"n\":3}")
})

test_that("a request says what it reads, and whether volatile or an action", {

  requests <- declared_fields()$block_registry$letters_block$requests

  expect_identical(
    json(requests),
    paste0(
      "{\"columns\":{\"inputs\":[\"data\"],\"state_args\":[],\"options\":[]},",
      "\"page\":{\"inputs\":[\"data\"],\"state_args\":[\"sep\"],",
      "\"options\":[\"n_rows\"]},",
      "\"files\":{\"inputs\":[],\"state_args\":[],\"options\":[],",
      "\"volatile\":true},",
      "\"upload\":{\"inputs\":[],\"state_args\":[],\"options\":[],",
      "\"action\":true,\"file\":true}}"
    )
  )
})

test_that("R says where it serves what packages list", {

  reply <- declared_fields()

  url <- function(file) {
    paste0("^blockr.test/js/", file, "\\?v=[0-9a-f]{8}$")
  }

  # The modules that the package offers others, by ID
  expect_named(reply$modules, "blockr.test-picker")
  expect_named(reply$modules[["blockr.test-picker"]], "module")
  expect_match(reply$modules[["blockr.test-picker"]]$module, url("picker.js"))

  # The module of the class of each of the app's plugins, with the stylesheet
  # beside it
  grid <- reply$plugins[["blockr.test-grid_plugin"]]

  expect_named(grid, c("name", "version", "options", "module", "css"))
  expect_match(grid$module, url("grid.js"))
  expect_match(grid$css, url("grid.css"))

  # The module of each board option that a loaded package lists
  gap <- reply$options[["grid_gap"]]

  expect_named(gap, c("package", "module", "version"))
  expect_identical(gap$package, "blockr.test")
  expect_match(gap$module, url("grid-gap.js"))
})

test_that("R gives the app's plugins with their classes' names and versions", {

  expect_match(
    json(declared_fields()$plugins),
    paste0(
      "^\\{\"blockr.test-grid_plugin\":",
      "\\{\"name\":\"Grid\",\"version\":2,\"options\":\\[\\],\"module\":"
    )
  )
  expect_identical(json(loaded_reply()$plugins), "{}")

  # The package that the id of each plugin's class names is among those of
  # the reply, where R serves its class's module at a URL that serves nothing,
  # which it logs as a warning
  local_versions(blockr.one = "1.2.0")
  withr::local_options(blockr.log_level = "error")
  reset_logging()
  withr::defer(reset_logging())

  reply <- loaded_reply(
    plugins = list(
      `blockr.one-dag_plugin` = list(
        name = "DAG",
        version = 3L,
        options = character(),
        module = js_module("js/dag.js", package = "blockr.one")
      )
    )
  )

  expect_identical(reply$packages$blockr.one, "1.2.0")
  expect_false("plugin_versions" %in% names(reply))
})

test_that("R gives what the app's frontend reads beside the app's plugins", {

  # The frontend of blockr.test reads the title "Grid" where the script passes
  # serve() none
  expect_identical(json(declared_fields()$frontend), "{\"title\":\"Grid\"}")

  # Core's basic frontend reads nothing, and so does a block served alone
  expect_identical(json(loaded_reply()$frontend), "{}")
})

test_that("R gives the version of each board option of the app's packages", {

  expect_identical(
    lst_xtr(declared_fields()$options, "version"),
    list(grid_gap = 2L)
  )

  # Core's board options, which every app uses, at version 1, since core gives
  # them no method of `board_option_version()`
  expect_identical(
    lst_xtr(loaded_reply()$options, "version"),
    list(n_rows = 1L, dark_mode = 1L)
  )

  # A method that gives no version fails as R builds the reply
  local_listing(
    "blockr.test",
    list(options = list(bad_gap = list(module = "js/bad-gap.js")))
  )
  local_board_option_version("bad_gap", 0L)

  plugins <- list(`blockr.test-grid_plugin` = list(name = "Grid"))

  expect_error(
    loaded_reply(plugins = plugins),
    class = "board_option_version_invalid"
  )
})

test_that("an entry names the renderers of its class's outputs", {

  local_fixture_renderer_urls()

  head <- get_registry_entry("blockr.core-head_block")

  expect_identical(
    json(registry_entry_reply(head)$renderers),
    paste0(
      "[{\"id\":\"blockr.core-table_output\",",
      "\"module\":\"blockr.core/basic-table.js?v=b7e2a915\"}]"
    )
  )

  # A class that names no output family shows its results as text
  upload <- get_registry_entry("blockr.core-upload_block")

  expect_identical(
    chr_xtr(entry_renderers(upload), "id"),
    "blockr.core-text_output"
  )

  # The entry names the renderer of every family whose outputs the class's
  # family makes
  entry <- function(family) list(block_class = list(output = family))

  words <- new_renderer("words_output", js_module("words.js", package = NULL))

  words_output <- new_output_family(
    function(result, ...) {
      if (is.data.frame(result)) {
        return(table_output(result, ...))
      }
      new_block_output(list(words = result), "words_output")
    },
    "words_output",
    renderer = words,
    families = table_output
  )

  expect_identical(
    chr_xtr(entry_renderers(entry(words_output)), "id"),
    c("words_output", "blockr.core-table_output")
  )

  head_output <- new_output_family(
    function(result, ...) table_output(utils::head(result), ...),
    "head_output",
    families = table_output
  )

  expect_identical(
    chr_xtr(entry_renderers(entry(head_output)), "id"),
    "blockr.core-table_output"
  )
})

test_that("an entry names the board options that its class's UI module reads", {

  head <- get_registry_entry("blockr.core-head_block")

  expect_identical(json(registry_entry_reply(head)$options), "[]")

  # By ID, each once, as the class holds their constructors
  head$block_class$options <- list(
    new_dark_mode_option, new_n_rows_option, new_dark_mode_option
  )

  expect_identical(
    json(registry_entry_reply(head)$options),
    "[\"dark_mode\",\"n_rows\"]"
  )
})

test_that("an entry names the board options that its class's outputs read", {

  head <- get_registry_entry("blockr.core-head_block")
  upload <- get_registry_entry("blockr.core-upload_block")

  expect_identical(
    json(registry_entry_reply(head)$output_options),
    "[\"n_rows\"]"
  )
  expect_identical(json(registry_entry_reply(upload)$output_options), "[]")

  # A family that hands its results to another reads the other's board
  # options, beside its own, each once
  head_output <- new_output_family(
    function(result, ...) table_output(utils::head(result), ...),
    "head_output",
    options = list(new_dark_mode_option, new_n_rows_option),
    families = table_output
  )

  expect_identical(family_option_ids(head_output), c("dark_mode", "n_rows"))
  expect_identical(family_option_ids(text_output), character())
})

test_that("an entry of a class whose family is open names every listed one", {

  local_family_package()

  any_output <- new_output_family(
    function(result, ...) text_output(result, ...),
    "any_output",
    families = TRUE
  )

  head <- get_registry_entry("blockr.core-head_block")
  head$block_class$output <- any_output

  # The renderers and the board options of the families that core and
  # famprobe list, as the available packages list them
  expect_identical(
    chr_xtr(entry_renderers(head), "id"),
    c("blockr.core-table_output", "blockr.core-text_output",
      "blockr.core-plot_output", "famprobe-probe_output")
  )
  expect_identical(
    json(registry_entry_reply(head)$output_options),
    "[\"n_rows\",\"dark_mode\"]"
  )

  # Or of those that the app read as it started
  listed <- list(text_output = text_output)

  expect_identical(
    chr_xtr(entry_renderers(head, listed), "id"),
    "blockr.core-text_output"
  )
  expect_identical(
    json(registry_entry_reply(head, families = listed)$output_options),
    "[]"
  )
})

test_that("the renderer the deployment names for a family is in the entry", {

  local_mocked_bindings(
    pkg_version = function(pkg = parent.frame()) package_version("1.0.0")
  )

  preview <- new_renderer("preview", test_module("js/preview.js", "blockr.ui"))

  # R finds no file for the renderer's module, nor for core's frontend
  withr::local_options(
    blockr.renderers = list(table_output = preview),
    blockr.log_level = "error"
  )
  local_reset_logging()

  head <- get_registry_entry("blockr.core-head_block")

  expect_identical(entry_renderers(head), list(preview))

  # The reply names the version of the package that the renderer comes from
  reply <- loaded_reply()

  expect_identical(
    reply$block_registry[["blockr.core-head_block"]]$renderers[[1L]]$id,
    "blockr.ui-preview"
  )
  expect_identical(
    json(reply$packages),
    "{\"blockr.core\":\"1.0.0\",\"blockr.ui\":\"1.0.0\"}"
  )
})

test_that("R names the modules that every loaded package offers", {

  dir <- withr::local_tempdir()
  local_empty_declarations()

  local_mocked_bindings(
    module_file = function(module, suffix = NULL) {
      if (is.null(suffix)) file.path(dir, module_path(module)) else ""
    },
    pkg_version = function(pkg = parent.frame()) package_version("1.0.0")
  )

  dir.create(file.path(dir, "js"))

  for (pkg in c("blockr.ui", "blockr.dock", "blockr.dag")) {
    writeBin(charToRaw(pkg), file.path(dir, "js", paste0(pkg, ".js")))
    local_listing(pkg, list(modules = list(kit = paste0("js/", pkg, ".js"))))
  }

  reply <- loaded_reply()

  # By ID, the packages in order of their names, with no order of their
  # DESCRIPTION files
  expect_named(
    reply$modules,
    c("blockr.dag-kit", "blockr.dock-kit", "blockr.ui-kit")
  )
  expect_identical(
    json(reply$packages),
    paste0(
      "{\"blockr.dag\":\"1.0.0\",",
      "\"blockr.dock\":\"1.0.0\",\"blockr.ui\":\"1.0.0\"}"
    )
  )
})

test_that("building what R tells the client builds no block", {

  local_mocked_bindings(
    new_class_block = function(...) stop("A block was built.")
  )

  withr::local_options(blockr.log_level = "error")
  local_reset_logging()

  expect_no_error(reply <- loaded_reply())
  expect_setequal(names(reply$block_registry), list_blocks())
  expect_identical(
    reply$packages[["blockr.core"]],
    as.character(pkg_version("blockr.core"))
  )
})

test_that("registration refuses a starting state the client cannot get", {

  cls <- new_block_class(
    "factor_block",
    expr = function(level = factor("a")) bbquote(identity(.(level))),
    ui = js_module("factor-block.js", package = NULL)
  )

  expect_error(
    register_block(block_constructor(cls), "Factor", "Holds a factor"),
    class = "board_value_invalid"
  )
  expect_false("factor_block" %in% list_blocks())
})
