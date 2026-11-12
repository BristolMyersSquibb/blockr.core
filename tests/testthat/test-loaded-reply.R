# The JSON that R writes for a part of what it tells the client.
json <- function(x) {
  as.character(encode_message(x))
}

test_that("R tells the client what it loaded in the properties of its reply", {

  reply <- declared_fields()

  expect_named(
    reply,
    c("packages", "block_registry", "modules", "plugins", "frontend",
      "options", "renderers")
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
    entry[c("id", "name", "description", "details", "url", "guidance",
            "category")],
    list(
      id = "letters_block",
      name = "letters block",
      description = "Pick columns",
      details = "Keeps the columns it is given.",
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
      "module", "version", "schema", "state", "arguments", "requests",
      "renderer")
  )
  expect_identical(json(entry$keywords), "[]")
})

test_that("an entry holds what the client cannot read off a class in R", {

  reply <- declared_fields()

  letters <- reply$block_registry$letters_block

  expect_identical(json(letters$inputs), "[\"data\"]")
  expect_match(letters$module, "^blockr-modules/[0-9a-f]{8}/letters-block.js$")
  expect_match(letters$css, "^blockr-modules/[0-9a-f]{8}/letters-block.css$")
  expect_identical(
    json(letters$schema),
    paste0(
      "{\"type\":\"object\",\"properties\":{\"columns\":{\"type\":\"array\",",
      "\"items\":{\"type\":\"string\"}},\"sep\":{\"type\":\"string\"}}}"
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
      c("block_class", "constructor", "package", "ui", "link", "examples")
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

test_that("an arg spec's example is written by its type", {

  reply <- declared_fields()

  expect_identical(
    json(reply$block_registry$letters_block$arguments),
    paste0(
      "{\"columns\":{\"description\":\"Columns to keep.\",\"example\":[\"x\"],",
      "\"type\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}},",
      "\"sep\":{\"description\":\"Separator.\",\"type\":{\"type\":\"string\"}}}"
    )
  )

  # The arg specs default to every state arg, each with nothing set.
  expect_identical(
    json(reply$block_registry$count_block$arguments),
    "{\"n\":{}}"
  )
})

test_that("an arg spec's type is the JSON Schema that it denotes", {

  type <- arg_object(
    pairs = arg_array(arg_object(key = arg_string(), value = arg_number())),
    mode = arg_enum("a", required = FALSE)
  )

  expect_identical(
    json(arg_type_reply(type)),
    paste0(
      "{\"type\":\"object\",\"properties\":{\"pairs\":{\"type\":\"array\",",
      "\"items\":{\"type\":\"object\",\"properties\":{\"key\":{\"type\":",
      "\"string\"},\"value\":{\"type\":\"number\"}},\"required\":[\"key\",",
      "\"value\"],\"additionalProperties\":false}},\"mode\":{\"type\":",
      "\"string\",\"enum\":[\"a\"]}},\"required\":[\"pairs\"],",
      "\"additionalProperties\":false}"
    )
  )

  # An object without properties keeps its `properties` an object.
  expect_identical(
    json(arg_type_reply(list(type = "object", properties = list()))),
    "{\"type\":\"object\",\"properties\":{}}"
  )
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
      "\"action\":true}}"
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

  expect_named(grid, c("name", "version", "module", "css"))
  expect_match(grid$module, url("grid.js"))
  expect_match(grid$css, url("grid.css"))

  # The module of each board option of the packages that the app uses
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
      "\\{\"name\":\"Grid\",\"version\":2,\"module\":"
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
    list(n_rows = 1L, show_conditions = 1L, dark_mode = 1L)
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

test_that("an entry names the renderer of its class's outputs", {

  local_fixture_renderer_urls()

  head <- get_registry_entry("blockr.core-head_block")

  expect_identical(
    json(registry_entry_reply(head, entry_renderer(head))$renderer),
    paste0(
      "{\"id\":\"blockr.core-table_output\",",
      "\"module\":\"blockr.core/basic-table.js?v=b7e2a915\"}"
    )
  )

  # A class that names no coercion shows its results as text
  upload <- get_registry_entry("blockr.core-upload_block")

  expect_identical(entry_renderer(upload)$id, "blockr.core-text_output")

  # A class whose coercion no output class gives has no renderer up front, so
  # that the client learns it from the first output
  raw <- list(
    block_class = list(output = function(result, ...) new_text_output("raw"))
  )

  expect_null(entry_renderer(raw))
  expect_null(registry_entry_reply(head, NULL)$renderer)

  # Nor has one whose output class has no renderer
  local_output_class("words_output", coercion = identity)

  expect_null(entry_renderer(list(block_class = list(output = identity))))
})

test_that("the renderer the app names for an output class is in the entry", {

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

  expect_identical(entry_renderer(head), preview)

  # The reply names the version of the package that the renderer comes from
  reply <- loaded_reply()

  expect_identical(
    reply$block_registry[["blockr.core-head_block"]]$renderer$id,
    "blockr.ui-preview"
  )
  expect_identical(
    json(reply$packages),
    "{\"blockr.core\":\"1.0.0\",\"blockr.ui\":\"1.0.0\"}"
  )
})

test_that("R names the renderers that the app names for board options", {

  # The app names one for table outputs too, which R sends with each output
  renderers <- declared_fields()$renderers

  expect_named(renderers, "blockr.test-dark")
  expect_identical(renderers[["blockr.test-dark"]]$option, "dark_mode")
  expect_match(
    renderers[["blockr.test-dark"]]$module,
    "^blockr.test/js/dark.js\\?v=[0-9a-f]{8}$"
  )

  expect_identical(json(loaded_reply()$renderers), "{}")
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

test_that("registration refuses an example the client cannot get", {

  cls <- new_block_class(
    "call_block",
    expr = function(x = 1L) bbquote(identity(.(x))),
    ui = js_module("call-block.js", package = NULL)
  )

  expect_error(
    register_block(
      block_constructor(cls),
      "Call",
      "Has a call for an example",
      arguments = new_arg_specs(x = new_arg_spec("A number", quote(x + 1)))
    ),
    class = "board_value_invalid"
  )
  expect_false("call_block" %in% list_blocks())
})
