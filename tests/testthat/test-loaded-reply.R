# The JSON that R writes for a part of what it tells the client.
json <- function(x) {
  as.character(encode_message(x))
}

test_that("R tells the client what it loaded in the properties of its reply", {

  reply <- declared_fields()

  expect_named(
    reply,
    c("packages", "block_registry", "startup_modules", "modules",
      "plugin_versions", "option_versions", "renderers")
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

test_that("R says where it serves what packages declare", {

  reply <- declared_fields()

  url <- function(file) {
    paste0("^blockr.test/js/", file, "\\?v=[0-9a-f]{8}$")
  }

  expect_length(reply$startup_modules, 1L)
  expect_named(reply$startup_modules[[1L]], c("package", "module"))
  expect_identical(reply$startup_modules[[1L]]$package, "blockr.test")
  expect_match(reply$startup_modules[[1L]]$module, url("startup.js"))

  # The module that the package's plugin class names is a module like any
  # other
  expect_named(reply$modules, c("blockr.test-grid", "blockr.test-picker"))
  expect_named(reply$modules[["blockr.test-grid"]], c("module", "css"))
  expect_match(reply$modules[["blockr.test-grid"]]$module, url("grid.js"))
  expect_match(reply$modules[["blockr.test-grid"]]$css, url("grid.css"))
  expect_match(reply$modules[["blockr.test-picker"]]$module, url("picker.js"))
})

test_that("R gives the version of each plugin class that a package versions", {

  expect_identical(
    json(declared_fields()$plugin_versions),
    "{\"blockr.test-grid_plugin\":2}"
  )
  expect_identical(json(plugin_versions()), "{}")

  # By the class's id, whose package is the one whose namespace holds the
  # method, which names the package among those of the reply
  local_plugin_version("dag_plugin", 3L, "blockr.one")
  local_versions(blockr.one = "1.2.0")

  expect_identical(plugin_versions(), list(`blockr.one-dag_plugin` = 3L))
  expect_identical(loaded_reply()$packages$blockr.one, "1.2.0")

  # A method that an app script defines, from no namespace, gives no version
  method <- function(x, ...) 2L
  environment(method) <- globalenv()
  ns <- asNamespace("blockr.core")
  registerS3method("plugin_version", "mine_plugin", method, envir = ns)
  withr::defer(
    rm(
      list = "plugin_version.mine_plugin",
      envir = get(".__S3MethodsTable__.", envir = ns)
    )
  )

  expect_identical(plugin_versions(), list(`blockr.one-dag_plugin` = 3L))

  # A method that gives no version fails as R builds the reply
  local_plugin_version("bad_plugin", 0L, "blockr.bad")

  expect_error(plugin_versions(), class = "plugin_version_invalid")
})

test_that("R gives the version of each board option that a package versions", {

  expect_identical(json(declared_fields()$option_versions), "{\"grid_gap\":2}")
  expect_identical(json(option_versions()), "{}")

  # A method that gives no version fails as R builds the reply
  local_board_option_version("bad_gap", 0L)

  expect_error(option_versions(), class = "board_option_version_invalid")
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
  local_logging()

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

test_that("R keeps the startup modules in the order packages declare them", {

  dir <- withr::local_tempdir()
  local_empty_declarations()

  local_mocked_bindings(
    module_file = function(module, suffix = NULL) {
      if (is.null(suffix)) file.path(dir, module_path(module)) else ""
    },
    pkg_version = function(pkg = parent.frame()) package_version("1.0.0")
  )

  for (pkg in c("blockr.ui", "blockr.dock", "blockr.dag")) {
    dir.create(file.path(dir, "js"), showWarnings = FALSE)
    writeBin(charToRaw(pkg), file.path(dir, "js", "startup.js"))
    register_startup_module(js_module("js/startup.js", package = pkg))
  }

  reply <- loaded_reply()

  expect_identical(
    chr_xtr(reply$startup_modules, "package"),
    c("blockr.ui", "blockr.dock", "blockr.dag")
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
  local_logging()

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
