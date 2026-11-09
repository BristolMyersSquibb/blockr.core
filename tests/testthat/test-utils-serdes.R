# A block class from outside a package, whose UI module sits in `dir`, with
# the schema of its state beside it where `schema` gives one.
outside_class <- function(name, expr, dir, schema = NULL, inputs = NULL,
                          version = 1L) {

  module <- file.path(dir, paste0(gsub("_", "-", name), ".js"))

  if (not_null(schema)) {
    writeLines(schema, sub("\\.js$", ".schema.json", module))
  }

  new_block_class(
    name,
    inputs = inputs,
    expr = expr,
    ui = js_module(module, package = NULL),
    version = version
  )
}

test_that("a board is written as a JSON board", {

  local_declared(block_registry)

  board <- new_board(
    blocks = c(
      a = new_class_head_block(n = 10L, block_name = "Ten"),
      b = new_class_head_block(direction = "tail")
    ),
    links = links(ab = new_link("a", "b", "data")),
    stacks = stacks(s1 = new_stack("a", "Input")),
    options = c(new_n_rows_option(25L), new_dark_mode_option("dark")),
    id = "sales_review",
    name = "Q3 numbers"
  )

  res <- read_json_strictly(encode_message(blockr_ser(board)))

  expect_named(res, c("format", "packages", "board"))
  expect_identical(res$format, 1)

  # The block class is from no package and the board holds no plugin, so the
  # JSON board records no package
  expect_identical(res$packages, set_names(list(), character()))

  expect_identical(
    res$board,
    read_json_strictly(
      '{
        "id": "sales_review",
        "versions": {},
        "name": "Q3 numbers",
        "blocks": {
          "a": {
            "registry_id": "head_block",
            "version": 1,
            "state": { "n": 10, "direction": "head" },
            "name": "Ten"
          },
          "b": {
            "registry_id": "head_block",
            "version": 1,
            "state": { "n": 6, "direction": "tail" },
            "name": "Head"
          }
        },
        "links": { "ab": { "from": "a", "to": "b", "input": "data" } },
        "stacks": {
          "s1": { "name": "Input", "blocks": ["a"], "color": null }
        },
        "options": {
          "n_rows": { "value": 25, "version": 1 },
          "dark_mode": { "value": "dark", "version": 1 }
        },
        "plugins": {}
      }'
    )
  )

  # R marks an array and writes an object as a named list, as R's writers all
  # write them
  res <- blockr_ser(board)$board

  expect_s3_class(res$stacks$s1$blocks, "AsIs")
  expect_named(res$options, c("n_rows", "dark_mode"))
  expect_identical(res$plugins, set_names(list(), character()))
})

test_that("a JSON board holds every ID, and an empty board empty objects", {

  local_declared(block_registry)

  board <- new_board(
    blocks = c(
      a = new_class_head_block(),
      b = new_class_head_block(),
      new_class_head_block()
    ),
    links = links(new_link("a", "b", "data")),
    stacks = list(new_stack(character(), "Empty"))
  )

  res <- read_json_strictly(encode_message(blockr_ser(board)))$board

  expect_identical(res$id, board_id(board))
  expect_identical(res$name, board_name(board))
  expect_named(res$blocks, board_block_ids(board))
  expect_identical(
    unname(res[c("links", "stacks")]),
    list(
      set_names(
        read_json_strictly('[{ "from": "a", "to": "b", "input": "data" }]'),
        board_link_ids(board)
      ),
      set_names(
        read_json_strictly(
          '[{ "name": "Empty", "blocks": [], "color": null }]'
        ),
        board_stack_ids(board)
      )
    )
  )

  expect_identical(
    read_json_strictly(encode_message(blockr_ser(new_board(id = "empty")))),
    read_json_strictly(
      '{
        "format": 1,
        "packages": {},
        "board": {
          "id": "empty",
          "versions": {},
          "name": "Empty",
          "blocks": {},
          "links": {},
          "stacks": {},
          "options": {},
          "plugins": {}
        }
      }'

    )
  )
})

test_that("a block's state is written by the schema its class's UI ships", {

  local_declared(block_registry)

  dir <- withr::local_tempdir()

  merge <- outside_class(
    "join_block",
    function(by = character(), all_x = FALSE, all_y = FALSE) {
      bbquote(merge(.(x), .(y), by = .(by), all.x = .(all_x)))
    },
    dir,
    schema = '{
      "type": "object",
      "properties": {
        "by": { "type": "array", "items": { "type": "string" } },
        "all_x": { "type": "boolean" },
        "all_y": { "type": "boolean" }
      }
    }',
    inputs = c("x", "y")
  )

  bare <- outside_class(
    "bare_block",
    function(cols = character(), n = 1L) bbquote(utils::head(.(data), .(n))),
    dir,
    inputs = "data",
    version = 3L
  )

  board <- new_board(
    blocks = c(
      a = block_constructor(merge)(by = "Time"),
      b = block_constructor(merge)(),
      c = block_constructor(bare)(cols = "Time")
    )
  )

  res <- read_json_strictly(encode_message(blockr_ser(board)))$board

  # Bare where the UI ships no schema, so that the UI takes either form
  expect_identical(
    lapply(res$blocks, `[[`, "state"),
    read_json_strictly(
      '{
        "a": { "by": ["Time"], "all_x": false, "all_y": false },
        "b": { "by": [], "all_x": false, "all_y": false },
        "c": { "cols": "Time", "n": 1 }
      }'
    )
  )

  # Each block records the version of its class's state
  expect_identical(
    lapply(res$blocks, `[[`, "version"),
    list(a = 1, b = 1, c = 3)
  )
})

test_that("a block class is registered from its blocks", {

  local_declared(block_registry)

  dir <- withr::local_tempdir()

  cls <- outside_class("note_block", function(text = "") bbquote(.(text)), dir)
  new_note_block <- block_constructor(cls)

  expect_false("note_block" %in% list_blocks())

  res <- blockr_ser(new_board(c(a = new_note_block("Hi"))))$board

  expect_identical(res$blocks$a$registry_id, "note_block")
  expect_identical(registry_id_from_block(cls), "note_block")

  entry <- get_registry_entry("note_block")

  expect_identical(entry$name, "Note")
  expect_null(entry$package)

  # A class registered already keeps its entry
  unregister_blocks("note_block")
  register_block(new_note_block, "Notes", "Free text", uid = "text_note")

  res <- blockr_ser(new_board(c(a = new_note_block("Hi"))))$board

  expect_identical(res$blocks$a$registry_id, "text_note")
  expect_false("note_block" %in% list_blocks())

  # Another class of the same name takes no entry from it
  other <- outside_class("text_note", function() bbquote(NULL), dir)

  expect_error(
    blockr_ser(new_board(c(a = block_constructor(other)()))),
    class = "block_already_in_registry"
  )
})

test_that("a board that the format cannot hold fails as it is written", {

  local_declared(block_registry)

  dir <- withr::local_tempdir()

  cls <- outside_class("any_block", function(x = NULL) "NULL", dir)
  new_any_block <- block_constructor(cls)

  ser <- function(x) blockr_ser(new_board(c(a = new_any_block(x))))

  values <- list(
    list(function() 1, "a function"),
    list(list(fun = mean), "a function"),
    list(factor("a"), "a value of class factor"),
    list(data.frame(a = 1), "a value of class data.frame"),
    list(new.env(), "a value of type environment"),
    list(quote(x), "a value of type symbol"),
    list(matrix(1:4, 2L), "a value with the attribute dim")
  )

  for (value in values) {
    expect_error(
      ser(value[[1L]]),
      paste0("The state of block \"Any\" holds ", value[[2L]]),
      class = "board_value_invalid"
    )
  }

  # A named vector is written as an object, as R decodes one
  res <- ser(c(a = "x", b = "y"))

  expect_identical(res$board$blocks$a$state$x, list(a = "x", b = "y"))

  expect_error(
    blockr_ser(new_board(options = new_n_rows_option(as.Date("2024-01-01")))),
    "The value of board option n_rows holds a value of class Date",
    class = "board_value_invalid"
  )

  notes <- plugin_ctor(function(fun = NULL) NULL, "notes_plugin")

  expect_error(
    blockr_ser(new_board(plugins = notes(identity))),
    "The state of plugin \"Notes\" holds a function",
    class = "board_value_invalid"
  )
})

test_that("the board holds the board options the script gives, by schema", {

  board <- new_board(
    options = c(
      new_show_conditions_option("error"),
      new_n_rows_option(5L),
      new_dark_mode_option()
    )
  )

  expect_identical(
    read_json_strictly(encode_message(blockr_ser(board)))$board$options,
    read_json_strictly(
      '{
        "show_conditions": { "value": ["error"], "version": 1 },
        "n_rows": { "value": 5, "version": 1 }
      }'
    )
  )

  # Core puts the schema of each of its board options next to its runtime
  for (id in c("n_rows", "show_conditions", "dark_mode")) {
    expect_type(board_option_schema(id), "list")
  }

  expect_identical(board_option_schema("show_conditions")$type, "array")
})

test_that("a plugin's state is written by the schema of its class", {

  local_declared(declared_startup_modules)

  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "js"))

  writeLines(
    '{
      "type": "object",
      "properties": {
        "notes": {
          "type": "object",
          "additionalProperties": {
            "type": "object",
            "properties": {
              "text": { "type": "string" },
              "blocks": { "type": "array", "items": { "type": "string" } }
            }
          }
        },
        "flags": {
          "type": "object",
          "properties": {
            "starred": { "type": "array", "items": { "type": "string" } }
          }
        },
        "pinned": { "type": "string" }
      }
    }',
    file.path(dir, "js", "notes_plugin.schema.json")
  )

  writeLines(
    '{ "type": "array", "items": { "type": "string" } }',
    file.path(dir, "js", "tags_option.schema.json")
  )

  # The files of blockr.test, and none of core's
  local_mocked_bindings(
    package_file = function(path, package) {
      path <- file.path(dir, path)
      if (identical(package, "blockr.test") && file.exists(path)) path else ""
    }
  )

  register_startup_module(test_module("js/startup.js"))
  local_board_option_version("tags", 2L)
  local_versions(blockr.test = "0.1.0")

  notes <- plugin_ctor(
    function(notes = list(), flags = list(), pinned = "", counts = list()) NULL,
    "notes_plugin"
  )
  grid <- plugin_ctor(function(cols = "a") NULL, "grid_plugin")

  board <- new_board(
    options = new_board_option("tags", "draft"),
    plugins = list(
      todo = notes(
        notes = list(n1 = list(text = "To check", blocks = "a"), n2 = list()),
        flags = list(starred = "a", note = "x"),
        pinned = "n1"
      ),
      done = notes(plugin_name = "Done"),
      grid()
    ),
    id = "grid"
  )

  # Each plugin of the class is written by the schema of the class's state,
  # whatever its id, what the schema leaves out keeps the shape that R's writer
  # gives it, and the state of a class that no package gives a schema is bare,
  # while a board option's value is shaped by its schema before it is recorded
  # with its version
  expect_identical(
    read_json_strictly(encode_message(blockr_ser(board)))$board,
    read_json_strictly(
      '{
        "id": "grid",
        "versions": {
          "blockr.test-notes_plugin": 1,
          "blockr.test-grid_plugin": 1
        },
        "name": "Grid",
        "blocks": {},
        "links": {},
        "stacks": {},
        "options": { "tags": { "value": ["draft"], "version": 2 } },
        "plugins": {
          "todo": {
            "class": "blockr.test-notes_plugin",
            "name": "Notes",
            "state": {
              "notes": {
                "n1": { "text": "To check", "blocks": ["a"] },
                "n2": {}
              },
              "flags": { "starred": ["a"], "note": "x" },
              "pinned": "n1",
              "counts": []
            }
          },
          "done": {
            "class": "blockr.test-notes_plugin",
            "name": "Done",
            "state": { "notes": {}, "flags": {}, "pinned": "", "counts": [] }
          },
          "grid": {
            "class": "blockr.test-grid_plugin",
            "name": "Grid",
            "state": { "cols": "a" }
          }
        }
      }'
    )
  )

  # A plugin without state holds an empty object
  empty <- plugin_ctor(function() NULL, "empty_plugin")
  res <- blockr_ser(new_board(plugins = empty()))
  res <- read_json_strictly(encode_message(res))

  expect_identical(
    res$board$plugins$empty$state,
    set_names(list(), character())
  )
})

test_that("R finds a class's schema next to its package's startup module", {

  local_declared(declared_startup_modules)

  dirs <- list(
    blockr.one = withr::local_tempdir(),
    blockr.two = withr::local_tempdir()
  )

  for (dir in dirs) {
    dir.create(file.path(dir, "js"))
  }

  writeLines(
    '{ "properties": { "views": { "type": "array" } } }',
    file.path(dirs$blockr.one, "js", "views_plugin.schema.json")
  )

  writeLines(
    '{ "type": "array" }',
    file.path(dirs$blockr.two, "js", "tags_option.schema.json")
  )

  local_mocked_bindings(
    package_file = function(path, package) {
      dir <- dirs[[coal(package, "")]]
      path <- if (is.null(dir)) "" else file.path(dir, path)
      if (nzchar(path) && file.exists(path)) path else ""
    }
  )

  register_startup_module(test_module("js/startup.js", "blockr.one"))
  register_startup_module(test_module("js/startup.js", "blockr.two"))

  local_versions(blockr.one = "0.1.0", blockr.three = "0.1.0")

  views <- plugin_ctor(
    function(views = "main") NULL,
    "views_plugin",
    "blockr.one"
  )

  # A class of the same name from a package that ships no schema
  other <- plugin_ctor(
    function(views = "main") NULL,
    "views_plugin",
    "blockr.three"
  )

  board <- new_board(
    options = new_board_option("tags", "draft"),
    plugins = list(views(), other = other())
  )

  res <- read_json_strictly(encode_message(blockr_ser(board)))$board

  expect_identical(
    res$plugins$views$state,
    read_json_strictly('{ "views": ["main"] }')
  )
  expect_identical(
    res$plugins$other$state,
    read_json_strictly('{ "views": "main" }')
  )

  # A board option's ID names no package, so its schema is the one next to
  # any startup module
  expect_identical(
    res$options,
    read_json_strictly('{ "tags": { "value": ["draft"], "version": 1 } }')
  )
})

test_that("a JSON board records the package and version of each plugin class", {

  local_versions(blockr.one = "1.2.0", blockr.two = "0.3.0")
  local_plugin_version("dag_plugin", 3L, "blockr.one")

  core <- as.character(pkg_version("blockr.core"))

  dag <- plugin_ctor(function() NULL, "dag_plugin", "blockr.one")
  md <- plugin_ctor(function() NULL, "md_plugin", "blockr.two")

  board <- new_board(
    c(a = new_dataset_block()),
    plugins = list(dag(), notes = md(), report = md())
  )

  res <- blockr_ser(board)

  # The package that the id of each plugin's class names, beside that of each
  # block's class
  expect_identical(
    res$packages,
    list(blockr.core = core, blockr.one = "1.2.0", blockr.two = "0.3.0")
  )

  # The version of each plugin class, by the class's id, which is 1 for a
  # class without a method of plugin_version()
  expect_identical(
    res$board$versions,
    list(`blockr.one-dag_plugin` = 3L, `blockr.two-md_plugin` = 1L)
  )

  # The package of a block's class is the one its registry entry names
  local_mocked_bindings(
    get_registry_entry = function(uid) {
      list(package = if (startsWith(uid, "blockr.two-")) "blockr.two")
    }
  )

  blocks <- list(
    a = list(registry_id = "blockr.two-tag_block"),
    b = list(registry_id = "note_block")
  )

  plugins <- list(dag = list(class = "blockr.one-dag_plugin"))

  expect_identical(
    board_packages(list(blocks = blocks, plugins = plugins)),
    list(blockr.one = "1.2.0", blockr.two = "0.3.0")
  )
})

test_that("a board's parts are written by their methods", {

  expect_identical(
    blockr_ser(new_link("a", "b", "x")),
    list(from = "a", to = "b", input = "x")
  )

  expect_identical(
    blockr_ser(new_stack(c("a", "b"), "Input")),
    list(name = "Input", blocks = c("a", "b"), color = NA_character_)
  )

  expect_identical(
    blockr_ser(new_stack("a", "Input", color = "#66c2a5")),
    list(name = "Input", blocks = "a", color = "#66c2a5")
  )

  expect_identical(
    blockr_ser(links(ab = new_link("a", "b"), bc = new_link("b", "c"))),
    list(
      ab = list(from = "a", to = "b", input = ""),
      bc = list(from = "b", to = "c", input = "")
    )
  )

  expect_identical(blockr_ser(new_show_conditions_option("error")), "error")
  expect_identical(blockr_ser(list(a = 1L)), list(a = 1L))

  notes <- plugin_ctor(function(text = "") NULL, "notes_plugin")

  expect_identical(
    blockr_ser(notes("To do", plugin_name = "Todo")),
    list(
      class = "blockr.test-notes_plugin",
      name = "Todo",
      state = list(text = "To do")
    )
  )
})
