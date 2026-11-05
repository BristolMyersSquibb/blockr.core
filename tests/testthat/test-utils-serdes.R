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

  # The block class is from no package, so the board's content is core's alone
  expect_identical(
    res$packages,
    list(blockr.core = as.character(pkg_version("blockr.core")))
  )

  expect_identical(
    res$board,
    read_json_strictly(
      '{
        "id": "sales_review",
        "class": ["board"],
        "versions": { "board": 1 },
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
        "stacks": { "s1": { "name": "Input", "blocks": ["a"] } },
        "options": {
          "n_rows": { "value": 25, "version": 1 },
          "dark_mode": { "value": "dark", "version": 1 }
        }
      }'
    )
  )

  # R marks an array and writes an object as a named list, as R's writers all
  # write them
  res <- blockr_ser(board)$board

  expect_s3_class(res$stacks$s1$blocks, "AsIs")
  expect_s3_class(res$class, "AsIs")
  expect_named(res$options, c("n_rows", "dark_mode"))
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
        read_json_strictly('[{ "name": "Empty", "blocks": [] }]'),
        board_stack_ids(board)
      )
    )
  )

  expect_identical(
    read_json_strictly(encode_message(blockr_ser(new_board(id = "empty")))),
    read_json_strictly(
      paste0(
        '{
          "format": 1,
          "packages": { "blockr.core": "',
        as.character(pkg_version("blockr.core")),
        '" },
          "board": {
            "id": "empty",
            "class": ["board"],
            "versions": { "board": 1 },
            "name": "Empty",
            "blocks": {},
            "links": {},
            "stacks": {},
            "options": {}
          }
        }'
      )
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

  expect_error(
    blockr_ser(new_board(views = list(fun = identity))),
    "The board's property views holds a function",
    class = "board_value_invalid"
  )

  expect_error(
    blockr_ser(new_board(list(), list(), list(), list(), "unnamed")),
    class = "board_properties_invalid"
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

  # The schema next to core's runtime, which is the first that R applies after
  # the JSON board's
  options <- startup_schemas()[[1L]]$properties$options$properties

  expect_setequal(names(options), c("n_rows", "show_conditions", "dark_mode"))
  expect_identical(options$show_conditions$type, "array")
})

test_that("a board's class and its properties are written by the schemas", {

  local_declared(declared_startup_modules)

  dir <- withr::local_tempdir()

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
        "pinned": { "type": "string" },
        "options": {
          "type": "object",
          "properties": {
            "tags": { "type": "array", "items": { "type": "string" } }
          }
        }
      }
    }',
    file.path(dir, "startup.schema.json")
  )

  local_mocked_bindings(
    module_file = function(module, suffix) {
      if (identical(module$package, "blockr.test")) {
        return(file.path(dir, "startup.schema.json"))
      }
      ""
    }
  )

  register_startup_module(test_module("js/startup.js"))
  local_board_option_version("tags", 2L)

  board <- new_board(
    options = new_board_option("tags", "draft"),
    notes = list(n1 = list(text = "To check", blocks = "a"), n2 = list()),
    flags = list(starred = "a", note = "x"),
    pinned = "n1",
    counts = list(),
    id = "grid",
    class = "grid_board"
  )

  # The properties come after core's, in their order, what the schema leaves
  # out keeps the shape that R's writer gives it, and a board option's value
  # is shaped by the schema before it is recorded with its version
  expect_identical(
    read_json_strictly(encode_message(blockr_ser(board)))$board,
    read_json_strictly(
      '{
        "id": "grid",
        "class": ["grid_board", "board"],
        "versions": { "board": 1 },
        "name": "Grid",
        "blocks": {},
        "links": {},
        "stacks": {},
        "options": { "tags": { "value": ["draft"], "version": 2 } },
        "notes": {
          "n1": { "text": "To check", "blocks": ["a"] },
          "n2": {}
        },
        "flags": { "starred": ["a"], "note": "x" },
        "pinned": "n1",
        "counts": []
      }'
    )
  )

  # An empty collection is an object, and a property the board lacks stays out,
  # while a class that no package defines is written all the same
  board <- new_board(notes = list(), class = c("my_board", "grid_board"))
  res <- read_json_strictly(encode_message(blockr_ser(board)))$board

  expect_identical(
    res[c("class", "notes")],
    read_json_strictly(
      '{ "class": ["my_board", "grid_board", "board"], "notes": {} }'
    )
  )
  expect_false("pinned" %in% names(res))
})

test_that("R applies the schema of every package that has one", {

  local_declared(declared_startup_modules)

  dir <- withr::local_tempdir()

  writeLines(
    '{ "properties": { "views": { "type": "array" } } }',
    file.path(dir, "views.schema.json")
  )

  writeLines(
    '{
      "properties": {
        "options": { "properties": { "tags": { "type": "array" } } }
      }
    }',
    file.path(dir, "tags.schema.json")
  )

  files <- list(
    blockr.one = "views.schema.json",
    blockr.two = "tags.schema.json"
  )

  local_mocked_bindings(
    module_file = function(module, suffix) {
      file <- files[[coal(module$package, "")]]
      if (is.null(file)) "" else file.path(dir, file)
    }
  )

  register_startup_module(test_module("js/startup.js", "blockr.one"))
  register_startup_module(test_module("js/startup.js", "blockr.two"))

  board <- new_board(
    options = new_board_option("tags", "draft"),
    views = "main"
  )

  res <- read_json_strictly(encode_message(blockr_ser(board)))$board

  expect_identical(res["views"], read_json_strictly('{ "views": ["main"] }'))
  expect_identical(
    res$options,
    read_json_strictly('{ "tags": { "value": ["draft"], "version": 1 } }')
  )
})

test_that("a JSON board records the package of each class and block class", {

  local_versions(blockr.one = "1.2.0", blockr.two = "0.3.0")
  local_board_front_end("one_board", "blockr.one-grid", "blockr.one", 3L)

  core <- as.character(pkg_version("blockr.core"))

  # The package whose namespace holds a class's method of board_front_end(),
  # while a class without one of its own, as my_board, comes from no package
  board <- new_board(
    c(a = new_dataset_block()),
    class = c("my_board", "one_board")
  )

  res <- blockr_ser(board)

  expect_identical(res$packages, list(blockr.core = core, blockr.one = "1.2.0"))

  # Beside the version of each class that names a front-end, by class
  expect_identical(res$board$versions, list(one_board = 3L, board = 1L))

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

  expect_identical(
    board_packages(list(class = "board", blocks = blocks)),
    list(blockr.core = core, blockr.two = "0.3.0")
  )
})

test_that("the schema next to a startup module is a JSON object", {

  dir <- withr::local_tempdir()
  file <- file.path(dir, "startup.schema.json")

  for (json in c('["views"]', '"views"', "3")) {
    writeLines(json, file)
    expect_error(
      read_startup_schema(file, "blockr.test"),
      "The schema that package blockr.test puts next to its startup module",
      class = "startup_schema_invalid"
    )
  }

  writeLines("{}", file)

  expect_length(read_startup_schema(file, "blockr.test"), 0L)
})

test_that("a board's parts are written by their methods", {

  expect_identical(
    blockr_ser(new_link("a", "b", "x")),
    list(from = "a", to = "b", input = "x")
  )

  expect_identical(
    blockr_ser(new_stack(c("a", "b"), "Input")),
    list(name = "Input", blocks = c("a", "b"))
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
})
