# A block class from outside a package, whose UI module sits in `dir`, with
# the schema of its state beside it where `schema` gives one.
outside_class <- function(name, expr, dir, schema = NULL, inputs = NULL) {

  module <- file.path(dir, paste0(gsub("_", "-", name), ".js"))

  if (not_null(schema)) {
    writeLines(schema, sub("\\.js$", ".schema.json", module))
  }

  new_block_class(
    name,
    inputs = inputs,
    expr = expr,
    ui = js_module(module, package = NULL)
  )
}

test_that("a board is written as R sends it", {

  local_declared(block_registry)

  board <- new_board(
    blocks = c(
      a = new_class_head_block(n = 10L, block_name = "Ten"),
      b = new_class_head_block(direction = "tail", visible = "outputs")
    ),
    links = links(ab = new_link("a", "b", "data")),
    stacks = stacks(s1 = new_stack("a", "Input")),
    options = c(new_n_rows_option(25L), new_dark_mode_option("dark")),
    id = "sales_review",
    name = "Q3 numbers"
  )

  expect_identical(
    read_json_strictly(encode_message(blockr_ser(board))),
    read_json_strictly(
      '{
        "id": "sales_review",
        "front_end": { "id": "blockr.core-basic", "state": {}, "attrs": {} },
        "name": "Q3 numbers",
        "blocks": [
          {
            "id": "a",
            "registry_id": "head_block",
            "state": { "n": 10, "direction": "head" },
            "name": "Ten",
            "attrs": {}
          },
          {
            "id": "b",
            "registry_id": "head_block",
            "state": { "n": 6, "direction": "tail" },
            "name": "Head",
            "attrs": { "visible": ["outputs"] }
          }
        ],
        "links": [{ "id": "ab", "from": "a", "to": "b", "input": "data" }],
        "stacks": [{ "id": "s1", "name": "Input", "blocks": ["a"] }],
        "options": { "n_rows": 25, "dark_mode": "dark" }
      }'
    )
  )

  # R marks an array and writes an object as a named list, as R's writers all
  # write them
  res <- blockr_ser(board)

  expect_s3_class(res$stacks[[1L]]$blocks, "AsIs")
  expect_named(res$front_end$state, character())
})

test_that("R mints no ids and writes an empty board as such", {

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

  res <- read_json_strictly(encode_message(blockr_ser(board)))

  expect_false(any(c("id", "name") %in% names(res)))
  expect_identical(lapply(res$blocks, `[[`, "id"), list("a", "b", NULL))
  expect_identical(
    res[c("links", "stacks")],
    read_json_strictly(
      '{
        "links": [{ "from": "a", "to": "b", "input": "data" }],
        "stacks": [{ "name": "Empty", "blocks": [] }]
      }'
    )
  )

  expect_identical(
    read_json_strictly(encode_message(blockr_ser(new_board()))),
    read_json_strictly(
      '{
        "front_end": { "id": "blockr.core-basic", "state": {}, "attrs": {} },
        "blocks": [],
        "links": [],
        "stacks": [],
        "options": {}
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
    inputs = "data"
  )

  board <- new_board(
    blocks = c(
      a = block_constructor(merge)(by = "Time"),
      b = block_constructor(merge)(),
      c = block_constructor(bare)(cols = "Time")
    )
  )

  res <- read_json_strictly(encode_message(blockr_ser(board)))

  # Bare where the UI ships no schema, so that the UI takes either form
  expect_identical(
    lapply(res$blocks, `[[`, "state"),
    read_json_strictly(
      '[
        { "by": ["Time"], "all_x": false, "all_y": false },
        { "by": [], "all_x": false, "all_y": false },
        { "cols": "Time", "n": 1 }
      ]'
    )
  )
})

test_that("a block class is registered from its blocks", {

  local_declared(block_registry)

  dir <- withr::local_tempdir()

  cls <- outside_class("note_block", function(text = "") bbquote(.(text)), dir)
  new_note_block <- block_constructor(cls)

  expect_false("note_block" %in% list_blocks())

  res <- blockr_ser(new_board(c(a = new_note_block("Hi"))))

  expect_identical(res$blocks[[1L]]$registry_id, "note_block")
  expect_identical(registry_id_from_block(cls), "note_block")

  entry <- get_registry_entry("note_block")

  expect_identical(entry$name, "Note")
  expect_null(entry$package)

  # A class registered already keeps its entry
  unregister_blocks("note_block")
  register_block(new_note_block, "Notes", "Free text", uid = "text_note")

  res <- blockr_ser(new_board(c(a = new_note_block("Hi"))))

  expect_identical(res$blocks[[1L]]$registry_id, "text_note")
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

  expect_identical(res$blocks[[1L]]$state$x, list(a = "x", b = "y"))

  expect_error(
    blockr_ser(new_board(options = new_n_rows_option(as.Date("2024-01-01")))),
    "The value of board option n_rows holds a value of class Date",
    class = "board_value_invalid"
  )

  expect_error(
    blockr_ser(new_board(views = list(fun = identity))),
    "The front-end's state views holds a function",
    class = "board_value_invalid"
  )

  expect_error(
    blockr_ser(new_board(list(), list(), list(), list(), "unnamed")),
    class = "board_front_end_state_invalid"
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
    read_json_strictly(encode_message(blockr_ser(board)))$options,
    read_json_strictly('{ "show_conditions": ["error"], "n_rows": 5 }')
  )

  # The schema next to core's runtime, which is the first that R applies after
  # the board's
  options <- startup_schemas()[[1L]]$properties$options$properties

  expect_setequal(names(options), c("n_rows", "show_conditions", "dark_mode"))
  expect_identical(options$show_conditions$type, "array")
})

test_that("the front-end's state is written by the schemas packages ship", {

  local_declared(declared_startup_modules)
  local_declared(declared_front_ends)

  dir <- withr::local_tempdir()

  writeLines(
    '{
      "type": "object",
      "properties": {
        "front_end": {
          "type": "object",
          "properties": {
            "state": {
              "type": "object",
              "properties": {
                "panels": {
                  "type": "object",
                  "additionalProperties": {
                    "type": "object",
                    "properties": {
                      "title": { "type": "string" },
                      "blocks": {
                        "type": "array",
                        "items": { "type": "string" }
                      }
                    }
                  }
                },
                "layout": {
                  "type": "object",
                  "properties": {
                    "pinned": { "type": "array", "items": { "type": "string" } }
                  }
                },
                "active": { "type": "string" }
              }
            }
          }
        },
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
  register_front_end("grid", test_module("js/grid.js"), class = "grid_board")

  board <- new_board(
    options = new_board_option("tags", "draft"),
    panels = list(p1 = list(title = "Main", blocks = "a"), p2 = list()),
    layout = list(pinned = "a", note = "x"),
    active = "p1",
    widths = list(),
    class = "grid_board"
  )

  # What the schema leaves out keeps the shape that R's writer gives it
  expect_identical(
    read_json_strictly(encode_message(blockr_ser(board))),
    read_json_strictly(
      '{
        "front_end": {
          "id": "blockr.test-grid",
          "state": {
            "panels": {
              "p1": { "title": "Main", "blocks": ["a"] },
              "p2": {}
            },
            "layout": { "pinned": ["a"], "note": "x" },
            "active": "p1",
            "widths": []
          },
          "attrs": {}
        },
        "blocks": [],
        "links": [],
        "stacks": [],
        "options": { "tags": ["draft"] }
      }'
    )
  )

  # An empty collection is an object, and a part the board lacks stays out
  board <- new_board(panels = list(), class = "grid_board")

  expect_identical(
    read_json_strictly(encode_message(blockr_ser(board)))$front_end$state,
    read_json_strictly('{ "panels": {} }')
  )
})

test_that("R applies the schema of every package that has one", {

  local_declared(declared_startup_modules)

  dir <- withr::local_tempdir()

  writeLines(
    '{
      "properties": {
        "front_end": {
          "properties": {
            "state": {
              "properties": { "views": { "type": "array" } }
            }
          }
        }
      }
    }',
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

  res <- read_json_strictly(encode_message(blockr_ser(board)))

  expect_identical(
    res$front_end$state,
    read_json_strictly('{ "views": ["main"] }')
  )
  expect_identical(res$options, read_json_strictly('{ "tags": ["draft"] }'))
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
    blockr_ser(links(ab = new_link("a", "b"), new_link("b", "c"))),
    list(
      list(id = "ab", from = "a", to = "b", input = ""),
      list(from = "b", to = "c", input = "")
    )
  )

  expect_identical(blockr_ser(new_show_conditions_option("error")), "error")
  expect_identical(blockr_ser(list(a = 1L)), list(a = 1L))
})
