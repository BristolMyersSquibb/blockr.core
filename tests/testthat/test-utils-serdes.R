# A constructor that no longer returns a block, saved as today's files save a
# function from no package: the one class change deserialization refuses
not_a_block <- list(
  object = "blockr_ctor",
  constructor = serialize(function(...) list(), NULL),
  package = NULL,
  version = NULL
)

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

test_that("a block made by new_block() cannot be written", {
  expect_error(
    blockr_ser(new_board(c(a = new_dataset_block("iris")))),
    class = "block_ser_legacy"
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

test_that("serialization", {

  blk <- blockr_deser(v014_dataset_block("iris"))

  expect_identical(blk, new_dataset_block("iris"), ignore_function_env = TRUE)

  blks <- blockr_deser(
    v014_blocks(a = v014_dataset_block(), b = v014_subset_block())
  )

  expect_identical(
    blks,
    c(a = new_dataset_block("iris"), b = new_subset_block()),
    ignore_function_env = TRUE
  )

  expect_identical(
    blockr_deser(v014_links(ab = v014_link("a", "b"))),
    links(ab = new_link("a", "b"))
  )

  expect_identical(
    blockr_deser(v014_stacks(ab = v014_stack(c("a", "b")))),
    stacks(ab = new_stack(c("a", "b")))
  )

  ser <- v014_board(
    blocks = v014_blocks(a = v014_dataset_block(), b = v014_subset_block()),
    links = v014_links(ab = v014_link("a", "b")),
    options = v014_options(
      v014_option("show_conditions", c("warning", "error")),
      v014_option("dark_mode")
    )
  )

  expect_identical(
    blockr_deser(ser),
    new_board(
      blocks = c(a = new_dataset_block("iris"), b = new_subset_block()),
      links = links(ab = new_link("a", "b")),
      options = c(
        new_show_conditions_option(c("warning", "error")),
        new_dark_mode_option()
      )
    ),
    ignore_function_env = TRUE
  )

  blk <- v014_block("identity_block", list(block_name = "Identity"))
  blk$constructor$package <- "some_imaginary_package"

  expect_error(blockr_deser(blk), class = "blockr_deser_missing_pkg")
})

test_that("deserialization checks the kind of object, not its classes", {

  ser <- v014_dataset_block("iris")
  ser$object <- c("foo_block", "bar_block", ser$object[-1L])

  expect_identical(
    blockr_deser(ser),
    new_dataset_block("iris"),
    ignore_function_env = TRUE
  )

  ser <- v014_board(v014_blocks(a = v014_dataset_block()))
  ser$object <- c("foo_board", ser$object)

  expect_identical(
    blockr_deser(ser),
    new_board(blocks(a = new_dataset_block("iris"))),
    ignore_function_env = TRUE
  )

  ser <- v014_dataset_block("iris")
  ser$constructor <- not_a_block

  expect_error(blockr_deser(ser), class = "block_deser_class_error")
})

test_that("a board keeps its ID and name through deserialization", {

  res <- blockr_deser(
    v014_board(
      v014_blocks(a = v014_dataset_block()),
      id = "sales_review",
      name = "Q3 numbers"
    )
  )

  expect_identical(board_id(res), "sales_review")
  expect_identical(board_name(res), "Q3 numbers")
})

test_that("a board saved by blockr.core 0.1.4 keeps its name and ID", {

  # As blockr.core 0.1.4 wrote it: the name as the value of a `board_name`
  # board option, and the ID only where the app passed one
  ser <- v014_board(
    v014_blocks(a = v014_dataset_block()),
    options = v014_options(
      v014_option("board_name", "Q3 numbers", label = "Board name"),
      v014_option("show_conditions", c("warning", "error"))
    ),
    id = "sales_review"
  )

  res <- blockr_deser(ser)

  expect_identical(board_id(res), "sales_review")
  expect_identical(board_name(res), "Q3 numbers")
  expect_identical(board_option_ids(res), "show_conditions")

  ser$payload$options$payload[[1L]]$payload$value <- NULL
  ser["id"] <- list(NULL)

  res <- blockr_deser(ser)

  expect_null(board_id(res))
  expect_null(board_name(res))
})

test_that("objects without an ID deserialize without one", {

  res <- blockr_deser(
    v014_board(
      v014_blocks(a = v014_dataset_block()),
      stacks = v014_stacks(v014_stack("a"))
    )
  )

  expect_null(board_id(res))
  expect_identical(board_stack_ids(res), "")

  expect_identical(
    blockr_deser(v014_links(v014_link("a", "b"), bc = v014_link("b", "c"))),
    links(new_link("a", "b"), bc = new_link("b", "c"))
  )
})

test_that("a board option keeps its value, not its label or category", {

  opt <- blockr_deser(v014_option("n_rows", 25L, "Table options", "Rows"))

  expect_identical(opt, new_n_rows_option(25L))

  expect_identical(
    blockr_deser(v014_option("dark_mode", label = "Mode")),
    new_dark_mode_option()
  )
})

test_that("blockr_deser.list forwards `...` to per-class methods", {

  captured <- NULL

  registerS3method(
    "blockr_deser", "deser_ctx_probe",
    function(x, data, version = NULL, ...) {
      captured <<- version
      structure(list(), class = "deser_ctx_probe")
    },
    envir = asNamespace("blockr.core")
  )

  with_ctx <- blockr_deser(
    list(object = "deser_ctx_probe"),
    version = "1.2.3"
  )

  expect_s3_class(with_ctx, "deser_ctx_probe")
  expect_identical(captured, "1.2.3")

  no_ctx <- blockr_deser(list(object = "deser_ctx_probe"))

  expect_s3_class(no_ctx, "deser_ctx_probe")
  expect_null(captured)
})

test_that("a board with variadic links deserializes", {

  ser <- v014_board(
    v014_blocks(
      a = v014_dataset_block("BOD"),
      b = v014_dataset_block("BOD"),
      c = v014_block(
        c("rbind_block", "transform_block"),
        list(block_name = "Rbind")
      )
    ),
    links = v014_links(ac = v014_link("a", "c"), bc = v014_link("b", "c"))
  )

  expect_identical(
    blockr_deser(ser),
    new_board(
      blocks = c(
        a = new_dataset_block("BOD"),
        b = new_dataset_block("BOD"),
        c = new_rbind_block()
      ),
      links = links(ac = new_link("a", "c"), bc = new_link("b", "c"))
    ),
    ignore_function_env = TRUE
  )
})

test_that("blocks deser drops offending blocks when on_error is drop", {

  class_err <- v014_blocks(a = v014_dataset_block(), b = v014_subset_block())
  class_err$payload$b$constructor <- not_a_block

  expect_error(
    blockr_deser(class_err),
    class = "block_deser_class_error"
  )

  expect_error(
    blockr_deser(class_err, on_error = "abort"),
    class = "block_deser_class_error"
  )

  kept <- expect_output(
    blockr_deser(class_err, on_error = "drop"),
    "Dropping block b"
  )

  expect_true(is_blocks(kept))
  expect_identical(names(kept), "a")

  miss_pkg <- v014_blocks(a = v014_dataset_block(), b = v014_subset_block())
  miss_pkg$payload$b$constructor$package <- "some_imaginary_pkg"

  expect_error(
    blockr_deser(miss_pkg),
    class = "blockr_deser_missing_pkg"
  )

  dropped_pkg <- expect_output(
    blockr_deser(miss_pkg, on_error = "drop"),
    "Dropping block b"
  )

  expect_identical(names(dropped_pkg), "a")

  expect_error(blockr_deser(class_err, on_error = "nonsense"))
})

test_that("board deser prunes links and stacks referencing dropped blocks", {

  ser <- v014_board(
    v014_blocks(
      a = v014_dataset_block(),
      b = v014_subset_block(),
      c = v014_subset_block()
    ),
    links = v014_links(ab = v014_link("a", "b")),
    stacks = v014_stacks(
      with_a = v014_stack(c("a", "b")),
      only_c = v014_stack("c")
    )
  )

  for (id in c("b", "c")) {
    ser$payload$blocks$payload[[id]]$constructor <- not_a_block
  }

  expect_error(
    blockr_deser(ser),
    class = "block_deser_class_error"
  )

  res <- expect_output(
    blockr_deser(ser, on_error = "drop"),
    "Dropping block b"
  )

  expect_true(is_board(res))
  expect_identical(board_block_ids(res), "a")
  expect_length(board_links(res), 0L)

  # The stack with_a keeps its surviving member, and only_c, emptied, goes
  expect_identical(board_stack_ids(res), "with_a")
  expect_identical(stack_blocks(board_stacks(res)[["with_a"]]), "a")
})

test_that("deser_on_error default is sourced from a blockr_option", {

  ser <- v014_blocks(a = v014_dataset_block(), b = v014_subset_block())
  ser$payload$b$constructor <- not_a_block

  withr::with_envvar(
    c(BLOCKR_DESER_ON_ERROR = NA),
    withr::with_options(
      list(blockr.deser_on_error = NULL),
      expect_error(blockr_deser(ser), class = "block_deser_class_error")
    )
  )

  withr::with_options(
    list(blockr.deser_on_error = "drop"),
    {
      opt_res <- expect_output(blockr_deser(ser), "Dropping block b")
      expect_identical(names(opt_res), "a")
    }
  )

  withr::with_envvar(
    c(BLOCKR_DESER_ON_ERROR = "drop"),
    {
      env_res <- expect_output(blockr_deser(ser), "Dropping block b")
      expect_identical(names(env_res), "a")
    }
  )
})

test_that("board deser resolves the option and threads it to blocks", {

  ser <- v014_board(
    v014_blocks(a = v014_dataset_block(), b = v014_subset_block()),
    links = v014_links(ab = v014_link("a", "b")),
    stacks = v014_stacks(with_a = v014_stack(c("a", "b")))
  )

  ser$payload$blocks$payload$b$constructor <- not_a_block

  withr::with_envvar(
    c(BLOCKR_DESER_ON_ERROR = NA),
    withr::with_options(
      list(blockr.deser_on_error = NULL),
      expect_error(blockr_deser(ser), class = "block_deser_class_error")
    )
  )

  res <- withr::with_options(
    list(blockr.deser_on_error = "drop"),
    expect_output(blockr_deser(ser), "Dropping block b")
  )

  expect_true(is_board(res))
  expect_identical(board_block_ids(res), "a")
  expect_length(board_links(res), 0L)
  expect_identical(stack_blocks(board_stacks(res)[["with_a"]]), "a")
})
