test_that("an output is a named list of a class ending in block_output", {

  out <- new_block_output(list(text = "Hi", n = 2L),
                          c("sub_output", "x_output"))

  expect_s3_class(out, c("sub_output", "x_output", "block_output"),
                  exact = TRUE)
  expect_identical(out$text, "Hi")

  expect_s3_class(new_block_output(list(), "empty_output"), "block_output")

  expect_error(new_block_output("Hi", "x_output"),
               class = "block_output_invalid")
  expect_error(new_block_output(list("Hi"), "x_output"),
               class = "block_output_invalid")
  expect_error(new_block_output(list(a = 1, a = 2), "x_output"),
               class = "block_output_invalid")
  expect_error(new_block_output(datasets::BOD, "x_output"),
               class = "block_output_invalid")
  expect_error(new_block_output(list(renderer = "a"), "x_output"),
               class = "block_output_invalid")
  expect_error(new_block_output(list(a = 1), character()),
               class = "block_output_invalid")
  expect_error(new_block_output(list(a = 1), c("x_output", "block_output")),
               class = "block_output_invalid")
  expect_error(new_block_output(list(a = 1), NA_character_),
               class = "block_output_invalid")

  body <- new_reply_body(as.raw(1:3), "image/png")

  expect_error(
    new_block_output(list(a = body, b = body), "x_output"),
    class = "block_output_invalid"
  )
})

test_that("outputs print their class, properties and body", {

  body <- new_reply_body(as.raw(seq_len(2000L) %% 256L), "image/png")

  expect_snapshot({
    new_block_output(list(text = "Hi", n = 2L), "x_output")
    new_block_output(list(img = body, width = 1L), "x_output")
    body
    new_reply_body(charToRaw("a,b"), "text/csv", name = "data.csv")
  })
})

test_that("a reply body is made from raw bytes or a file", {

  bytes <- charToRaw("a,b\n1,2\n")
  body <- new_reply_body(bytes, "text/csv", name = "data.csv")

  expect_s3_class(body, "reply_body")
  expect_identical(reply_body_bytes(body), bytes)
  expect_identical(body_announcement(body),
                   list(type = "text/csv", name = "data.csv"))

  file <- withr::local_tempfile(fileext = ".csv")
  writeBin(bytes, file)

  body <- new_reply_body(file, "text/csv")

  expect_identical(body$content, normalizePath(file))
  expect_identical(reply_body_bytes(body), bytes)
  expect_identical(body_announcement(body), list(type = "text/csv"))

  expect_error(new_reply_body("no/such/file", "text/csv"),
               class = "reply_body_invalid")
  expect_error(new_reply_body(1:3, "text/csv"),
               class = "reply_body_invalid")
  expect_error(new_reply_body(bytes, ""), class = "reply_body_invalid")
  expect_error(new_reply_body(bytes, "text/csv", name = 1),
               class = "reply_body_invalid")
})

test_that("an output class gives the schema of its outputs by a method", {

  local_output_class(
    "tags_output",
    schema = list(
      type = "object",
      properties = list(tags = list(type = "array"))
    )
  )

  out <- new_block_output(list(tags = "a"), "tags_output")

  expect_identical(output_schema(out)$properties$tags$type, "array")
  expect_identical(
    as.character(encode_message(shape_output(out)$output)),
    "{\"tags\":[\"a\"]}"
  )

  expect_error(
    output_schema(new_block_output(list(), "x_output")),
    class = "output_schema_missing"
  )
})

test_that("a package reads a schema from a file that it ships", {

  file <- withr::local_tempfile(fileext = ".json")
  writeLines('{"type": "object", "properties": {"tags": {"type": "array"}}}',
             file)

  expect_identical(
    read_json_schema(file, package = NULL)$properties$tags$type,
    "array"
  )
  expect_identical(
    read_json_schema("outputs/table-output.schema.json", "blockr.core")$type,
    "object"
  )

  expect_error(read_json_schema("no-such-file.json", package = NULL),
               class = "json_schema_file_invalid")
  expect_error(read_json_schema("no-such-file.json", package = "blockr.core"),
               class = "json_schema_file_invalid",
               regexp = "in package blockr.core")
  expect_error(read_json_schema(1, package = NULL),
               class = "json_schema_file_invalid")
})

test_that("core gives each of its output classes its schema", {

  for (cls in c("table_output", "text_output", "plot_output")) {

    schema <- output_schema(new_block_output(list(), cls))

    expect_identical(schema$type, "object")
    expect_true("renderer" %in% names(schema$properties))
  }
})

test_that("an output family is a generic with a class of its own", {

  families <- list(
    table_output = table_output,
    text_output = text_output,
    plot_output = plot_output
  )

  for (cls in names(families)) {
    expect_true(is_output_family(families[[cls]]))
    expect_s3_class(
      families[[cls]],
      c(cls, "output_family", "function"),
      exact = TRUE
    )
  }

  # A family that is a generic dispatches on the result, as S3 registers the
  # methods of any generic
  tbl <- table_output(datasets::BOD, options = list(n_rows = 2L))

  expect_s3_class(tbl, "table_output")
  expect_s3_class(text_output(1:3), "text_output")

  words <- new_renderer("words_output", test_module("words.js"))

  words_output <- new_output_family(
    function(result, ...) {
      new_block_output(list(words = result), "words_output")
    },
    "words_output",
    renderer = words
  )

  expect_true(is_output_family(words_output))
  expect_s3_class(words_output("hi"), "words_output")
  expect_identical(attr(words_output, "renderer"), words)

  expect_false(is_output_family(function(result, ...) result))
  expect_false(is_output_family(structure(list(), class = "output_family")))

  check <- function(...) {
    expect_error(new_output_family(...), class = "output_family_invalid")
  }

  check("words_output", "words_output", renderer = words)
  check(identity, character(), renderer = words)
  check(identity, NA_character_, renderer = words)
  check(identity, "output_family", renderer = words)
  check(identity, "words_output")
  check(identity, "words_output", renderer = "words.js")
  check(identity, "words_output",
        renderer = new_renderer("rows", test_module("rows.js"),
                                option = "n_rows"))
  check(identity, "words_output", families = identity)
})

test_that("an output family holds the board options it reads", {

  expect_identical(family_option_ids(table_output), "n_rows")
  expect_identical(family_option_ids(plot_output), character())

  themed_output <- new_output_family(
    function(result, ...) new_text_output(format(result)),
    "themed_output",
    options = new_dark_mode_option,
    renderer = attr(text_output, "renderer")
  )

  expect_identical(family_option_ids(themed_output), "dark_mode")

  expect_error(
    new_output_family(identity, "themed_output", options = "dark_mode",
                      renderer = attr(text_output, "renderer")),
    class = "output_family_invalid"
  )
  expect_error(
    new_output_family(identity, "themed_output", options = identity,
                      renderer = attr(text_output, "renderer")),
    class = "output_family_invalid"
  )
})

test_that("an output family hands its results to the families it names", {

  coef_output <- new_output_family(
    function(result, ...) {
      table_output(as.data.frame(stats::coef(summary(result))), ...)
    },
    "coef_output",
    options = new_dark_mode_option,
    families = table_output
  )

  # It reads their board options beside its own, and holds them, so that
  # their packages load wherever it does
  expect_identical(family_option_ids(coef_output), c("dark_mode", "n_rows"))
  expect_identical(attr(coef_output, "families"), list(table_output))

  fit <- stats::lm(Sepal.Length ~ Sepal.Width, datasets::iris)
  out <- coef_output(fit, options = list(n_rows = 4L))

  expect_s3_class(out, "table_output")
  expect_identical(out$total, 2L)

  # A family that names another family twice, or one that names it too,
  # reads its board options once
  both_output <- new_output_family(
    identity,
    "both_output",
    families = list(coef_output, table_output, coef_output)
  )

  expect_identical(family_option_ids(both_output), c("dark_mode", "n_rows"))
  expect_named(reached_families(both_output),
               c("both_output", "coef_output", "table_output"))
})

test_that("an open family hands its results to every listed family", {

  any_output <- new_output_family(
    function(result, ...) {
      if (is.data.frame(result)) {
        return(table_output(result, ...))
      }
      text_output(result, ...)
    },
    "any_output",
    families = TRUE
  )

  expect_true(is_open_family(any_output))
  expect_false(is_open_family(table_output))
  expect_true(attr(any_output, "families"))

  expect_s3_class(any_output(datasets::BOD, options = list(n_rows = 2L)),
                  "table_output")
  expect_s3_class(any_output(month.name), "text_output")

  # It reaches the families that the available packages list, core's here,
  # and reads their board options
  expect_named(
    reached_families(any_output),
    c("any_output", "table_output", "text_output", "plot_output")
  )
  expect_identical(family_option_ids(any_output), "n_rows")

  # Or the families that the app read, with those that they name, each once
  themed_output <- new_output_family(
    function(result, ...) text_output(format(result), ...),
    "themed_output",
    options = new_dark_mode_option,
    families = text_output
  )

  listed <- list(themed_output = themed_output, table_output = table_output)

  expect_named(
    reached_families(any_output, listed),
    c("any_output", "themed_output", "table_output", "text_output")
  )
  expect_identical(
    family_option_ids(any_output, listed),
    c("dark_mode", "n_rows")
  )

  # A family that names an open family reaches the listed families too
  wrap_output <- new_output_family(
    function(result, ...) any_output(result, ...),
    "wrap_output",
    families = any_output
  )

  expect_named(
    reached_families(wrap_output, listed),
    c("wrap_output", "any_output", "themed_output", "table_output",
      "text_output")
  )

  # R reads the listings only where it reaches an open family
  local_mocked_bindings(app_families = function(...) stop("A listing read"))

  expect_identical(family_option_ids(table_output), "n_rows")
  expect_identical(family_option_ids(any_output, listed),
                   c("dark_mode", "n_rows"))
  expect_error(family_option_ids(any_output), "A listing read")

  # No other value makes a family open, and FALSE does not stand for no family
  for (families in list("all", FALSE, NA, c(TRUE, TRUE), list(TRUE))) {
    expect_error(
      new_output_family(identity, "any_output", families = families),
      class = "output_family_invalid"
    )
  }
})

test_that("an output family takes no other family's class", {

  # A family that took the class of another would draw its outputs, but read
  # none of the other's board options
  expect_error(
    new_output_family(identity, "table_output", families = text_output),
    "names in `families`",
    class = "output_family_invalid"
  )

  # A script that runs again defines its families anew
  words <- new_renderer("words_output", test_module("words.js"))

  words_output <- new_output_family(function(result, ...) result,
                                    "words_output", renderer = words)
  words_output <- new_output_family(function(result, ...) result,
                                    "words_output", renderer = words)

  expect_true(is_output_family(words_output))
})

test_that("R reads a family as its package defines it now", {

  # A block class of another package holds a copy of core's family, made when
  # that package was built, which may be older than core's
  stale <- structure(
    table_output,
    options = NULL,
    renderer = new_renderer("old", test_module("old.js"))
  )

  expect_identical(current_family(stale), table_output)
  expect_identical(family_option_ids(stale), "n_rows")
  expect_identical(
    family_renderers(stale)$table_output$id,
    "blockr.core-table_output"
  )

  # A family that its package no longer binds under its class reads what the
  # copy holds
  gone <- new_output_family(
    function(result, ...) result,
    "gone_output",
    renderer = new_renderer("gone", test_module("g.js"))
  )

  expect_identical(current_family(gone), gone)
})

test_that("an output is written by the schema of its most specific class", {

  local_output_class(
    "a_output",
    schema = list(type = "object", properties = list(x = list(type = "array")))
  )

  out <- new_block_output(list(x = 1L, y = 2L), c("c_output", "a_output"))

  expect_identical(as.character(encode_message(shape_output(out)$output)),
                   "{\"x\":[1],\"y\":2}")

  local_output_class(
    "c_output",
    schema = list(type = "object", properties = list(y = list(type = "array")))
  )

  expect_identical(as.character(encode_message(shape_output(out)$output)),
                   "{\"x\":1,\"y\":[2]}")

  expect_error(shape_output(new_block_output(list(), "d_output")),
               class = "output_schema_missing")
  expect_error(shape_output(list(x = 1)), class = "block_output_invalid")
})

test_that("an output's body travels beside its properties", {

  body <- new_reply_body(as.raw(1:3), "image/png")
  out <- new_plot_output(body, 640, 400)

  parts <- shape_output(out)

  expect_identical(parts$output, list(width = 640, height = 400))
  expect_identical(parts$body, body)

  expect_null(shape_output(new_text_output("Hi"))$body)
})

test_that("a subclass's requests come before those of its class", {

  local_output_class(
    "base_output",
    requests = list(
      size = function(result) length(result),
      label = function(result, prefix = "Base") paste(prefix, result[1L])
    )
  )

  local_output_class(
    "sub_output",
    requests = function(x, ...) {
      c(
        list(
          label = function(result, prefix = "Sub") paste(prefix, result[1L])
        ),
        NextMethod()
      )
    }
  )

  out <- new_block_output(list(), c("sub_output", "base_output"))

  expect_identical(run_output_class_request(out, "size", letters), 26L)
  expect_identical(run_output_class_request(out, "label", letters), "Sub a")
  expect_identical(
    run_output_class_request(
      new_block_output(list(), "base_output"), "label", letters
    ),
    "Base a"
  )
  expect_identical(
    run_output_class_request(out, "label", letters, args = list(prefix = "A")),
    "A a"
  )

  expect_error(run_output_class_request(out, "nope", letters),
               class = "output_class_request_unknown")
  expect_error(run_output_class_request(out, 1, letters),
               class = "output_class_request_unknown")
  expect_error(
    run_output_class_request(new_text_output("Hi"), "size", letters),
    class = "output_class_request_unknown"
  )
  expect_error(run_output_class_request("base_output", "label", letters),
               class = "block_output_invalid")

  local_output_class("bad_output", requests = list(1))

  expect_error(
    run_output_class_request(new_block_output(list(), "bad_output"), "x", 1),
    class = "output_class_requests_invalid"
  )
})

test_that("a class gives an output's requests by what the output holds", {

  local_output_class(
    "count_output",
    requests = function(x, ...) {
      if (is.null(x$total)) {
        list(count = function(result) length(result))
      } else {
        list(total = function() x$total)
      }
    }
  )

  unknown <- new_block_output(list(total = NULL), "count_output")
  known <- new_block_output(list(total = 3L), "count_output")

  expect_identical(run_output_class_request(unknown, "count", letters), 26L)
  expect_identical(run_output_class_request(known, "total", letters), 3L)

  expect_error(run_output_class_request(known, "count", letters),
               class = "output_class_request_unknown")
})

test_that("a request takes the result, board options, parameters and file", {

  local_output_class(
    "arg_output",
    requests = list(
      all = function(result, options, params, file, format = "csv") {
        list(
          result = result,
          options = options,
          params = params,
          file = if (missing(file)) "none" else file,
          format = format
        )
      },
      dots = function(...) list(...),
      some = function(params) params
    )
  )

  out <- new_block_output(list(), "arg_output")

  res <- run_output_class_request(
    out, "all", 1:3,
    args = list(format = "xlsx"),
    options = list(n_rows = 5L),
    params = list(page = 2L),
    file = list(path = "x.png")
  )

  expect_identical(
    res,
    list(
      result = 1:3,
      options = list(n_rows = 5L),
      params = list(page = 2L),
      file = list(path = "x.png"),
      format = "xlsx"
    )
  )

  expect_identical(run_output_class_request(out, "all", 1)$file,
                   "none")

  expect_identical(
    run_output_class_request(out, "dots", 1, args = list(a = 1L)),
    list(a = 1L)
  )

  expect_identical(
    run_output_class_request(out, "some", 1,
                             params = list(page = 1L)),
    list(page = 1L)
  )

  expect_error(
    run_output_class_request(out, "some", 1, args = list(x = 1)),
    class = "output_class_request_args_invalid"
  )
  expect_error(
    run_output_class_request(out, "dots", 1,
                             args = list(result = 1)),
    class = "output_class_request_args_invalid"
  )
  expect_error(
    run_output_class_request(out, "all", 1, args = list(1)),
    class = "output_class_request_args_invalid"
  )
})
