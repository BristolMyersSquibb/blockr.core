test_that("an output is a list of fields of a class ending in block_output", {

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

test_that("outputs print their class, fields and body", {

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

test_that("an output class gives the coercion that makes its outputs", {

  coercions <- output_coercions()

  expect_identical(coercions$table_output, as_table_output)
  expect_identical(coercions$text_output, as_text_output)
  expect_identical(coercions$plot_output, as_plot_output)
  expect_null(output_coercion(new_block_output(list(), "words_output")))

  expect_identical(coercion_class(as_table_output), "table_output")
  expect_identical(coercion_class(as_plot_output), "plot_output")
  expect_null(coercion_class(function(result, ...) result))

  # A subclass that a method of its class's coercion makes gives no method of
  # its own, and S3 dispatch gives it its class's
  lazy <- new_block_output(list(), c("lazy_table_output", "table_output"))

  expect_identical(output_coercion(lazy), as_table_output)
  expect_identical(coercion_class(as_table_output), "table_output")

  as_words_output <- function(result, ...) {
    new_block_output(list(words = result), "words_output")
  }

  local_output_class("words_output", coercion = as_words_output)

  expect_identical(coercion_class(as_words_output), "words_output")

  # A coercion makes the outputs of one class
  local_output_class("pages_output", coercion = as_table_output)

  expect_error(
    coercion_class(as_table_output),
    class = "output_coercion_invalid"
  )
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

test_that("an output's body travels beside its fields", {

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
    run_output_class_request("base_output", "label", letters),
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
  expect_error(run_output_class_request("text_output", "size", letters),
               class = "output_class_request_unknown")

  local_output_class("bad_output", requests = list(1))

  expect_error(run_output_class_request("bad_output", "x", 1),
               class = "output_class_requests_invalid")
  expect_error(volatile(1), class = "output_class_requests_invalid")
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
      dots = volatile(function(...) list(...)),
      some = function(params) params
    )
  )

  res <- run_output_class_request(
    "arg_output", "all", 1:3,
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

  expect_identical(run_output_class_request("arg_output", "all", 1)$file,
                   "none")

  expect_identical(
    run_output_class_request("arg_output", "dots", 1, args = list(a = 1L)),
    list(a = 1L)
  )

  expect_identical(
    run_output_class_request("arg_output", "some", 1,
                             params = list(page = 1L)),
    list(page = 1L)
  )

  expect_true(output_class_request("arg_output", "dots")$volatile)
  expect_false(output_class_request("arg_output", "all")$volatile)

  expect_error(
    run_output_class_request("arg_output", "some", 1, args = list(x = 1)),
    class = "output_class_request_args_invalid"
  )
  expect_error(
    run_output_class_request("arg_output", "dots", 1,
                             args = list(result = 1)),
    class = "output_class_request_args_invalid"
  )
  expect_error(
    run_output_class_request("arg_output", "all", 1, args = list(1)),
    class = "output_class_request_args_invalid"
  )
})
