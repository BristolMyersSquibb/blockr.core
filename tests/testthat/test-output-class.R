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

test_that("an output class is registered with the schema of its outputs", {

  file <- withr::local_tempfile(fileext = ".json")
  writeLines('{"type": "object", "properties": {"tags": {"type": "array"}}}',
             file)

  withr::defer(unregister_output_classes("tags_output"))

  entry <- register_output_class(
    "tags_output",
    schema = file,
    requests = list(n = function(result) length(result)),
    volatile = "n",
    package = NULL
  )

  expect_s3_class(entry, "output_class_entry")
  expect_identical(output_class_entry("tags_output"), entry)
  expect_identical(entry$schema$properties$tags$type, "array")

  expect_snapshot(print(entry))

  out <- new_block_output(list(tags = "a"), "tags_output")

  expect_identical(
    as.character(encode_message(shape_output(out)$output)),
    "{\"tags\":[\"a\"]}"
  )

  # The package that registered a class registers it again, as its .onLoad()
  # does when the package loads again, but no other one.
  expect_no_error(
    register_output_class("tags_output", schema = list(), package = NULL)
  )

  expect_error(
    register_output_class("tags_output", schema = list(), package = "other"),
    class = "output_class_in_registry"
  )
})

test_that("registration checks the class, its schema and its requests", {

  expect_error(register_output_class(1, list()), class = "output_class_invalid")
  expect_error(register_output_class("block_output", list()),
               class = "output_class_invalid")

  expect_error(
    register_output_class("x_output", "no-such-file.json", package = NULL),
    class = "output_class_schema_invalid"
  )
  expect_error(
    register_output_class("x_output", "no-such-file.json",
                          package = "blockr.core"),
    class = "output_class_schema_invalid"
  )
  expect_error(
    register_output_class("x_output", 1, package = NULL),
    class = "output_class_schema_invalid"
  )

  expect_error(
    register_output_class("x_output", list(), requests = list(function() 1)),
    class = "output_class_requests_invalid"
  )
  expect_error(
    register_output_class("x_output", list(), requests = list(a = 1)),
    class = "output_class_requests_invalid"
  )
  expect_error(
    register_output_class("x_output", list(),
                          requests = list(a = function() 1), volatile = "b"),
    class = "output_class_requests_invalid"
  )

  expect_null(output_class_entry("x_output"))
})

test_that("core registers its output classes as it loads", {

  for (cls in c("table_output", "text_output", "plot_output")) {

    entry <- output_class_entry(cls)

    expect_identical(entry$package, "blockr.core")
    expect_identical(entry$schema$type, "object")
    expect_true("renderer" %in% names(entry$schema$properties))
  }
})

test_that("an output is written by the schema of its most specific class", {

  withr::defer(unregister_output_classes("a_output"))

  register_output_class(
    "a_output",
    list(type = "object", properties = list(x = list(type = "array"))),
    package = NULL
  )

  out <- new_block_output(list(x = 1L, y = 2L), c("c_output", "a_output"))

  expect_identical(as.character(encode_message(shape_output(out)$output)),
                   "{\"x\":[1],\"y\":2}")

  register_output_class(
    "c_output",
    list(type = "object", properties = list(y = list(type = "array"))),
    package = NULL
  )
  withr::defer(unregister_output_classes("c_output"))

  expect_identical(as.character(encode_message(shape_output(out)$output)),
                   "{\"x\":1,\"y\":[2]}")

  expect_error(shape_output(new_block_output(list(), "d_output")),
               class = "output_class_unknown")
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

test_that("a request is resolved by the most specific class that has it", {

  withr::defer(unregister_output_classes(c("base_output", "sub_output")))

  register_output_class(
    "base_output",
    list(),
    requests = list(
      size = function(result) length(result),
      label = function(result, prefix = "Base") paste(prefix, result[1L])
    ),
    package = NULL
  )

  register_output_class(
    "sub_output",
    list(),
    requests = list(
      label = function(result, prefix = "Sub") paste(prefix, result[1L])
    ),
    package = NULL
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

  expect_identical(output_class_request(class(out), "label")$class,
                   "sub_output")

  expect_error(run_output_class_request(out, "nope", letters),
               class = "output_class_request_unknown")
  expect_error(run_output_class_request(out, 1, letters),
               class = "output_class_request_unknown")
  expect_error(run_output_class_request("text_output", "size", letters),
               class = "output_class_request_unknown")
})

test_that("a request takes the result, board options, parameters and file", {

  withr::defer(unregister_output_classes("arg_output"))

  register_output_class(
    "arg_output",
    list(),
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
    ),
    volatile = "dots",
    package = NULL
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
