test_that("arg type descriptor constructors build JSON-Schema nodes", {

  expect_identical(arg_string()[["type"]], "string")
  expect_identical(arg_number()[["type"]], "number")
  expect_identical(arg_integer()[["type"]], "integer")
  expect_identical(arg_boolean()[["type"]], "boolean")

  en <- arg_enum(c("a", "b"))
  expect_identical(en[["type"]], "string")
  expect_identical(as.character(en[["enum"]]), c("a", "b"))

  arr <- arg_array(arg_string())
  expect_identical(arr[["type"]], "array")
  expect_identical(arr[["items"]][["type"]], "string")

  obj <- arg_object(x = arg_string(), y = arg_integer(required = FALSE))
  expect_identical(obj[["type"]], "object")
  expect_identical(as.character(obj[["required"]]), "x")
  expect_false(obj[["additionalProperties"]])
  expect_named(obj[["properties"]], c("x", "y"))

  expect_error(arg_enum(character()), class = "arg_enum_invalid")
  expect_error(arg_object(arg_string()), class = "arg_object_unnamed")
})

test_that("valid_descriptor accepts the subset and rejects malformed types", {

  expect_true(valid_descriptor(arg_string()))
  expect_true(valid_descriptor(arg_object(x = arg_array(arg_enum(c("a"))))))

  expect_false(valid_descriptor("string"))
  expect_false(valid_descriptor(list(type = "frobnicate")))
  expect_false(valid_descriptor(list(type = "array")))
  expect_false(
    valid_descriptor(list(type = "object", properties = list(arg_string())))
  )
})

test_that("conforms_to checks R values against a descriptor", {

  expect_true(conforms_to("x", arg_string()))
  expect_false(conforms_to(5L, arg_string()))

  expect_true(conforms_to(5L, arg_integer()))
  expect_false(conforms_to(5.5, arg_integer()))

  expect_true(conforms_to(TRUE, arg_boolean()))
  expect_true(conforms_to("a", arg_enum(c("a", "b"))))
  expect_false(conforms_to("z", arg_enum(c("a", "b"))))

  arr <- arg_array(arg_string())
  expect_true(conforms_to(list("a", "b"), arr))
  expect_false(conforms_to("not-a-list", arr))
  expect_false(conforms_to(list("a", 5L), arr))

  obj <- arg_object(x = arg_string(), y = arg_integer(required = FALSE))
  expect_true(conforms_to(list(x = "a", y = 3L), obj))
  expect_true(conforms_to(list(x = "a"), obj))
  expect_false(conforms_to(list(y = 3L), obj))
  expect_false(conforms_to(list(x = "a", z = 1L), obj))
})

test_that("descriptors serialise to JSON Schema that ellmer reads back", {

  skip_if_not_installed("ellmer")

  d <- arg_object(
    conditions = arg_array(arg_object(column = arg_string())),
    operator   = arg_enum(c("&", "|"))
  )

  js <- as.character(jsonlite::toJSON(d, auto_unbox = TRUE))

  expect_match(js, '"required":["conditions","operator"]', fixed = TRUE)
  expect_match(js, '"enum":["&","|"]', fixed = TRUE)
  expect_no_error(ellmer::type_from_schema(text = js))
})
