test_that("new_arg_spec and new_arg_specs constructors", {

  a <- new_arg_spec(description = "d", example = 1L, type = NULL)

  expect_s3_class(a, "arg_spec")
  expect_identical(a[["description"]], "d")
  expect_identical(a[["example"]], 1L)
  expect_null(a[["type"]])

  spec <- new_arg_specs(n = "just a description")

  expect_s3_class(spec, "arg_specs")
  expect_s3_class(spec[["n"]], "arg_spec")
  expect_identical(spec[["n"]][["description"]], "just a description")

  expect_length(new_arg_specs(), 0L)

  expect_error(new_arg_spec(description = 1L), class = "arg_spec_invalid")
  expect_error(new_arg_specs(new_arg_spec("x")), class = "arg_specs_unnamed")
  expect_error(new_arg_specs(n = 1L))
})

test_that("arg_spec field getters expose the spec without indexing", {

  a <- new_arg_spec(description = "rows", example = 10L, type = NULL)

  expect_identical(arg_spec_description(a), "rows")
  expect_identical(arg_spec_example(a), 10L)
  expect_null(arg_spec_type(a))

  # a bare description string resolves through as_arg_spec()
  expect_identical(arg_spec_description("just a description"),
                   "just a description")
  expect_null(arg_spec_example("just a description"))
  expect_error(arg_spec_description(1L))

  spec <- new_arg_specs(
    n = new_arg_spec("rows", example = 10L),
    direction = new_arg_spec("end", example = "tail")
  )

  expect_identical(
    vapply(spec, arg_spec_description, character(1)),
    c(n = "rows", direction = "end")
  )
})

test_that("as_arg_specs dispatches by input type", {

  spec <- new_arg_specs(n = new_arg_spec("rows"))

  expect_identical(as_arg_specs(spec), spec)
  expect_s3_class(as_arg_specs(list(n = new_arg_spec("rows"))), "arg_specs")
  expect_s3_class(as_arg_specs(c(n = "rows")), "arg_specs")

  expect_error(as_arg_specs(1L))
  expect_error(as_arg_spec(1L))
})

test_that("deprecated block_arg aliases warn once and forward", {

  withr::local_options(rlib_warning_verbosity = "verbose")

  expect_warning(
    a <- new_block_arg("rows", example = 10L),
    class = "deprecated_arg_spec"
  )
  expect_s3_class(a, "arg_spec")
  expect_identical(arg_spec_description(a), "rows")

  expect_warning(
    spec <- new_block_args(n = new_arg_spec("rows")),
    class = "deprecated_arg_spec"
  )
  expect_s3_class(spec, "arg_specs")

  expect_warning(
    desc <- block_arg_description(new_arg_spec("rows")),
    class = "deprecated_arg_spec"
  )
  expect_identical(desc, "rows")

  expect_warning(
    block_arg_example(new_arg_spec("x", example = 1L)),
    class = "deprecated_arg_spec"
  )
  expect_warning(
    block_arg_type(new_arg_spec("x", type = arg_integer())),
    class = "deprecated_arg_spec"
  )

  withr::defer(unregister_blocks("ut_dep_arg"))

  expect_warning(
    register_block(
      new_head_block,
      name = "t",
      description = "t",
      uid = "ut_dep_arg",
      arguments = new_block_args(direction = "head or tail")
    ),
    class = "deprecated_arg_spec"
  )

  expect_true("ut_dep_arg" %in% list_blocks())
})

test_that("bare and empty arguments normalize without warning", {

  expect_silent(
    bare <- normalize_arguments(c(n = "rows", direction = "end"), NULL)
  )

  expect_s3_class(bare[["arguments"]], "arg_specs")
  expect_identical(bare[["arguments"]][["n"]][["description"]], "rows")
  expect_null(bare[["guidance"]])

  empty <- normalize_arguments(character(), NULL)

  expect_length(empty[["arguments"]], 0L)
})

test_that("legacy examples/prompt attributes are absorbed with a deprecation", {

  withr::local_options(rlib_warning_verbosity = "verbose")

  legacy <- structure(
    c(direction = "head or tail"),
    examples = list(direction = "tail"),
    prompt = "Pick head for the first rows."
  )

  expect_warning(
    norm <- normalize_arguments(legacy, NULL),
    class = "deprecated_arg_attrs"
  )

  expect_identical(norm[["guidance"]], "Pick head for the first rows.")
  expect_identical(norm[["arguments"]][["direction"]][["example"]], "tail")

  expect_identical(
    block_examples_list(norm[["arguments"]], list()),
    list(list(direction = "tail"))
  )
})

test_that("per-argument examples assemble into one whole-block configuration", {

  spec <- new_arg_specs(
    n = new_arg_spec("count", example = 10L),
    direction = new_arg_spec("end", example = "tail")
  )

  expect_identical(
    block_examples_list(spec, list()),
    list(list(n = 10L, direction = "tail"))
  )
})

test_that("block-level examples supersede the per-argument assembly", {

  spec <- new_arg_specs(direction = new_arg_spec("end", example = "head"))

  authored <- list(
    list(direction = "head"),
    list(direction = "tail")
  )

  expect_identical(block_examples_list(spec, authored), authored)
})

test_that("registration hard-validates the structured spec form", {

  expect_error(
    register_block(
      new_head_block,
      name = "t",
      description = "t",
      uid = "ut_head_bad_arg",
      arguments = new_arg_specs(not_a_formal = new_arg_spec("x"))
    ),
    class = "block_arg_unknown"
  )

  expect_error(
    register_block(
      new_head_block,
      name = "t",
      description = "t",
      uid = "ut_head_bad_example",
      arguments = new_arg_specs(
        direction = new_arg_spec("end", example = "sideways")
      )
    ),
    class = "block_example_invalid"
  )

  expect_false("ut_head_bad_arg" %in% list_blocks())
  expect_false("ut_head_bad_example" %in% list_blocks())
})

test_that("registration validates worked examples against declared types", {

  new_stateful_block <- function(state = list(), ...) {
    new_transform_block(
      function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {
            list(expr = reactive(quote(identity(data))), state = list())
          }
        )
      },
      function(id) {
        tagList()
      },
      class = "stateful_block",
      ...
    )
  }

  state_type <- arg_object(
    conditions = arg_array(arg_object(column = arg_string())),
    operator   = arg_enum(c("&", "|"))
  )

  withr::defer(
    if ("stateful_block" %in% list_blocks()) unregister_blocks("stateful_block")
  )

  expect_no_error(
    register_block(
      new_stateful_block,
      name = "Stateful",
      description = "d",
      uid = "stateful_block",
      arguments = new_arg_specs(
        state = new_arg_spec(
          "the state",
          example = list(
            conditions = list(list(column = "Species")),
            operator = "&"
          ),
          type = state_type
        )
      )
    )
  )

  # malformed-but-constructible: conditions is a string, not an array
  expect_error(
    register_block(
      new_stateful_block,
      name = "Stateful",
      description = "d",
      uid = "stateful_block",
      overwrite = TRUE,
      arguments = new_arg_specs(
        state = new_arg_spec(
          "the state",
          example = list(conditions = "not-a-list", operator = "&"),
          type = state_type
        )
      )
    ),
    class = "block_example_nonconforming"
  )

  # structural: a malformed type descriptor is rejected
  expect_error(
    register_block(
      new_stateful_block,
      name = "Stateful",
      description = "d",
      uid = "stateful_block",
      overwrite = TRUE,
      arguments = new_arg_specs(
        state = new_arg_spec("the state", type = list(type = "frobnicate"))
      )
    ),
    class = "block_arg_type_invalid"
  )
})

test_that("block-level examples validate without a user-supplied spec", {

  withr::defer(unregister_blocks("ut_ex_only"))

  expect_no_error(
    register_block(
      new_dataset_block,
      name = "t",
      description = "t",
      uid = "ut_ex_only",
      examples = list(list(dataset = "iris"))
    )
  )

  expect_error(
    register_block(
      new_head_block,
      name = "t",
      description = "t",
      uid = "ut_ex_only_bad",
      examples = list(list(direction = "sideways"))
    ),
    class = "block_example_invalid"
  )
})

test_that("registry_metadata is deprecated but still projects legacy shape", {

  withr::local_options(rlib_warning_verbosity = "verbose")
  withr::defer(unregister_blocks("ut_head_legacy"))

  register_block(
    new_head_block,
    name = "Head",
    description = "First or last.",
    uid = "ut_head_legacy",
    arguments = new_arg_specs(
      direction = new_arg_spec("head or tail", example = "tail")
    ),
    guidance = "Pick head for the first rows."
  )

  expect_warning(
    args <- registry_metadata("ut_head_legacy", "arguments"),
    class = "deprecated_registry_metadata"
  )

  args <- args[[1L]]

  expect_type(args, "character")
  expect_identical(as.character(args), "head or tail")
  expect_identical(attr(args, "examples"), list(direction = "tail"))
  expect_identical(attr(args, "prompt"), "Pick head for the first rows.")
})

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
