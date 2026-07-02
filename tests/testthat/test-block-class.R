test_that("block constructor", {

  new_identity_block <- function() {
    new_transform_block(
      function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {
            list(
              expr = reactive(quote(identity(data))),
              state = list()
            )
          }
        )
      },
      function(id) {
        tagList()
      },
      class = "identity_block",
      block_metadata = list()
    )
  }

  expect_s3_class(
    new_identity_block(),
    c("identity_block", "transform_block", "block")
  )

  expect_error(
    validate_block_server("abc"),
    class = "block_server_function_invalid"
  )

  expect_error(
    validate_block_server(function() {}),
    class = "block_server_arg_id_invalid"
  )

  expect_error(
    validate_block_server(function(id, ...) {}),
    class = "block_server_dots_invalid"
  )

  expect_identical(
    validate_block_server(function(id) {}),
    function(id) {}
  )

  expect_error(
    validate_block_ui("abc"),
    class = "block_ui_function_invalid"
  )

  expect_error(
    validate_block_ui(function() {}),
    class = "block_ui_arg_id_invalid"
  )

  expect_error(
    validate_block_ui(function(abc) {}),
    class = "block_ui_arg_id_invalid"
  )

  expect_error(
    validate_data_validator(function(x) {}, function() {}),
    class = "block_validator_nullary_invalid"
  )

  expect_error(
    validate_data_validator("abc", function(id, x) {}),
    class = "block_validator_function_invalid"
  )

  expect_silent(
    validate_data_validator(NULL, function(id, x) {})
  )

  expect_error(
    validate_data_validator(function(y) {}, function(id, x) {}),
    class = "block_validator_args_invalid"
  )

  expect_error(
    validate_block("abc"),
    class = "block_class_invalid"
  )

  expect_error(
    validate_block(structure("abc", class = "block")),
    class = "block_class_invalid"
  )

  expect_error(
    validate_block(structure("abc", class = c("some_block", "block"))),
    class = "block_list_like_invalid"
  )

  expect_error(
    validate_block(
      structure(list(), ctor = "abc", class = c("some_block", "block"))
    ),
    class = "block_ctor_invalid"
  )

  expect_error(
    validate_block(
      structure(list(), ctor = new_blockr_ctor(function() {}), name = 1,
                class = c("some_block", "block"))
    ),
    class = "block_name_invalid"
  )

  expect_error(
    validate_block(
      structure(
        list(
          expr_server = function(id) {},
          expr_ui = function(id) {},
          dat_valid = NULL
        ),
        ctor = new_blockr_ctor(function() {}),
        block_name = "1",
        class = c("some_block", "block")
      ),
      ui_eval = TRUE
    ),
    class = "block_ui_eval_invalid"
  )
})

test_that("block class", {

  x <- new_dataset_block()

  expect_s3_class(x, "block")
  expect_true(is_block(x))
  expect_false(is_block("x"))

  expect_identical(x, as_block(x))

  expect_snapshot(print(x))
})

test_that("blocks report controllability in str_value()", {

  expect_identical(
    str_value(new_scatter_block(x = "wt", y = "mpg")),
    "<scatter_block> x, y"
  )
  expect_identical(
    str_value(new_subset_block()),
    "<subset_block> subset*, select*"
  )
  expect_identical(
    str_value(new_dataset_block()),
    "<dataset_block> dataset*, package"
  )
  expect_identical(str_value(new_rbind_block()), "<rbind_block>")
  expect_identical(str_value(structure(list(), class = "made_up")), "<made_up>")

  blk <- new_dataset_block()
  out <- capture.output(res <- withVisible(str(blk)))

  expect_identical(out, " <dataset_block> dataset*, package")
  expect_false(res$visible)
  expect_identical(res$value, blk)
})

test_that("block utils", {

  blk <- new_dataset_block()
  lst <- list(blk)

  expect_s3_class(c(blk, blk), "blocks")
  expect_s3_class(c(blk, lst), "blocks")
})

test_that("Without package blocks can print", {
  blk <- new_dummy_block <- function(text = "Hello World", ...) {
    new_data_block(
      function(id) {
        moduleServer(id, function(input, output, session) {
          list(
            expr = reactive(quote(text)),
            state = list(text = text)
          )
        })
      },
      function(id) {
        tagList()
      },
      class = "dummy_block",
      block_metadata = list(),
      ...
    )
  }
  expect_snapshot(blk())
})

test_that("external_ctrl_vars resolves the external_ctrl declaration", {

  expect_identical(
    external_ctrl_vars(new_static_block(mtcars)),
    "block_name"
  )

  expect_setequal(
    external_ctrl_vars(new_subset_block()),
    c("subset", "select", "block_name")
  )

  expect_setequal(
    external_ctrl_vars(new_dataset_block("mtcars")),
    c("dataset", "block_name")
  )

  blk <- new_dataset_block("mtcars")
  attr(blk, "external_ctrl") <- "not_an_input"

  expect_error(external_ctrl_vars(blk))
  expect_error(external_ctrl_vars("not a block"))
})

test_that("has_external_ctrl is TRUE whenever any var is controllable", {

  expect_true(has_external_ctrl(new_static_block(mtcars)))
  expect_true(has_external_ctrl(new_subset_block()))
  expect_true(has_external_ctrl(new_dataset_block("mtcars")))
})

test_that("allow_empty_state parses user- and data-input specs", {

  expect_identical(
    parse_allow_empty_state(FALSE),
    list(empty_state = FALSE, optional_inputs = character(), min_args = 1L)
  )

  expect_identical(
    parse_allow_empty_state(c("a", "b")),
    list(
      empty_state = c("a", "b"),
      optional_inputs = character(),
      min_args = 1L
    )
  )

  expect_identical(
    parse_allow_empty_state(list(input = "n", data = list("y", ...args = 2))),
    list(empty_state = "n", optional_inputs = "y", min_args = 2L)
  )

  expect_identical(
    parse_allow_empty_state(list(data = list(...args = 1))),
    list(empty_state = FALSE, optional_inputs = character(), min_args = 1L)
  )

  expect_identical(
    parse_allow_empty_state(list(input = TRUE)),
    list(empty_state = TRUE, optional_inputs = character(), min_args = 1L)
  )
})

test_that("block accessors expose user- and data-input specs", {

  # rbind declares nothing: variadic minimum defaults to 1
  rb <- new_rbind_block()
  expect_identical(block_min_args(rb), 1L)
  expect_identical(block_allow_empty_state(rb), FALSE)
  expect_identical(block_optional_inputs(rb), character())

  # glue opts out of the default, working with zero data inputs
  expect_identical(block_min_args(new_glue_block()), 0L)
})

test_that("allow_empty_state validates its structured form", {

  expect_error(
    new_head_block(allow_empty_state = list("data")),
    class = "allow_empty_state_invalid"
  )

  expect_error(
    new_head_block(allow_empty_state = list(data = list(...args = 2))),
    class = "allow_empty_state_invalid"
  )

  expect_error(
    new_head_block(allow_empty_state = list(data = list("nope"))),
    class = "allow_empty_state_invalid"
  )

  var_block <- function(aes) {
    new_transform_block(
      function(id, ...args) {
        moduleServer(
          id,
          function(input, output, session) {
            list(expr = reactive(quote(rbind(...args))), state = list())
          }
        )
      },
      function(id) tagList(),
      class = "test_variadic_block",
      allow_empty_state = aes,
      block_metadata = list()
    )
  }

  expect_error(
    var_block(list(data = list(...args = -1))),
    class = "allow_empty_state_invalid"
  )

  expect_s3_class(
    var_block(list(data = list(...args = 2))),
    "test_variadic_block"
  )
})
