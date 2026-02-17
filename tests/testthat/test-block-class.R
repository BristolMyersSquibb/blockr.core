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

  expect_error(
    validate_block_server(function(id, `1`) {}),
    class = "block_server_args_invalid"
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
        name = "1",
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

test_that("block metadata", {

  meta1 <- block_metadata(new_dataset_block())

  expect_s3_class(meta1, "data.frame")
  expect_identical(nrow(meta1), 1L)
  expect_identical(ncol(meta1), 7L)
  expect_named(
    meta1,
    c("id", "name", "description", "category", "icon", "arguments", "package")
  )

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
      block_metadata = list(),
      class = "identity_block"
    )
  }

  meta2 <- block_metadata(new_identity_block())

  expect_s3_class(meta2, "data.frame")
  expect_identical(nrow(meta2), 1L)
  expect_identical(ncol(meta2), 7L)
  expect_named(
    meta2,
    c("id", "name", "description", "category", "icon", "arguments", "package")
  )

  meta3 <- block_metadata(
    blocks(a = new_dataset_block(), b = new_identity_block())
  )

  expect_s3_class(meta3, "data.frame")
  expect_identical(nrow(meta3), 2L)
  expect_identical(ncol(meta3), 7L)
  expect_named(
    meta3,
    c("id", "name", "description", "category", "icon", "arguments", "package")
  )
  expect_identical(rownames(meta3), c("a", "b"))
})
