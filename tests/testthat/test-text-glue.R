test_that("glue block constructor", {

  blk <- new_glue_block()

  expect_s3_class(blk, "glue_block")
  expect_identical(block_state(blk), list(text = ""))
  expect_true(is.na(block_arity(blk)))

  expect_identical(
    block_state(new_glue_block("{nrow(data)} rows")),
    list(text = "{nrow(data)} rows")
  )

  expect_identical(
    registry_id_from_block(blk),
    list(package = "blockr.core", id = "glue_block")
  )
})

test_that("a glue block refers to its inputs by their links' names", {

  res <- evaluate_block(
    glue_block,
    list(text = "{nrow(data)} and {nrow(more)} rows"),
    list(... = list(data = "a", more = "b")),
    list(a = datasets::BOD, b = datasets::iris)
  )

  expect_identical(res[["status"]], "ok")
  expect_identical(
    res[["expr"]],
    quote(
      glue::glue(
        "{nrow(data)} and {nrow(more)} rows",
        .envir = list2env(list(data = a, more = b), parent = environment())
      )
    )
  )
  expect_identical(as.character(res[["result"]]), "6 and 150 rows")
  expect_identical(text_output(res[["result"]])[["text"]], "6 and 150 rows")

  res <- evaluate_block(glue_block, list(text = "{nrow(data)} rows"),
                        list(... = list("a", data = "b")),
                        list(a = datasets::BOD, b = datasets::iris))

  expect_identical(res[["status"]], "error")
  expect_identical(res[["conditions"]][[1L]][["phase"]], "block_eval")
})

test_that("glue text uses the environment of the block expression", {

  withr::local_envvar(BLOCKR_ATTACH_DEFAULT_PACKAGES = NA)
  withr::local_options(blockr.attach_default_packages = FALSE)

  res <- evaluate_block(
    glue_block,
    list(text = "{median(data$Time)}"),
    list(... = list(data = "a")),
    list(a = datasets::BOD)
  )

  expect_identical(res[["status"]], "error")

  withr::local_options(blockr.attach_default_packages = TRUE)

  res <- evaluate_block(
    glue_block,
    list(text = "{median(data$Time)}"),
    list(... = list(data = "a")),
    list(a = datasets::BOD)
  )

  expect_identical(res[["status"]], "ok")
  expect_identical(as.character(res[["result"]]), "3.5")
})

test_that("the glue block's fixture", {
  expect_block_fixture(
    glue_block,
    states = list(list(text = ""), list(text = "{nrow(data)} rows")),
    inputs = list(... = list(data = datasets::iris))
  )
})
