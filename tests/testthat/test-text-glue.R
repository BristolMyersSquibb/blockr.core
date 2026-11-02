test_that("glue block constructor", {

  blk <- new_glue_block()

  expect_s3_class(blk, "glue_block")
  expect_identical(block_state(blk), list(text = ""))
  expect_true(is.na(block_arity(blk)))

  expect_identical(
    block_state(new_glue_block("{nrow(data)} rows")),
    list(text = "{nrow(data)} rows")
  )

  expect_identical(registry_id_from_block(blk), "blockr.core-glue_block")
})

test_that("a glue block refers to its inputs by link or by block", {

  res <- evaluate_block(
    glue_block,
    list(text = "{nrow(data)} rows, {nrow(a)} in a"),
    list(... = list("a", data = "b")),
    list(a = datasets::BOD, b = datasets::iris)
  )

  expect_identical(res[["status"]], "ok")
  expect_identical(
    res[["expr"]],
    quote(
      glue::glue(
        "{nrow(data)} rows, {nrow(a)} in a",
        .envir = list2env(
          rlang::dots_list(a, data = b, .named = TRUE),
          parent = baseenv()
        )
      )
    )
  )
  expect_identical(as.character(res[["result"]]), "150 rows, 6 in a")
  expect_identical(as_text_output(res[["result"]])[["text"]],
                   "150 rows, 6 in a")
})

test_that("the glue block's fixture", {
  expect_block_fixture(
    glue_block,
    states = list(list(text = ""), list(text = "{nrow(data)} rows")),
    inputs = list(... = list(data = datasets::iris))
  )
})
