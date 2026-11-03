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

test_that("a glue block refers to its inputs by link name or position", {

  res <- evaluate_block(
    glue_block,
    list(text = "{nrow(...1)} and {nrow(data)} rows, {nrow(...3)} more"),
    list(... = list("a", data = "b", "c")),
    list(a = datasets::BOD, b = datasets::iris, c = datasets::mtcars)
  )

  expect_identical(res[["status"]], "ok")
  expect_identical(
    as.list(res[["expr"]])[-1L],
    list(quote(a), data = quote(b), quote(c))
  )
  expect_identical(as.character(res[["result"]]), "6 and 150 rows, 32 more")
  expect_identical(
    as_text_output(res[["result"]])[["text"]],
    "6 and 150 rows, 32 more"
  )

  res <- evaluate_block(glue_block, list(text = "{nrow(...1)} rows"),
                        list(... = list("a")), list(a = datasets::BOD))

  expect_identical(as.character(res[["result"]]), "6 rows")
})

test_that("the glue block's fixture", {
  expect_block_fixture(
    glue_block,
    states = list(list(text = ""), list(text = "{nrow(data)} rows")),
    inputs = list(... = list(data = datasets::iris))
  )
})
