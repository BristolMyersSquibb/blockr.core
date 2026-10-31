test_that("a fixture lists states and records request answers", {

  expect_block_fixture(
    head_class,
    states = list(list(n = 6L, direction = "head"), list(n = 2L)),
    inputs = list(data = datasets::iris),
    requests = list(list(name = "n_max"))
  )

  expect_block_fixture(
    merge_class,
    states = list(list(by = "Time"), list(by = "Time", all_x = TRUE)),
    inputs = list(x = datasets::BOD, y = datasets::BOD),
    requests = list(list(name = "shared"))
  )

  expect_block_fixture(
    rbind_class,
    states = list(list()),
    inputs = list(... = list(datasets::BOD, b2 = datasets::BOD))
  )
})

test_that("a fixture fails on a state that does not run", {

  expect_failure(
    expect_block_fixture(
      merge_class,
      states = list(list(all_x = TRUE)),
      inputs = list(x = datasets::BOD, y = datasets::BOD)
    ),
    "does not run \\(invalid\\)"
  )
})

test_that("sample inputs get ids", {

  expect_identical(
    sample_input_ids(list(data = 1)),
    list(inputs = list(data = "data"), results = list(data = 1))
  )

  expect_identical(
    sample_input_ids(list(... = list(1, b2 = 2))),
    list(
      inputs = list(... = list("input_1", b2 = "input_2")),
      results = list(input_1 = 1, input_2 = 2)
    )
  )

  expect_identical(
    sample_input_ids(list()),
    list(inputs = list(), results = list())
  )
})

test_that("request answers are written with every vector as an array", {

  boxed <- box_vectors(list(a = 1L, b = list(c = "x", d = NULL)))

  expect_s3_class(boxed$a, "AsIs")
  expect_s3_class(boxed$b$c, "AsIs")
  expect_null(boxed$b$d)

  json <- jsonlite::toJSON(boxed, auto_unbox = TRUE, null = "null")

  expect_identical(as.character(json), '{"a":[1],"b":{"c":["x"],"d":null}}')

  expect_identical(
    as_json_object(list()),
    set_names(list(), character())
  )
})
