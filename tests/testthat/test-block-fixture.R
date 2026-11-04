test_that("a fixture lists states and records request answers", {

  expect_block_fixture(
    head_class,
    states = list(list(n = 6L, direction = "head"), list(n = 2L)),
    inputs = list(data = datasets::iris),
    requests = list(list(name = "n_max"))
  )

  expect_block_fixture(
    merge_class,
    states = list(list(by = I("Time")), list(by = "Time", all_x = TRUE)),
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

test_that("a fixture writes each state as a board's JSON holds it", {

  labelled <- new_block_class(
    "labelled_block",
    inputs = "data",
    expr = function(label = "a", volumes = c(home = "~")) bbquote(.(data)),
    ui = js_module("labelled-block.js", package = "blockr.core")
  )

  # An arg left out takes its default, and a named vector is written as an
  # object, as blockr_ser() writes the state of a block.
  expect_identical(
    fixture_state(1L, list(list(label = "b")), labelled),
    list(label = "b", volumes = list(home = "~"))
  )

  expect_error(
    expect_block_fixture(
      labelled,
      states = list(list(label = factor("a"))),
      inputs = list(data = datasets::BOD)
    ),
    "State 1 of block class labelled_block holds a value of class factor",
    class = "board_value_invalid"
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

test_that("empty args and options are written as objects", {

  expect_identical(
    as_json_object(list()),
    set_names(list(), character())
  )
})
