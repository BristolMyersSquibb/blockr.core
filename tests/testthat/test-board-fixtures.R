test_that("every board fixture has its R values", {
  expect_setequal(names(board_fixture_values()), board_fixtures())
})

test_that("R writes the board of every fixture as the fixture holds it", {

  local_fixture_package()

  values <- board_fixture_values()

  for (name in board_fixtures()) {

    # Not expect_equal(), whose tolerance takes a number marked with `I()` for
    # the same number unmarked, which would let a scalar pass for an array.
    expect_identical(
      read_json_strictly(encode_message(blockr_ser(values[[name]]$board))),
      read_json_strictly(board_fixture(name))$board,
      label = paste("the board R writes for", name),
      expected.label = "the fixture's"
    )
  }
})

test_that("R registers the classes of a fixture from its blocks", {

  local_fixture_package()

  board <- board_fixture_values()$board$board

  expect_false(any(c("letters_block", "join_block") %in% list_blocks()))

  blockr_ser(board)

  expect_true(all(c("letters_block", "join_block") %in% list_blocks()))
})
