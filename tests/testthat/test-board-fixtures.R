test_that("every board fixture has its R values", {
  expect_setequal(names(board_fixture_values()), board_fixtures())
})

test_that("R writes the JSON board of every fixture as the fixture holds it", {

  local_fixture_package()

  values <- board_fixture_values()

  for (name in board_fixtures()) {

    fixture <- read_json_strictly(board_fixture(name))

    # Not expect_equal(), whose tolerance takes a number marked with `I()` for
    # the same number unmarked, which would let a scalar pass for an array.
    expect_identical(
      read_json_strictly(encode_message(blockr_ser(values[[name]]$board))),
      fixture[names(fixture) != "description"],
      label = paste("the JSON board R writes for", name),
      expected.label = "the fixture's"
    )
  }
})

test_that("serving a fixture registers its classes, and writing it does not", {

  local_fixture_package()

  board <- board_fixture_values()$board$board
  ids <- c("letters_block", "join_block")

  expect_false(any(ids %in% list_blocks()))

  blockr_ser(board)

  expect_false(any(ids %in% list_blocks()))

  # In the app's block registry, which the block registry of the process
  # leaves as it was
  registry <- served_registry(serve(board))

  expect_true(all(ids %in% ls(registry)))
  expect_false(any(ids %in% list_blocks()))
})
