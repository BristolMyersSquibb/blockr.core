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

test_that("writing a fixture registers no class, and serving it needs them", {

  local_fixture_package()

  board <- board_fixture_values()$board$board
  ids <- c("letters_block", "join_block")

  expect_false(any(lgl_ply(ids, lists_block)))

  blockr_ser(board)

  expect_false(any(lgl_ply(ids, lists_block)))

  # The fixture's classes are defined outside a package, so the script
  # registers them before it serves the board
  expect_error(serve(board), class = "board_classes_unregistered")

  for (cls in unique(lapply(as.list(board_blocks(board)), block_class))) {
    local_block_class(cls)
  }

  held <- ref_labels(registry_entries(served_registry(serve(board))))

  expect_true(all(ids %in% held))
})
