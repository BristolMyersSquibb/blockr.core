test_that("every sample output fixture has its R values", {
  expect_setequal(names(output_fixture_values()), output_fixtures())
})

test_that("every sample output is written as R writes it", {

  values <- output_fixture_values()

  for (name in output_fixtures()) {

    # Not expect_equal(), whose tolerance takes a number marked with `I()` for
    # the same number unmarked, which would let a scalar pass for an array.
    expect_identical(
      read_json_strictly(output_fixture_json(values[[name]])),
      read_json_strictly(output_fixture(name)),
      label = paste("the sample outputs R writes for", name),
      expected.label = "the fixture's"
    )
  }
})

test_that("every sample body is a file beside the fixtures", {

  for (name in output_fixtures()) {

    for (sample in output_fixture_values()[[name]]$outputs) {

      if (is.null(sample$body_file)) {
        next
      }

      file <- file.path(output_fixture_dir(), sample$body_file)

      expect_true(file.exists(file), label = sample$body_file)
      expect_identical(
        readBin(file, "raw", 8L),
        as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)),
        label = paste("the start of", sample$body_file)
      )
    }
  }
})
