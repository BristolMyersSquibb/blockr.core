test_that("R reads the format of a JSON board, keeping its text", {

  # R loads none of the packages that the file names, such as one that is not
  # installed, since loading them is the deployment's job.
  text <- paste0(
    "{\"format\": 1, \"packages\": {\"blockr.core\": \"0.2.0\", ",
    "\"blockr.absent\": \"0.1.0\"}, \"board\": {\"name\": \"Café\"}}\n"
  )

  res <- read_board_json(local_board_json(text))

  expect_s3_class(res, "json")
  expect_identical(unclass(res), text)
  expect_false(isNamespaceLoaded("blockr.absent"))

  # So that the reply to a hello holds the file as it is.
  expect_identical(
    unclass(encode_message(list(list(id = "1", file = res)))),
    paste0("[{\"id\":\"1\",\"file\":", text, "}]")
  )

  # The client takes any whole number up to its own format.
  for (version in c("1.0", "0")) {
    text <- paste0("{\"format\": ", version, "}")
    expect_s3_class(read_board_json(local_board_json(text)), "json")
  }
})

test_that("R refuses a JSON board whose format the client would refuse", {

  expect_error(read_board_json(1L), class = "board_json_path_invalid")

  expect_error(
    read_board_json(NA_character_),
    class = "board_json_path_invalid"
  )

  expect_error(read_board_json(tempfile()), class = "board_json_missing")
  expect_error(read_board_json(tempdir()), class = "board_json_missing")

  refused <- list(
    board_json_json_invalid = c("", "{", "{\"format\": 1,}"),
    board_json_invalid = c("[1, 2]", "1", "\"board\"", "[{\"format\": 1}]"),
    board_json_format_missing = "{\"packages\": {}, \"board\": {}}",
    board_json_format_invalid = c(
      "{\"format\": 1.5}",
      "{\"format\": \"1\"}",
      "{\"format\": [1]}",
      "{\"format\": null}"
    )
  )

  for (class in names(refused)) {
    for (text in refused[[class]]) {
      expect_error(read_board_json(local_board_json(text)), class = class)
    }
  }
})

test_that("R refuses a JSON board of a format above any it reads", {

  path <- local_board_json("{\"format\": 2, \"packages\": {}, \"board\": {}}")

  expect_error(
    read_board_json(path),
    "has format 2, while R reads format 1 at most",
    class = "board_json_format_unknown"
  )
})

test_that("R reads the JSON board that the fixture hello-file serves", {

  msg <- decode_message(protocol_fixture("hello-file"))

  expect_identical(msg$r[[1L]][[1L]]$file$format, board_json_format())

  text <- encode_message(proto_served_file())

  expect_identical(read_board_json(local_board_json(text)), text)
})
