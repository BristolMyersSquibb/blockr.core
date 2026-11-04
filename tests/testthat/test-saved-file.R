test_that("R reads the format of a saved file, keeping its text", {

  # R loads none of the packages that the file names, such as one that is not
  # installed, since loading them is the deployment's job.
  text <- paste0(
    "{\"format\": 1, \"packages\": {\"blockr.core\": \"0.2.0\", ",
    "\"blockr.absent\": \"0.1.0\"}, \"board\": {\"name\": \"Café\"}}\n"
  )

  res <- read_saved_file(local_saved_file(text))

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
    expect_s3_class(read_saved_file(local_saved_file(text)), "json")
  }
})

test_that("R refuses a saved file whose format the client would refuse", {

  expect_error(read_saved_file(1L), class = "saved_file_path_invalid")

  expect_error(
    read_saved_file(NA_character_),
    class = "saved_file_path_invalid"
  )

  expect_error(read_saved_file(tempfile()), class = "saved_file_missing")
  expect_error(read_saved_file(tempdir()), class = "saved_file_missing")

  refused <- list(
    saved_file_json_invalid = c("", "{", "{\"format\": 1,}"),
    saved_file_invalid = c("[1, 2]", "1", "\"board\"", "[{\"format\": 1}]"),
    saved_file_format_missing = "{\"packages\": {}, \"board\": {}}",
    saved_file_format_invalid = c(
      "{\"format\": 1.5}",
      "{\"format\": \"1\"}",
      "{\"format\": [1]}",
      "{\"format\": null}"
    )
  )

  for (class in names(refused)) {
    for (text in refused[[class]]) {
      expect_error(read_saved_file(local_saved_file(text)), class = class)
    }
  }
})

test_that("R refuses a saved file of a format above any it reads", {

  path <- local_saved_file("{\"format\": 2, \"packages\": {}, \"board\": {}}")

  expect_error(
    read_saved_file(path),
    "has format 2, while R reads format 1 at most",
    class = "saved_file_format_unknown"
  )
})

test_that("R reads the saved file that the fixture hello-file serves", {

  msg <- decode_message(protocol_fixture("hello-file"))

  expect_identical(msg$r[[1L]][[1L]]$file$format, saved_file_format())

  text <- encode_message(proto_served_file())

  expect_identical(read_saved_file(local_saved_file(text)), text)
})
