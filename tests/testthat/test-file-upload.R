upload_file <- function(name = "sales.csv") {

  writeBin(charToRaw("Time,demand\n1,8.3\n"), "upload.tmp")

  list(datapath = "upload.tmp", name = name, type = "text/csv", size = 18L)
}

upload <- function(file) {
  run_block_request(upload_block, "upload", args = list(file = file))
}

test_that("upload block constructor", {

  blk <- new_upload_block()

  expect_s3_class(blk, "upload_block")
  expect_identical(block_state(blk), set_names(list(), character()))
  expect_identical(registry_id_from_block(blk), "blockr.core-upload_block")
})

test_that("an upload keeps the file and answers with its path", {

  withr::local_dir(withr::local_tempdir())
  withr::local_options(blockr.upload_dir = "uploads")

  file <- upload_file()
  path <- upload(file)

  expect_match(path, "^uploads/[^/]+/sales\\.csv$")
  expect_identical(readLines(path), c("Time,demand", "1,8.3"))

  again <- upload(file)

  expect_match(again, "^uploads/[^/]+/sales\\.csv$")
  expect_false(identical(again, path))

  expect_identical(
    basename(upload(upload_file(file.path("..", "notes.csv")))),
    "notes.csv"
  )

  expect_identical(basename(upload(upload_file(NULL))), "upload.tmp")

  expect_error(
    upload(list(name = "sales.csv")),
    class = "upload_file_invalid"
  )

  expect_true(upload_block[["requests"]][["upload"]][["action"]])

  res <- evaluate_block(upload_block, list(file_path = path))

  expect_identical(res[["status"]], "ok")
  expect_identical(res[["expr"]], bquote(file.path(.(path))))
  expect_identical(res[["result"]], path)

  res <- evaluate_block(upload_block, list())

  expect_identical(res[["status"]], "invalid")
  expect_identical(
    res[["reason"]],
    list(message = "Upload a file.", arg = "file_path")
  )

  res <- evaluate_block(upload_block, list(file_path = ""))

  expect_identical(res[["status"]], "invalid")
  expect_identical(
    res[["reason"]],
    list(message = "Upload a file.", arg = "file_path")
  )
})

test_that("uploads go to the session's temporary directory by default", {

  withr::local_dir(withr::local_tempdir())
  withr::local_options(blockr.upload_dir = NULL)
  withr::local_envvar(BLOCKR_UPLOAD_DIR = NA)

  path <- upload(upload_file())

  expect_identical(
    normalizePath(dirname(dirname(path))),
    normalizePath(file.path(tempdir(), "uploads"))
  )
})

test_that("the upload block's fixture", {

  withr::local_dir(withr::local_tempdir())
  local_mocked_bindings(
    upload_folder = function() file.path("uploads", "3f9a2c")
  )

  file <- upload_file()

  expect_block_fixture(
    upload_block,
    states = list(list(file_path = "uploads/3f9a2c/sales.csv")),
    requests = list(list(name = "upload", args = list(file = file)))
  )
})
