skip_on_os("windows")

test_that("filebrowser block constructor", {

  file <- normalizePath(withr::local_tempfile(lines = "abc"))

  blk <- new_filebrowser_block(volumes = c(tmp = dirname(file)))

  expect_s3_class(blk, "filebrowser_block")
  expect_identical(block_state(blk), list(volumes = c(tmp = dirname(file))))
  expect_identical(
    registry_id_from_block(blk),
    "blockr.core-filebrowser_block"
  )

  vols <- filebrowser_volumes()

  expect_type(vols, "character")
  expect_named(vols, "home")

  vols <- filebrowser_volumes(file.path("foo", "bar"))

  expect_type(vols, "character")
  expect_named(vols, "volume")

  vols <- filebrowser_volumes("foo/bar:foo/baz")

  expect_type(vols, "character")
  expect_named(vols, c("volume1", "volume2"))

  expect_error(
    filebrowser_volumes(c(a = "foo", a = "bar")),
    class = "filebrowser_volumes_invalid"
  )
  expect_error(filebrowser_volumes(1), class = "filebrowser_volumes_invalid")
})

test_that("a file browser block gives the path of its file", {

  vols <- list(data = "/data")

  res <- evaluate_block(filebrowser_block, list(volumes = vols))

  expect_identical(res[["status"]], "invalid")

  res <- evaluate_block(
    filebrowser_block,
    list(file_path = "/data/sales.csv", volumes = vols)
  )

  expect_identical(res[["status"]], "ok")
  expect_identical(res[["expr"]], quote(file.path("/data/sales.csv")))
  expect_identical(res[["result"]], "/data/sales.csv")
})

test_that("a file browser block lists the directories of its volumes", {

  withr::local_dir(withr::local_tempdir())

  dir.create(file.path("data", "raw"), recursive = TRUE)
  expect_true(all(file.create(c("notes.txt", file.path("data", "sales.csv")))))

  list_dir <- function(...) {
    run_block_request(filebrowser_block, "list_dir", args = list(...))
  }

  vols <- list(here = ".", data = "data")

  expect_identical(
    list_dir(volumes = vols),
    list(dirs = "data", files = "notes.txt")
  )

  expect_identical(
    list_dir(volumes = vols, volume = "data"),
    list(dirs = "raw", files = "sales.csv")
  )

  expect_identical(
    list_dir(volumes = c(here = "."), path = "data"),
    list(dirs = "raw", files = "sales.csv")
  )

  expect_identical(
    list_dir(volumes = vols, volume = "data", path = "raw"),
    list(dirs = character(), files = character())
  )

  expect_error(
    list_dir(volumes = vols, volume = "data", path = ".."),
    class = "filebrowser_path_invalid"
  )

  expect_error(
    list_dir(volumes = vols, path = 1),
    class = "filebrowser_path_invalid"
  )

  expect_error(
    list_dir(volumes = vols, volume = "nope"),
    class = "filebrowser_volume_invalid"
  )

  expect_error(list_dir(volumes = vols, path = "nope"))

  expect_true(filebrowser_block[["requests"]][["list_dir"]][["volatile"]])
})

test_that("the file browser block's fixture", {

  withr::local_dir(withr::local_tempdir())

  dir.create("data")
  expect_true(all(file.create(c("notes.txt", file.path("data", "sales.csv")))))

  vols <- list(here = ".")

  expect_block_fixture(
    filebrowser_block,
    states = list(list(file_path = "data/sales.csv", volumes = vols)),
    requests = list(
      list(name = "list_dir", args = list(volumes = vols)),
      list(name = "list_dir", args = list(volumes = vols, path = "data"))
    )
  )
})
