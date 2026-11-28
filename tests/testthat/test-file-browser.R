skip_on_os("windows")

test_that("filebrowser block constructor", {

  file <- normalizePath(withr::local_tempfile(lines = "abc"))

  blk <- new_filebrowser_block(volumes = c(tmp = dirname(file)))

  expect_s3_class(blk, "filebrowser_block")
  expect_identical(block_state(blk), list(volumes = c(tmp = dirname(file))))
  expect_identical(
    registry_id_from_block(blk),
    list(package = "blockr.core", id = "filebrowser_block")
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
  expect_identical(res[["reason"]], "Choose a file.")

  res <- evaluate_block(filebrowser_block,
                        list(file_path = "", volumes = vols))

  expect_identical(res[["status"]], "invalid")
  expect_identical(res[["reason"]], "Choose a file.")

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
  writeLines("notes", "notes.txt")
  writeLines(c("region,sales", "north,12"), file.path("data", "sales.csv"))

  list_dir <- function(...) {
    run_block_request(filebrowser_block, "list_dir", args = list(...))
  }

  # The names that a listing holds, of its directories and of its files.
  listed <- function(res) {
    list(dirs = res[["dirs"]][["name"]], files = res[["files"]][["name"]])
  }

  vols <- list(here = ".", data = "data")

  expect_identical(
    listed(list_dir(volumes = vols)),
    list(dirs = "data", files = "notes.txt")
  )

  expect_identical(
    listed(list_dir(volumes = vols, volume = "data")),
    list(dirs = "raw", files = "sales.csv")
  )

  expect_identical(
    listed(list_dir(volumes = c(here = "."), path = "data")),
    list(dirs = "raw", files = "sales.csv")
  )

  expect_identical(
    listed(list_dir(volumes = vols, volume = "data", path = "raw")),
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

test_that("a file browser block lists file sizes, and times in UTC", {

  withr::local_dir(withr::local_tempdir())

  dir.create("raw")
  writeLines(c("region,sales", "north,12"), "sales.csv")

  at <- as.POSIXct("2026-11-20 08:03:00", tz = "UTC")
  expect_true(all(Sys.setFileTime(c("raw", "sales.csv"), at)))

  res <- run_block_request(
    filebrowser_block,
    "list_dir",
    args = list(volumes = list(here = "."))
  )

  expect_identical(
    res[["dirs"]],
    data.frame(name = "raw", modified = "2026-11-20T08:03:00Z")
  )
  expect_identical(
    res[["files"]],
    data.frame(
      name = "sales.csv",
      size = file.size("sales.csv"),
      modified = "2026-11-20T08:03:00Z"
    )
  )
  expect_identical(file.size("sales.csv"), 22)
})

test_that("the file browser block's fixture", {

  withr::local_dir(withr::local_tempdir())

  dir.create("data")
  writeLines("notes", "notes.txt")
  writeLines(c("region,sales", "north,12"), file.path("data", "sales.csv"))

  # Fixed times, so that the fixture records the same answers on every run.
  expect_true(all(Sys.setFileTime(
    c(file.path("data", "sales.csv"), "data", "notes.txt"),
    as.POSIXct(c("2026-11-20 08:03:00", "2026-11-02 09:14:00",
                 "2026-11-19 16:51:00"), tz = "UTC")
  )))

  vols <- list(here = ".")
  at <- function(path) list(volumes = vols, volume = "here", path = path)

  expect_block_fixture(
    filebrowser_block,
    states = list(list(file_path = "./data/sales.csv", volumes = vols)),
    requests = list(
      list(name = "list_dir", args = at("")),
      list(name = "list_dir", args = at("data"))
    )
  )
})

test_that("a board writes the volumes of a file browser block as an object", {

  vols <- c(home = "~", data = "/data")
  board <- new_board(blocks = c(a = new_filebrowser_block(volumes = vols)))

  res <- read_json_strictly(encode_message(blockr_ser(board)))$board

  expect_identical(
    res$blocks$a$state,
    read_json_strictly('{ "volumes": { "home": "~", "data": "/data" } }')
  )
})
