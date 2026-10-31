skip_on_os("windows")

test_that("filebrowser block constructor", {

  file <- normalizePath(withr::local_tempfile(lines = "abc"))

  blk <- new_filebrowser_block(volumes = c(tmp = dirname(file)))

  expect_s3_class(blk, "filebrowser_block")

  vols <- filebrowser_volumes()

  expect_type(vols, "character")
  expect_named(vols, "home")

  vols <- filebrowser_volumes(file.path("foo", "bar"))

  expect_type(vols, "character")
  expect_named(vols, "volume")

  vols <- filebrowser_volumes("foo/bar:foo/baz")

  expect_type(vols, "character")
  expect_named(vols, c("volume1", "volume2"))
})
