test_that("block metadata spelling", {

  skip_on_cran()
  skip_if_not_installed("spelling")

  # Registry metadata never reaches man/: `@blockDescr` and `@blockGuidance`
  # text is read by registry consumers only, and so are the descriptions of
  # state args in the schemas of core's block classes, so the package-level
  # check in tests/spelling.R cannot see them.
  prose <- function(entry) {
    c(
      entry[["name"]],
      entry[["description"]],
      entry[["details"]],
      entry[["guidance"]]
    )
  }

  entries <- yaml::read_yaml(
    system.file("registry", "blocks.yml", package = "blockr.core")
  )

  expect_gt(length(entries), 0L)

  schemas <- list.files(
    system.file("js", package = "blockr.core"),
    pattern = "-block[.]schema[.]json$",
    full.names = TRUE
  )

  expect_gt(length(schemas), 0L)

  args <- lapply(
    schemas,
    function(file) {
      props <- yyjsonr::read_json_file(file)[["properties"]]
      unlist(lst_xtr(props, "description"))
    }
  )

  file <- withr::local_tempfile(fileext = ".md")
  writeLines(c(unlist(lapply(entries, prose)), unlist(args)), file)

  words <- spelling::spell_check_files(
    file,
    ignore = readLines(system.file("WORDLIST", package = "blockr.core")),
    lang = "en-US"
  )

  expect_identical(words$word, character())
})
