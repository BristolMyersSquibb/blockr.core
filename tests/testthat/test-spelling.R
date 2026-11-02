test_that("block metadata spelling", {

  skip_on_cran()
  skip_if_not_installed("spelling")

  # Registry metadata never reaches man/: `@blockDescr`, `@blockGuidance` and
  # `@blockArg` text is read by registry consumers only, so the package-level
  # check in tests/spelling.R cannot see it.
  prose <- function(entry) {
    c(
      entry[["name"]],
      entry[["description"]],
      entry[["details"]],
      entry[["guidance"]],
      unlist(lst_xtr(entry[["arguments"]], "description"))
    )
  }

  entries <- yaml::read_yaml(
    system.file("registry", "blocks.yml", package = "blockr.core")
  )

  expect_gt(length(entries), 0L)

  file <- withr::local_tempfile(fileext = ".md")
  writeLines(unlist(lapply(entries, prose)), file)

  words <- spelling::spell_check_files(
    file,
    ignore = readLines(system.file("WORDLIST", package = "blockr.core")),
    lang = "en-US"
  )

  expect_identical(words$word, character())
})
