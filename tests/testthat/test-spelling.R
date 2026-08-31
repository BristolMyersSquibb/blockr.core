test_that("block metadata spelling", {

  skip_on_cran()
  skip_if_not_installed("spelling")

  # Registry metadata never reaches man/: `@blockDescr`, `@blockGuidance` and
  # `@blockArg` text is read by registry consumers only, so the package-level
  # check in tests/spelling.R cannot see it.
  prose <- function(id) {
    c(
      block_meta_name(id),
      block_meta_description(id),
      block_meta_details(id),
      block_meta_guidance(id),
      chr_ply(block_meta_arguments(id), arg_spec_description)
    )
  }

  ids <- list_blocks()

  expect_gt(length(ids), 0L)

  file <- withr::local_tempfile(fileext = ".md")
  writeLines(unlist(lapply(ids, prose)), file)

  words <- spelling::spell_check_files(
    file,
    ignore = readLines(system.file("WORDLIST", package = "blockr.core")),
    lang = "en-US"
  )

  expect_identical(words$word, character())
})
