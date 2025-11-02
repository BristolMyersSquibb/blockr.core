test_that("registry", {

  curr <- list_blocks()

  withr::defer(
    register_core_blocks(curr)
  )

  unregister_blocks()

  expect_length(list_blocks(), 0L)

  candidates <- ls(
    envir = asNamespace("blockr.core"),
    pattern = "^new_.+_block$"
  )

  candidates <- candidates[lgl_ply(candidates, function(x) {

    res <- try(do.call(x, list()), silent = TRUE)

    if (inherits(res, "try-error")) {
      return(FALSE)
    }

    length(grep("_block$", class(res), value = TRUE)) > 1L
  })]

  register_core_blocks("all")

  expect_setequal(
    paste0("new_", list_blocks()),
    candidates
  )
})

test_that("registry metadata", {

  meta <- block_metadata()

  expect_s3_class(meta, "data.frame")
  expect_identical(nrow(meta), length(list_blocks()))
  expect_identical(ncol(meta), 6L)
  expect_named(
    meta,
    c("id", "name", "description", "category", "icon", "package")
  )

  meta <- block_metadata(list_blocks()[1L], "name")

  expect_type(meta, "character")
  expect_length(meta, 1L)

  meta <- block_metadata(fields = "name")

  expect_type(meta, "character")
  expect_length(meta, length(list_blocks()))
  expect_named(meta, list_blocks())
})
