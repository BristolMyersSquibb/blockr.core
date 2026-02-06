test_that("registry", {

  curr <- list_blocks()

  withr::defer(
    {
      unregister_blocks()
      register_core_blocks(curr)
    }
  )

  unregister_blocks()

  expect_length(list_blocks(), 0L)

  candidates <- ls(
    envir = asNamespace("blockr.core"),
    pattern = "^new_.+_block$"
  )

  is_cand <- lgl_ply(
    candidates,
      function(x) {

      res <- try(do.call(x, list(block_metadata = list())), silent = TRUE)

      if (inherits(res, "try-error")) {
        return(FALSE)
      }

      length(grep("_block$", class(res), value = TRUE)) > 1L
    }
  )

  candidates <- candidates[is_cand]

  cand_1 <- do.call(candidates[1L], list(block_metadata = list()))

  expect_identical(
    registry_id_from_block(cand_1),
    character()
  )

  register_core_blocks("all")

  expect_setequal(
    paste0("new_", list_blocks()),
    candidates
  )

  expect_identical(
    registry_id_from_block(cand_1),
    sub("^new_", "", candidates[1L])
  )

  expect_error(register_block(candidates[1L], "test", "test"))

  expect_error(
    register_block(get(candidates[1L]), "test", "test"),
    class = "block_already_in_registry"
  )

  expect_error(
    register_block(candidates[1L], "test", "test", package = "blockr.core"),
    class = "block_already_in_registry"
  )

  rlang::reset_warning_verbosity("block_category_test_discouraged")

  expect_warning(
    register_block(get(candidates[1L]), "test", "test", category = "test",
                   overwrite = TRUE),
    class = "block_category_discouraged"
  )

  rlang::reset_warning_verbosity("block_icon_cloud-arrow-up-fill_discouraged")

  expect_warning(
    register_block(get(candidates[1L]), "test", "test",
                   icon = "cloud-arrow-up-fill", overwrite = TRUE),
    class = "block_icon_fill_discouraged"
  )

  expect_error(
    register_block(get(candidates[1L]), "test", "test", icon = "test"),
    class = "block_icon_invalid"
  )
})

test_that("registry metadata", {

  meta <- block_metadata()

  expect_s3_class(meta, "data.frame")
  expect_identical(nrow(meta), length(list_blocks()))
  expect_identical(ncol(meta), 7L)
  expect_named(
    meta,
    c("id", "name", "description", "category", "icon", "arguments", "package")
  )

  meta <- block_metadata(list_blocks()[1L], "name")

  expect_type(meta, "character")
  expect_length(meta, 1L)

  meta <- block_metadata(fields = "name")

  expect_type(meta, "character")
  expect_length(meta, length(list_blocks()))
  expect_named(meta, list_blocks())

  icon <- default_icon(default_category())

  expect_type(icon, "character")
  expect_length(icon, 1L)
})
