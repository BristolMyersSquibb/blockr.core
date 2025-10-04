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
