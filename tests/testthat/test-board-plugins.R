test_that("plugins", {

  a <- preserve_board()
  b <- manage_blocks()

  expect_s3_class(a, "plugin")
  expect_s3_class(a, "preserve_board")
  expect_true(is_plugin(a))

  expect_snapshot(print(a))

  expect_error(
    validate_plugin("a"),
    class = "plugin_inheritance_invalid"
  )

  expect_error(
    validate_plugin(
      structure("a", class = c("preserve_board", "plugin"))
    ),
    class = "plugin_type_invalid"
  )

  expect_error(
    validate_plugin(
      structure(list("a"), class = c("preserve_board", "plugin"))
    ),
    class = "plugin_components_invalid"
  )

  expect_error(
    validate_plugin(
      structure(
        list(server = "a", ui = NULL),
        class = c("preserve_board", "plugin")
      )
    ),
    class = "plugin_server_invalid"
  )

  expect_error(
    validate_plugin(
      structure(
        list(server = identity, ui = "a"),
        class = c("preserve_board", "plugin")
      )
    ),
    class = "plugin_ui_invalid"
  )

  expect_error(
    validate_plugin(
      structure(
        list(server = identity, ui = identity),
        validator = "a",
        class = c("preserve_board", "plugin")
      )
    ),
    class = "plugin_validator_invalid"
  )

  expect_identical(as_plugin(as.list(a)), a)
  expect_identical(as_plugin(as_plugins(a)), a)

  c <- manage_links()
  d <- manage_stacks()

  cd <- plugins(c, d)

  expect_s3_class(cd, "plugins")
  expect_true(is_plugins(cd))
  expect_length(cd, 2L)

  expect_snapshot(print(cd))

  expect_error(
    validate_plugins("a"),
    class = "plugins_inheritance_invalid"
  )

  expect_error(
    validate_plugins(
      structure(
        "a",
        class = c("plugins", "list", "vctrs_vctr", "list")
      )
    ),
    class = "plugins_type_invalid"
  )

  expect_error(
    validate_plugins(
      structure(
        list(a, a),
        class = c("plugins", "list", "vctrs_vctr", "list")
      )
    ),
    class = "plugins_duplicate_invalid"
  )

  expect_identical(as_plugins(cd), cd)
  expect_identical(as_plugins(as.list(cd)), cd)

  expect_identical(c(a), as_plugins(a))
  expect_identical(c(cd), cd)

  expect_identical(
    names(cd),
    c("manage_links", "manage_stacks")
  )

  names(cd) <- NULL

  expect_identical(
    names(cd),
    c("manage_links", "manage_stacks")
  )

  expect_error(
    names(cd) <- c("c", "d"),
    class = "plugin_names_assign_invalid"
  )

  res <- c(a, cd)

  expect_s3_class(res, "plugins")
  expect_length(res, 3L)

  res <- c(a, b)

  expect_s3_class(res, "plugins")
  expect_length(res, 2L)

  res <- c(a, b, c)

  expect_s3_class(res, "plugins")
  expect_length(res, 3L)

  res <- c(a, list(c, d))

  expect_s3_class(res, "plugins")
  expect_length(res, 3L)

  res <- c(cd, a)

  expect_s3_class(res, "plugins")
  expect_length(res, 3L)

  res <- c(cd, list(a))

  expect_s3_class(res, "plugins")
  expect_length(res, 3L)

  expect_equal(cd[1L], as_plugins(c), ignore_function_env = TRUE)
  expect_equal(cd[c(1L, 2L)], cd, ignore_function_env = TRUE)

  expect_equal(cd["manage_links"], as_plugins(c), ignore_function_env = TRUE)
  expect_equal(cd[c("manage_links", "manage_stacks")], cd,
               ignore_function_env = TRUE)

  expect_error(
    cd[1] <- c,
    class = "plugins_assignment_invalid"
  )

  expect_equal(cd[[1L]], c, ignore_function_env = TRUE)
  expect_equal(cd[["manage_links"]], c, ignore_function_env = TRUE)

  expect_error(
    cd[[1]] <- c,
    class = "plugins_assignment_invalid"
  )
})

test_that("check default validator", {

  expect_null(abort_not_null(NULL))
  expect_invisible(abort_not_null(NULL))

  expect_error(
    abort_not_null(1),
    class = "expect_not_null"
  )
})
