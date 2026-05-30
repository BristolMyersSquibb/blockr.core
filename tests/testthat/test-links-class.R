test_that("links class", {

  x <- new_link("a", "b")
  y <- new_link("b", "c")
  z <- new_link("c", "a")

  lnk <- links(x, y)

  expect_s3_class(lnk, "links")
  expect_true(is_links(lnk))
  expect_false(is_links("lnk"))
  expect_named(lnk)
  expect_named(links(x = x, y = y), c("x", "y"))

  expect_identical(lnk, as_links(lnk))
  expect_identical(lnk, as_links(as.list(lnk)))

  expect_error(links(x = x, x = y), class = "links_names_unique_invalid")
  expect_error(c(lnk, z), class = "links_acyclic_invalid")

  expect_snapshot(print(links(a = x, b = y)))

  z <- new_link("c", "d")

  lnks <- c(x, y)

  expect_s3_class(lnks, "links")

  expect_s3_class(c(lnks, z), "links")
  expect_s3_class(c(z, lnks), "links")

  lnks <- c(x = x, y = y)

  expect_s3_class(lnks, "links")
  expect_named(lnks, c("x", "y"))

  names(lnks) <- NULL

  expect_named(lnks, c("", ""))

  expect_error(
    names(lnks) <- c("x", "x"),
    class = "links_names_unique_invalid"
  )

  names(lnks) <- c("x", "y")

  expect_error(
    names(lnks)[1] <- "y",
    class = "links_names_unique_invalid"
  )

  expect_named(lnks, c("x", "y"))

  lnks[1] <- list(from = "a", to = "d")
  expect_identical(lnks[["x"]], as_link(list(from = "a", to = "d")))

  lnks[1] <- new_link(from = "a", to = "d")
  expect_identical(lnks[["x"]], as_link(list(from = "a", to = "d")))

  lnks[1] <- links(x = new_link(from = "a", to = "d"))
  expect_identical(lnks[["x"]], as_link(list(from = "a", to = "d")))

  expect_error(
    lnks[1] <- links(y = new_link(from = "a", to = "d")),
    class = "links_assignment_name_invalid"
  )

  lnks[1] <- data.frame(from = "a", to = "d")
  expect_identical(lnks[["x"]], as_link(list(from = "a", to = "d")))

  lnks[[1]] <- list(from = "a", to = "d")
  expect_identical(lnks[["x"]], as_link(list(from = "a", to = "d")))

  lnks[[1]] <- new_link(from = "a", to = "d")
  expect_identical(lnks[["x"]], as_link(list(from = "a", to = "d")))

  expect_error(
    lnks[[1]] <- links(x = new_link(from = "a", to = "d"))
  )

  lnks[[1]] <- data.frame(from = "a", to = "d")
  expect_identical(lnks[["x"]], as_link(list(from = "a", to = "d")))

  expect_identical(as_links(lnks), lnks)
  expect_identical(as_links(as.list(lnks)), lnks)
  expect_identical(as_links(as.data.frame(lnks)), lnks)

  expect_error(
    validate_links("a"),
    class = "links_class_invalid"
  )

  expect_error(
    validate_links(structure(1, class = "links")),
    class = "links_list_like_invalid"
  )

  expect_error(
    validate_links(structure(list(input = "a"), class = "links")),
    class = "links_fields_invalid"
  )

  expect_error(
    validate_links(
      structure(
        list(
          id = c("x", "x"),
          from = c("a", "b"),
          to = c("d",  "c"),
          input = c("", "")
        ),
        class = c("links", "vctrs_rcrd", "vctrs_vctr")
      )
    ),
    class = "links_names_unique_invalid"
  )

  expect_error(
    validate_links(
      structure(
        list(
          id = c("x", "y"),
          from = c("a", "b"),
          to = c("c",  "c"),
          input = c("x", "x")
        ),
        class = c("links", "vctrs_rcrd", "vctrs_vctr")
      )
    ),
    class = "links_block_inputs_invalid"
  )

  expect_error(
    validate_links(
      structure(
        list(
          id = c("x", "y", "z"),
          from = c("a", "b", "c"),
          to = c("b",  "c", "a"),
          input = c("", "", "")
        ),
        class = c("links", "vctrs_rcrd", "vctrs_vctr")
      )
    ),
    class = "links_acyclic_invalid"
  )
})

test_that("links split into singletons", {

  lnk <- links(b = new_link("a", "b"), a = new_link("b", "c"))

  res <- split(lnk)

  expect_type(res, "list")
  expect_length(res, 2L)
  expect_true(all(lgl_ply(res, is_links)))
  expect_identical(int_ply(res, length), c(1L, 1L))
  expect_identical(chr_ply(res, names), c("b", "a"))

  expect_length(split(links()), 0L)

  grp <- split(lnk, c("g", "g"))

  expect_named(grp, "g")
  expect_s3_class(grp[["g"]], "links")
  expect_length(grp[["g"]], 2L)

  fac <- factor(c("g1", "g2"), levels = c("g1", "g2", "g3"))

  expect_named(split(lnk, fac), c("g1", "g2", "g3"))
  expect_named(split(lnk, fac, drop = TRUE), c("g1", "g2"))
})
