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

test_that("link IDs are neither empty nor made of digits alone", {

  expect_error(
    links(ab = new_link("a", "b"), `3` = new_link("b", "c")),
    "\"3\"",
    class = "links_names_invalid"
  )

  expect_error(
    validate_links(
      structure(
        list(id = "", from = "a", to = "b", input = ""),
        class = c("links", "vctrs_rcrd", "vctrs_vctr")
      )
    ),
    class = "links_names_invalid"
  )
})

test_that("a links container has a compact str_value()", {

  lks <- as_links(
    list(
      l1 = new_link(from = "a", to = "b"),
      l2 = new_link(from = "c", to = "d", input = "z")
    )
  )

  expect_identical(
    str_value(lks),
    "<links[2]>\n  l1: <link> a -> b\n  l2: <link> c -> d (z)"
  )

  expect_identical(str_value(links()), "<links[0]>")

  expect_identical(capture.output(str(lks))[1L], " <links[2]>")
})
