test_that("link class", {

  x <- new_link("a", "b")

  expect_s3_class(x, "link")
  expect_snapshot(print(x))

  expect_error(
    validate_link("abc"),
    class = "link_class_invalid"
  )

  expect_error(
    validate_link(new_vctr("abc", class = "link")),
    class = "link_list_like_invalid"
  )

  expect_error(
    validate_link(new_vctr(list("abc"), class = "link")),
    class = "link_components_missing"
  )

  expect_error(
    validate_link(
      new_vctr(list(from = "abc", to = 1, input = ""), class = "link")
    ),
    class = "link_components_invalid"
  )

  expect_error(new_link("a", c("b", "c")), class = "link_components_invalid")
  expect_error(new_link(character(), "b"), class = "link_components_invalid")

  expect_error(
    validate_link(
      new_vctr(
        list(from = "abc", to = "abc", input = ""),
        class = "link"
      )
    ),
    class = "link_self_referencing"
  )

  lnk <- new_link("a", "b")

  dtf <- as.data.frame(lnk)
  lst <- as.list(lnk)
  chr <- as.character(lnk)

  expect_s3_class(c(lnk, dtf), "links")
  expect_s3_class(c(lnk, lst), "links")
  expect_s3_class(c(lnk, chr), "links")
})

test_that("a link takes core's properties and nothing else", {

  expect_error(new_link("a", "b", "x", "y"), "unused argument")
  expect_error(new_link("a", "b", weight = 1), "unused argument")
  expect_error(new_link("a", "b", class = "weighted_link"), "unused argument")

  expect_error(links(from = "a", to = "b", weight = 1), "unused argument")
  expect_error(
    as_link(list(from = "a", to = "b", weight = 1)),
    "unused argument"
  )
})

test_that("a link has a compact str_value()", {

  expect_identical(
    str_value(new_link(from = "a", to = "b", input = "x")),
    "<link> a -> b (x)"
  )
  expect_identical(str_value(new_link(from = "a", to = "b")), "<link> a -> b")
  expect_identical(str_value(new_link()), "<link> ? -> ?")

  expect_identical(
    capture.output(str(new_link(from = "a", to = "b"))),
    " <link> a -> b"
  )
})
