test_that("bbquote", {

  expect_identical(
    bbquote(func(.(a), .(a)), list(a = "a")),
    quote(func("a", "a"))
  )

  expect_identical(
    bbquote(func(..(a), ..(a)), list(a = 1:2), splice = TRUE),
    quote(func(1L, 2L, 1L, 2L))
  )

  expr1 <- quote(func(..(a), ..(b)))

  expect_identical(
    bbquote(expr1, list()),
    expr1
  )

  expect_identical(
    expr2 <- bbquote(expr1, list(a = list(x = 1, y = 2)), splice = TRUE),
    quote(func(x = 1, y = 2, ..(b)))
  )

  expect_identical(
    expr3 <- bbquote(expr2, list(b = list(z = 3)), splice = TRUE),
    quote(func(x = 1, y = 2, z = 3))
  )

  expr4 <- bbquote(
    plot(..(args), main = .(title)),
    list(args = list(x = 1:10, y = 1:10)),
    splice = TRUE
  )

  expect_identical(
    deparse(expr4),
    "plot(x = 1:10, y = 1:10, main = .(title))"
  )

  expr5 <- bbquote(expr4, list(title = "My Plot"))

  expect_identical(
    deparse(expr5),
    "plot(x = 1:10, y = 1:10, main = \"My Plot\")"
  )

  expect_identical(
    bbquote(func(a = 1L, ..(a)), list()),
    quote(func(a = 1L, ..(a)))
  )

  expect_identical(
    bbquote(func(a = 1L, ..(a)), list(a = c("a", "b")), splice = TRUE),
    quote(func(a = 1L, "a", "b"))
  )
})
