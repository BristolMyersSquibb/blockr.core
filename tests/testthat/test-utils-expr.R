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

  expect_identical(
    bbquote(func(a = 1L, .(a)), list(a = "a")),
    quote(func(a = 1L, "a"))
  )

  expect_identical(
    eval(
      bbquote(
        {
          b <- 1L
          .(a) + b
        },
        list(a = 2L)
      )
    ),
    3L
  )

  expect_identical(
    eval(
      bbquote(
        {
          a <- 1L
          .(a) + a
        },
        list(a = 2L)
      )
    ),
    3L
  )

  expect_identical(
    eval(bbquote(1L + 2L, list())),
    3L
  )

  expect_identical(
    eval(bbquote(1L + 2L, list(a = 3L))),
    3L
  )

  expect_identical(bquote(), bbquote())

  expect_identical(
    bbquote(func(.(a), .(a)), list2env(list(a = "a"), parent = emptyenv())),
    quote(func("a", "a"))
  )

  expect_error(.("a"), class = "dot_should_not_be_called")
  expect_error(..("a"), class = "dots_should_not_be_called")
})

test_that("bbquote keeps NULL call elements", {

  expect_identical(
    bbquote(func(a, NULL), list()),
    quote(func(a, NULL))
  )

  expect_identical(
    bbquote(list(a = NULL, b = .(x)), list(x = 1L)),
    quote(list(a = NULL, b = 1L))
  )

  expect_identical(
    bbquote(func(inner(NULL, .(x))), list(x = 1L)),
    quote(func(inner(NULL, 1L)))
  )
})

test_that("bbquote handles a function definition parsed without srcrefs", {

  # A `function` call carries its srcref as a fourth element, and that slot is
  # NULL whenever the code was parsed without srcrefs -- what an installed
  # package does (`keep.source = FALSE`) and what `load_all()` does not.
  expr <- parse(
    text = "local({ f <- function(v, w) v; f(.(x), 1L) })",
    keep.source = FALSE
  )[[1L]]

  fn_def <- as.list(expr[[2L]][[2L]][[3L]])

  expect_length(fn_def, 4L)
  expect_null(fn_def[[4L]])

  expect_identical(eval(bbquote(expr, list(x = 1L))), 1L)

  expect_identical(bbquote(expr, list())[[2L]][[3L]][[2L]], quote(.(x)))
})

test_that("bbquote preserves missing arguments", {

  expect_identical(bbquote(x[.(i), ], list(i = 1L)), quote(x[1L, ]))
  expect_identical(bbquote(x[, .(j)], list(j = 2L)), quote(x[, 2L]))
})
