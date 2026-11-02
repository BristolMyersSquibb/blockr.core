test_that("a call or a name becomes a function of the inputs", {

  expect_identical(
    interpolate(quote(utils::head(data, n = 10L)), list(data = "a")),
    quote((function(data) utils::head(data, n = 10L))(data = a))
  )

  expect_identical(
    interpolate(quote(merge(x, y)), list(x = "a", y = "b")),
    quote((function(x, y) merge(x, y))(x = a, y = b))
  )

  expect_identical(
    interpolate(quote(rbind(...)), list(... = list("a", b2 = "b"))),
    quote((function(...) rbind(...))(a, b2 = b))
  )

  expect_identical(
    interpolate(quote(datasets::iris), list()),
    quote((function() datasets::iris)())
  )

  expect_identical(
    interpolate(quote(data), list(data = "a")),
    quote((function(data) data)(data = a))
  )

  braced <- quote({
    x <- 1
    data[x, ]
  })

  expect_identical(
    interpolate(braced, list(data = "a")),
    quote(
      (function(data) {
        x <- 1
        data[x, ]
      })(data = a)
    )
  )

  expect_error(interpolate(1, list()), class = "interpolate_invalid")
  expect_error(interpolate(list(), list()), class = "interpolate_invalid")
})

test_that("a string is parsed into such a call", {

  expect_identical(
    interpolate("utils::head(data, n = 10L)", list(data = "a")),
    quote((function(data) utils::head(data, n = 10L))(data = a))
  )

  expect_identical(
    interpolate(c("x <- 1L", "data[x, ]"), list(data = "a")),
    quote(
      (function(data) {
        x <- 1L
        data[x, ]
      })(data = a)
    )
  )

  expect_error(interpolate("utils::head(", list(data = "a")))
})

test_that("a function's .() is filled in from its environment", {

  make <- function(n = 10L) function(data) utils::head(data, n = .(n))

  expect_identical(
    interpolate(make(), list(data = "a")),
    quote((function(data) utils::head(data, n = 10L))(data = a))
  )

  unset <- function(n) function(data) utils::head(data, n = .(n))

  expect_true(
    is_missing_arg_error(
      tryCatch(interpolate(unset(), list(data = "a")), error = identity)
    )
  )

  expect_error(
    interpolate(make(), list(x = "a")),
    class = "interpolate_invalid"
  )
})

test_that("a bquoted_expr has its placeholders filled in with the ids", {

  n <- 10L

  expect_identical(
    interpolate(bbquote(utils::head(.(data), n = .(n))), list(data = "a")),
    quote(utils::head(a, n = 10L))
  )

  expect_identical(
    interpolate(bbquote(merge(.(x), .(y), by = "k")), list(x = "a", y = "b")),
    quote(merge(a, b, by = "k"))
  )

  expect_identical(
    interpolate(bbquote(rbind(...)), list(... = list("a", b2 = "b"))),
    quote(rbind(a, b2 = b))
  )

  expect_identical(
    interpolate(bbquote(.(data)), list(data = "a")),
    quote(a)
  )

  threshold <- 5

  expect_identical(
    interpolate(
      bbquote(subset(.(data), threshold > .(threshold))),
      list(data = "a")
    ),
    quote(subset(a, threshold > 5))
  )
})

test_that("placeholders are filled in wherever they are", {

  expect_identical(
    interpolate(
      bbquote(lapply(.(data), function(col) mean(col, ...))),
      list(data = "a")
    ),
    quote(lapply(a, function(col) mean(col, ...)))
  )

  expect_identical(
    interpolate(
      bbquote(Map(function(...) sum(...), ...)),
      list(... = list("a", "b"))
    ),
    quote(Map(function(...) sum(...), a, b))
  )

  expect_identical(
    interpolate(bbquote(.(data)[, 1]), list(data = "a")),
    quote(a[, 1])
  )

  expect_identical(
    interpolate(bbquote(.(data)[.(data)$x > 1, ]), list(data = "a")),
    quote(a[a$x > 1, ])
  )

  expect_identical(
    interpolate(bbquote(dt[, .(x)]), list(data = "a")),
    quote(dt[, .(x)])
  )

  expect_identical(
    interpolate(bbquote(.(data)[, .(x)]), list(data = "a", x = "b")),
    quote(a[, b])
  )

  expect_identical(
    interpolate(bbquote(f(NULL, .(data))), list(data = "a")),
    quote(f(NULL, a))
  )

  expect_identical(
    interpolate(bbquote(lapply(.(data), function(x) NULL)), list(data = "a")),
    quote(lapply(a, function(x) NULL))
  )
})

test_that("bquoted_exprs print as the call they hold", {
  expect_snapshot(print(bbquote(utils::head(.(data), n = 10L))))
})
