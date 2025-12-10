test_that("cove generation utils", {

  expect_false(has_assignment(quote(x + y)))
  expect_true(has_assignment(quote(x <- 5)))
  expect_false(has_assignment(quote(f(x, y))))
  expect_false(has_assignment(quote(1 + 2)))
  expect_false(has_assignment(quote(x)))

  expect_warning(
    expect_true(has_assignment(quote(x <<- 5))),
    class = "code_generation_discouraged_assignments"
  )


  expect_warning(
    expect_true(has_assignment(quote(assign("x", 5)))),
    class = "code_generation_discouraged_assignments"
  )

  expect_true(
    has_assignment(
      quote(
        {
          x <- 1
        }
      )
    )
  )

  expect_true(
    has_assignment(
      quote(
        {
          y <- 1
          z <- 2
        }
      )
    )
  )

  expect_true(has_assignment(quote(x <- 5)))
  expect_true(has_assignment(quote(y <- x + 1)))
  expect_true(has_assignment(quote(result <- f(x))))

  expect_true(has_assignment(quote(f(x <- 5))))
  expect_true(has_assignment(quote(g(a, b <- 2))))
  expect_true(has_assignment(quote(plot(x, y <- x^2))))

  expect_false(
    has_assignment(
      quote(function() x <- 5)
    )
  )

  expect_false(
    has_assignment(
      quote(
        function(a) {
          x <- a
        }
      )
    )
  )

  expect_false(
    has_assignment(
      quote(
        function(a, b) {
          x <- a + b
          y <- x * 2
        }
      )
    )
  )

  expect_false(
    has_assignment(
      quote(
        \() {
          x <- 5
        }
      )
    )
  )

  expect_false(
    has_assignment(
      quote(
        \(a) {
          x <- a
        }
      )
    )
  )

  expect_false(
    has_assignment(
      quote(\(a, b) x <- a + b)
    )
  )

  expect_false(
    has_assignment(
      quote(
        local(
          {
            x <- 5
          }
        )
      )
    )
  )

  expect_false(
    has_assignment(
      quote(
        local(
          {
            x <- 1
            y <- 2
          }
        )
      )
    )
  )

  expect_warning(
    expect_true(has_assignment(quote(x <<- 5))),
    class = "code_generation_discouraged_assignments"
  )

  expect_warning(
    expect_true(has_assignment(quote(f(x <<- 5)))),
    class = "code_generation_discouraged_assignments"
  )

  expect_warning(
    expect_true(
      has_assignment(
        quote(
          function() {
            x <<- 5
          }
        )
      )
    ),
    class = "code_generation_discouraged_assignments"
  )

  expect_warning(
    expect_true(
      has_assignment(
        quote(
          local(
            {
              x <<- 5
            }
          )
        )
      )
    ),
    class = "code_generation_discouraged_assignments"
  )

  expect_warning(
    expect_true(has_assignment(quote(\() x <<- 5))),
    class = "code_generation_discouraged_assignments"
  )

  expect_warning(
    expect_true(has_assignment(quote(assign("x", 5)))),
    class = "code_generation_discouraged_assignments"
  )

  expect_warning(
    expect_true(has_assignment(quote(f(assign("x", 5))))),
    class = "code_generation_discouraged_assignments"
  )

  expect_warning(
    expect_true(has_assignment(quote(function() assign("x", 5)))),
    class = "code_generation_discouraged_assignments"
  )

  expect_warning(
    expect_true(has_assignment(quote(local(assign("x", 5))))),
    class = "code_generation_discouraged_assignments"
  )

  expect_warning(
    expect_true(has_assignment(quote(\() assign("x", 5)))),
    class = "code_generation_discouraged_assignments"
  )

  expect_true(has_assignment(quote(f(g(x <- 5)))))

  expect_false(
    has_assignment(
      quote(
        lapply(
          data,
          function(x) {
            y <- x * 2
          }
        )
      )
    )
  )

  expect_true(
    has_assignment(quote(lapply(data <- getData(), function(x) x * 2)))
  )

  expect_warning(
    expect_true(
      has_assignment(
        quote(
          function() {
            x <<- 5
          }
        )
      )
    ),
    class = "code_generation_discouraged_assignments"
  )

  expect_warning(
    expect_true(
      has_assignment(
        quote(
          local(
            {
              x <<- 5
              y <- 3
            }
          )
        )
      )
    ),
    class = "code_generation_discouraged_assignments"
  )

  expect_true(
    has_assignment(
      quote(
        {
          x <- 5
          f <- function() {
            y <- 10
          }
        }
      )
    )
  )

  expect_true(
    has_assignment(
      quote(
        {
          f <- function() {
            y <- 10
          }
        }
      )
    )
  )

  expect_false(has_assignment(quote({})))
  expect_false(has_assignment(quote(42)))
  expect_false(has_assignment(quote("hello")))
  expect_false(has_assignment(quote(NULL)))
  expect_true(has_assignment(quote(x <- 5)))

  expect_true(
    has_assignment(
      quote({
        if (x <- 5) {
          print(x)
        }
      })
    )
  )

  expect_false(
    has_assignment(
      quote(
        {
          for (i in 1:10) {
            print(i)
          }
        }
      )
    )
  )

  expect_true(
    has_assignment(
      quote(
        {
          for (i in 1:10) {
            x <- i
          }
        }
      )
    )
  )

  expect_true(
    has_assignment(
      quote(
        {
          while (x <- next_value()) {
            process(x)
          }
        }
      )
    )
  )
})
