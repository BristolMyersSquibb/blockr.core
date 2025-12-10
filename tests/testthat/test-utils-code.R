test_that("cove generation utils", {

  expect_false(has_assignment(quote(x + y)))
  expect_true(has_assignment(quote(x <- 5)))

  expect_warning(
    expect_true(has_assignment(quote(x <<- 5))),
    class = "code_generation_discouraged_assignments"
  )


  expect_warning(
    expect_true(has_assignment(quote(assign("x", 5)))),
    class = "code_generation_discouraged_assignments"
  )

  expect_true(has_assignment(quote({y <- 1; z <- 2})))

  expect_true(has_assignment(quote(f(x <- 5))))
})
