test_that("code export assigns each block expression to its block id", {

  code <- export_code(
    list(
      a = quote(datasets::BOD),
      b = quote(datasets::ChickWeight),
      c = quote(merge(a, b, by = "Time"))
    )
  )

  expect_named(code, c("a", "b", "c"))
  expect_identical(code[["a"]], quote(a <- datasets::BOD))
  expect_identical(code[["c"]], quote(c <- merge(a, b, by = "Time")))

  expect_named(
    export_code(list(c = quote(f(a)), a = quote(g()))),
    c("c", "a")
  )

  expect_length(export_code(list()), 0L)

  env <- new.env()

  for (assignment in code) {
    eval(assignment, env)
  }

  expect_identical(
    get("c", envir = env),
    merge(datasets::BOD, datasets::ChickWeight, by = "Time")
  )

  expect_error(export_code(list(quote(a))), class = "export_code_ids_invalid")
  expect_error(
    export_code(list(a = quote(a), a = quote(b))),
    class = "export_code_ids_invalid"
  )
  expect_error(export_code(list(a = 1)), class = "export_code_exprs_invalid")
})
