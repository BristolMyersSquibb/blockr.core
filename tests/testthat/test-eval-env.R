test_that("eval_env defaults to baseenv (utils not visible)", {
  e <- eval_env(list())
  expect_identical(parent.env(e), baseenv())
  expect_error(eval(quote(head(1:10)), e), "could not find function")
})

test_that("attach_default_packages exposes vanilla R defaults", {
  withr::with_options(list(blockr.attach_default_packages = TRUE), {
    e <- eval_env(list())
    expect_equal(eval(quote(head(1:10, 3)), e), 1:3)
    expect_s3_class(
      eval(quote(lm(y ~ x, data.frame(x = 1:3, y = c(2, 4, 6)))), e),
      "lm"
    )
    expect_s3_class(eval(quote(iris), e), "data.frame")
    expect_equal(dim(eval(quote(mtcars), e)), c(32L, 11L))
    expect_true(is.function(eval(quote(setMethod), e)))
    expect_true(is.function(eval(quote(png), e)))
  })
})

test_that("non-default base packages remain namespace-only", {
  withr::with_options(list(blockr.attach_default_packages = TRUE), {
    e <- eval_env(list())
    expect_error(
      eval(quote(mclapply(1:2, identity)), e),
      "could not find function"
    )
    expect_error(
      eval(quote(viewport()), e),
      "could not find function"
    )
  })
})

test_that("evals do not leak state across calls", {
  withr::with_options(list(blockr.attach_default_packages = TRUE), {
    eval(quote(x <- 1), eval_env(list()))
    expect_false(exists("x", envir = eval_env(list()), inherits = FALSE))
  })
})

test_that("default eval parent chain is locked end-to-end", {
  withr::with_options(list(blockr.attach_default_packages = TRUE), {
    parent <- parent.env(eval_env(list()))
    while (!identical(parent, baseenv())) {
      expect_true(environmentIsLocked(parent))
      expect_error(
        assign("head", 1L, envir = parent),
        "locked"
      )
      parent <- parent.env(parent)
    }
  })
})

test_that("default eval parent is cached across calls", {
  withr::with_options(list(blockr.attach_default_packages = TRUE), {
    expect_identical(default_eval_parent(), default_eval_parent())
    expect_identical(parent.env(eval_env(list())), default_eval_parent())
  })
})

test_that("data inputs are bound in the eval env", {
  e <- eval_env(list(x = 1:5, y = letters[1:3]))
  expect_equal(get("x", envir = e), 1:5)
  expect_equal(get("y", envir = e), letters[1:3])
})

test_that("lazy-loaded datasets resolve as promises, not active bindings", {
  withr::with_options(list(blockr.attach_default_packages = TRUE), {
    parent <- parent.env(eval_env(list()))
    while (!exists("iris", envir = parent, inherits = FALSE)) {
      parent <- parent.env(parent)
    }
    expect_false(bindingIsActive("iris", parent))
  })
})
