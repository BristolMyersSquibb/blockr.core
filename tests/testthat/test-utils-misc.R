test_that("exprs_to_lang", {

  eval_exprs <- function(exprs) {
    eval(exprs_to_lang(exprs), list())
  }

  deparse_exprs <- function(exprs) {
    deparse(call("local", exprs_to_lang(exprs)))
  }

  expected_eval <- 3
  expected_dparse <- c(
    "local({",
    "    x <- 1",
    "    y <- 2",
    "    x + y", "})"
  )

  a <- quote(
    {
      x <- 1
      y <- 2
      x + y
    }
  )

  expect_identical(eval_exprs(a), expected_eval)
  expect_identical(deparse_exprs(a), expected_dparse)

  b <- parse(
    text = "
      x <- 1
      y <- 2
      x + y
    "
  )

  expect_identical(eval_exprs(b), expected_eval)
  expect_identical(deparse_exprs(b), expected_dparse)

  c <- call("{", quote(x <- 1), quote(y <- 2), quote(x + y))

  expect_identical(eval_exprs(c), expected_eval)
  expect_identical(deparse_exprs(c), expected_dparse)

  d <- list(quote(x <- 1), quote(y <- 2), quote(x + y))

  expect_identical(eval_exprs(d), expected_eval)
  expect_identical(deparse_exprs(d), expected_dparse)

  e <- expression(x <- 1, y <- 2, x + y)

  expect_identical(eval_exprs(e), expected_eval)
  expect_identical(deparse_exprs(e), expected_dparse)

  f <- quote(1 + 2)

  expect_identical(eval_exprs(f), expected_eval)
  expect_identical(deparse_exprs(f), "local(1 + 2)")

  g <- quote(sum(1, 2))

  expect_identical(eval_exprs(g), expected_eval)
  expect_identical(deparse_exprs(g), "local(sum(1, 2))")

  f <- 3

  expect_identical(eval_exprs(f), expected_eval)
  expect_identical(deparse_exprs(f), "local(3)")
})

test_that("rand_names()", {

  id <- rand_names(n = 1L)

  expect_type(id, "character")
  expect_length(id, 1L)

  expect_identical(
    rand_names(n = 1L, id_fun = function(n) rep("a", n)),
    "a"
  )

  expect_error(
    rand_names(n = 2L, max_tries = 5, id_fun = function(n) rep("a", n)),
    class = "id_creation_unsuccessful"
  )

  expect_error(
    rand_names("a", n = 1L, max_tries = 5, id_fun = function(n) rep("a", n)),
    class = "id_creation_unsuccessful"
  )
})

test_that("sentence case", {

  expect_identical(
    to_sentence_case("tiny_aidi", "_", " "),
    "Tiny aidi"
  )

  expect_identical(
    to_sentence_case("key_sick_puma", "_", " "),
    "Key sick puma"
  )

  expect_identical(
    to_sentence_case("HAZY_PINK_RHEA", "_", " "),
    "Hazy pink rhea"
  )

  expect_identical(
    to_sentence_case(c("used_sad_olm", "bad_numb_rhea"), "_", " "),
    c("Used sad olm", "Bad numb rhea")
  )

  expect_identical(
    to_sentence_case("firmHare", "([A-Z])", " \\1"),
    "Firm hare"
  )

  expect_identical(
    to_sentence_case("thinCuteDove", "([A-Z])", " \\1"),
    "Thin cute dove"
  )

  expect_identical(
    to_sentence_case("FitFunFly", "([A-Z])", " \\1"),
    "Fit fun fly"
  )

  expect_identical(
    to_sentence_case("313d964aca2"),
    "313d964aca2"
  )

  expect_identical(
    to_sentence_case("a13d964aca2"),
    "A13d964aca2"
  )

  expect_identical(
    to_sentence_case("d12c577e-f94a-4d8a-b6cf-eab63b854fcf"),
    "D12c577e-f94a-4d8a-b6cf-eab63b854fcf"
  )

  expect_identical(
    to_sentence_case("d12c577e-f94a-4d8a-b6cf-eab63b854fcf", "-", " "),
    "D12c577e f94a 4d8a b6cf eab63b854fcf"
  )

  expect_identical(
    to_sentence_case(""),
    ""
  )

  expect_identical(
    to_sentence_case(NA_character_),
    ""
  )

  expect_identical(
    to_sentence_case(NULL),
    ""
  )

  expect_identical(
    id_to_sentence_case(
      c("thinCuteDove", "gold-busy-rail", "key_sick_puma", "left.tart.naga")
    ),
    c("Thin cute dove", "Gold busy rail", "Key sick puma", "Left tart naga")
  )
})
