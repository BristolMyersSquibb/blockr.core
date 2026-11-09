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

  expect_error(exprs_to_lang(quote(x)), class = "exprs_to_lang_invalid")
})

test_that("rand_names() draws IDs from the shared words", {

  words <- id_words()
  ids <- rand_names(n = 50L)

  expect_type(ids, "character")
  expect_length(ids, 50L)
  expect_identical(anyDuplicated(ids), 0L)

  parts <- strsplit(ids, "_", fixed = TRUE)

  expect_true(all(chr_xtr(parts, 1L) %in% words[["adjectives"]]))
  expect_true(all(chr_xtr(parts, 2L) %in% words[["animals"]]))

  expect_identical(rand_names(n = 0L), character())
  expect_identical(adjective_animal(0L), character())
  expect_length(adjective_animal(3L), 3L)
})

test_that("rand_names() counts an ID up where every draw is taken", {

  local_mocked_bindings(adjective_animal = function(n) rep("calm_heron", n))

  expect_identical(rand_names(), "calm_heron")
  expect_identical(rand_names("calm_heron"), "calm_heron_2")

  expect_identical(
    rand_names(c("calm_heron", "calm_heron_2"), n = 2L, max_tries = 5L),
    c("calm_heron_3", "calm_heron_4")
  )

  expect_identical(
    rand_names(n = 2L, max_tries = 0L),
    c("calm_heron_2", "calm_heron_3")
  )
})

test_that("rand_names() draws IDs in the shape that rand_id names", {

  shapes <- c(
    adjective_animal = "^[a-z]+_[a-z]+$",
    adjective_adjective_animal = "^[a-z]+_[a-z]+_[a-z]+$",
    letters_6 = "^[a-z]{6}$",
    letters_8 = "^[a-z]{8}$",
    letters_12 = "^[a-z]{12}$",
    bytes_4 = "^[0-9a-f]{8}$",
    bytes_8 = "^[0-9a-f]{16}$",
    uuid = paste0(
      "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-",
      "[0-9a-f]{12}$"
    )
  )

  expect_setequal(names(shapes), rand_id_shapes())

  for (rand_id in names(shapes)) {

    ids <- rand_names(n = 50L, rand_id = rand_id)

    expect_true(all(grepl(shapes[[rand_id]], ids)), label = rand_id)
    expect_false(any(grepl("^[0-9]+$", ids)), label = rand_id)
  }

  parts <- strsplit(
    rand_names(n = 50L, rand_id = "adjective_adjective_animal"),
    "_",
    fixed = TRUE
  )

  expect_false(any(chr_xtr(parts, 1L) == chr_xtr(parts, 2L)))

  withr::local_options(blockr.rand_id = "uuid")

  expect_match(rand_names(), shapes[["uuid"]])
})

test_that("rand_names() draws again where a draw is made of digits alone", {

  draws <- c("40213377", "4021ab77")

  local_mocked_bindings(
    draw_id = function(rand_id) {
      res <- draws[1L]
      draws <<- draws[-1L]
      res
    }
  )

  expect_identical(rand_names(rand_id = "bytes_4"), "4021ab77")
})

test_that("rand_names() and adjective_animal() check their arguments", {

  expect_error(rand_names(1), class = "rand_names_old_names_invalid")
  expect_error(rand_names(n = -1L), class = "rand_names_n_invalid")

  expect_error(
    rand_names(max_tries = 1.5),
    class = "rand_names_max_tries_invalid"
  )

  expect_error(
    rand_names(rand_id = "counter"),
    "letters_12",
    class = "rand_names_rand_id_invalid"
  )

  expect_error(adjective_animal(-1L), class = "adjective_animal_n_invalid")
})

test_that("the words of IDs are distinct words of at most six letters", {

  # The build writes them into inst/ids from the client's
  # srcts/store/words.ts, and CI checks that a rebuild changes nothing there.
  for (words in id_words()) {
    expect_true(all(grepl("^[a-z]{2,6}$", words)))
    expect_identical(anyDuplicated(words), 0L)
  }

  expect_named(id_words(), c("adjectives", "animals"))
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

  expect_error(to_sentence_case(1), class = "to_sentence_case_x_invalid")
  expect_error(to_sentence_case(character()),
               class = "to_sentence_case_x_invalid")
  expect_error(to_sentence_case("a", 1, "b"),
               class = "to_sentence_case_replace_invalid")
  expect_error(to_sentence_case("a", "a", 1),
               class = "to_sentence_case_with_invalid")
  expect_error(to_sentence_case("a", c("a", "b"), c("x", "y", "z")),
               class = "to_sentence_case_with_invalid")
})

test_that("blockr_flag() reads TRUE or FALSE, a string as as.logical()", {

  withr::local_options(blockr.test_flag = NULL)
  withr::local_envvar(BLOCKR_TEST_FLAG = "")

  expect_true(blockr_flag("test_flag", TRUE))
  expect_false(blockr_flag("test_flag", FALSE))

  withr::local_envvar(BLOCKR_TEST_FLAG = "T")
  expect_true(blockr_flag("test_flag", FALSE))

  withr::local_envvar(BLOCKR_TEST_FLAG = "FALSE")
  expect_false(blockr_flag("test_flag", TRUE))

  for (value in c("yes", " TRUE", "1", "NA")) {
    withr::local_envvar(BLOCKR_TEST_FLAG = value)
    expect_error(
      blockr_flag("test_flag", FALSE),
      "The blockr option `test_flag` is TRUE or FALSE.",
      class = "blockr_option_invalid",
      fixed = TRUE
    )
  }

  withr::local_envvar(BLOCKR_TEST_FLAG = "")

  for (value in list(1, NA, c(TRUE, FALSE), "maybe")) {
    withr::local_options(blockr.test_flag = value)
    expect_error(
      blockr_flag("test_flag", FALSE),
      class = "blockr_option_invalid"
    )
  }
})

test_that("misc", {

  expect_identical(paste_enum(character()), character())

  expect_error(
    coal(NULL),
    class = "coal_null_return_disallowed"
  )

  res_1 <- withr::with_options(
    list(blockr.test_opt = "b"),
    blockr_option("test_opt", "a")
  )

  expect_identical("a", blockr_option("test_opt", "a"))

  expect_identical(
    "b",
    withr::with_options(
      list(blockr.test_opt = "b"),
      blockr_option("test_opt", "a")
    )
  )

  expect_identical(
    "b",
    withr::with_envvar(
      list(BLOCKR_TEST_OPT = "b"),
      blockr_option("test_opt", "a")
    )
  )

  expect_identical(
    "b",
    withr::with_options(
      list(blockr.test_opt = "b"),
      withr::with_envvar(
        list(BLOCKR_TEST_OPT = "b"),
        blockr_option("test_opt", "a")
      )
    )
  )

  expect_error(
    withr::with_options(
      list(blockr.test_opt = "b"),
      withr::with_envvar(
        list(BLOCKR_TEST_OPT = "c"),
        blockr_option("test_opt", "a")
      )
    ),
    class = "conflicing_blockr_option"
  )

  withr::defer(
    {
      Sys.unsetenv("BLOCKR_TEST_OPT")
      options(`blockr.test_opt` = NULL)
    }
  )

  expect_null(set_blockr_options())

  withr::with_options(
    list(blockr.test_opt = NULL),
    expect_identical(
      set_blockr_options(test_opt = "a"),
      list(blockr.test_opt = NULL)
    )
  )

  withr::with_options(
    list(blockr.test_opt = "a"),
    expect_identical(
      set_blockr_options(test_opt = "b"),
      list(blockr.test_opt = "a")
    )
  )

  withr::with_envvar(
    list(BLOCKR_TEST_OPT = "b"),
    withr::with_options(
      list(blockr.test_opt = "a"),
      {
        expect_identical(
          set_blockr_options(test_opt = "c"),
          list(blockr.test_opt = "a")
        )
        expect_identical(blockr_option("test_opt"), "c")
        expect_identical(Sys.getenv("BLOCKR_TEST_OPT"), "")
      }
    )
  )

  expect_error(set_blockr_options("a"), class = "blockr_options_names_invalid")
  expect_error(set_blockr_options(test_opt = "a", "b"),
               class = "blockr_options_names_invalid")
  expect_error(set_blockr_options(test_opt = "a", test_opt = "b"),
               class = "blockr_options_names_invalid")
})
