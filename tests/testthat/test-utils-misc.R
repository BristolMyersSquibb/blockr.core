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

  id_1 <- rand_names(n = 1L)

  expect_type(id_1, "character")
  expect_length(id_1, 1L)
  expect_true(grepl("_", id_1, fixed = TRUE))

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

  id_2 <- with_mocked_bindings(
    rand_names(),
    pkg_avail = function(x) {
      if (identical(x, "ids")) {
        FALSE
      } else {
        requireNamespace(x, quietly = TRUE)
      }
    }
  )

  expect_identical(nchar(id_2), 8L)
  expect_type(id_2, "character")
  expect_length(id_2, 1L)
  expect_false(grepl("_", id_2, fixed = TRUE))
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

  expect_s3_class(
    new_blockr_ctor("blockr.core::new_dataset_block"),
    "blockr_ctor"
  )

  expect_identical(forward_ctor(1L), -2L)

  # test for namespaced ctor in `resolve_ctor()`
  expect_s3_class(
    blockr.core::new_dataset_block(),
    "block"
  )
})

test_that("core declares the blockr options its client reads", {

  # As the reply to `hello` carries them in the protocol's fixtures
  expect_identical(
    client_blockr_options()["blockr.core"],
    proto_served()$blockr_options
  )
})

test_that("R sends the blockr options a package's client reads", {

  local_declared(declared_blockr_options)

  register_blockr_options(view_tabs = TRUE, theme = NULL,
                          package = "blockr.test")

  register_blockr_options(view_tabs = FALSE, rails = 2,
                          package = "blockr.test")

  expect_identical(
    client_blockr_options()[["blockr.test"]],
    list(view_tabs = FALSE, theme = NULL, rails = 2)
  )

  withr::local_options(blockr.view_tabs = TRUE, blockr.theme = "dark")

  expect_identical(
    client_blockr_options()[["blockr.test"]],
    list(view_tabs = TRUE, theme = "dark", rails = 2)
  )

  expect_identical(
    names(client_blockr_options()),
    c("blockr.core", "blockr.test")
  )
})

test_that("a blockr option set as a string reads as its default's type", {

  local_declared(declared_blockr_options)

  register_blockr_options(flag = TRUE, count = 2L, delay = Inf, label = "a",
                          unset = NULL, package = "blockr.test")

  withr::local_envvar(
    BLOCKR_FLAG = "false",
    BLOCKR_COUNT = "3",
    BLOCKR_DELAY = "500",
    BLOCKR_LABEL = "b",
    BLOCKR_UNSET = "c"
  )

  expect_identical(
    client_blockr_options()[["blockr.test"]],
    list(flag = FALSE, count = 3L, delay = 500, label = "b", unset = "c")
  )

  withr::local_envvar(BLOCKR_COUNT = "many")

  expect_error(
    client_blockr_options(),
    class = "blockr_option_invalid",
    regexp = "count"
  )
})

test_that("declaring blockr options is checked", {

  local_declared(declared_blockr_options)

  expect_error(
    register_blockr_options(flag = TRUE, package = NULL),
    class = "blockr_options_invalid",
    regexp = "declared by a package"
  )

  expect_error(
    register_blockr_options(TRUE, package = "blockr.test"),
    class = "blockr_options_invalid"
  )

  expect_error(
    register_blockr_options(flag = TRUE, flag = FALSE,
                            package = "blockr.test"),
    class = "blockr_options_invalid"
  )

  expect_identical(register_blockr_options(package = "blockr.test"), list())
})
