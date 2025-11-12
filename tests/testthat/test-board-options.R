test_that("board opts", {

  opts1 <- new_board_options(
    new_board_name_option(),
    new_n_rows_option(),
    new_page_size_option()
  )

  expect_s3_class(opts1, "board_options")
  expect_length(opts1, 3L)
  expect_named(opts1, c("board_name", "n_rows", "page_size"))

  expect_s3_class(opts1[[1]], "board_option")
  expect_s3_class(opts1[["board_name"]], "board_option")

  expect_error(
    opts1[[1]] <- opts1[[2]],
    class = "board_options_duplicated_ids"
  )

  expect_error(
    opts1[["board_name"]] <- opts1[["n_rows"]],
    class = "board_options_duplicated_ids"
  )

  opts2 <- c(new_filter_rows_option(), opts1)

  expect_s3_class(opts2, "board_options")
  expect_length(opts2, 4L)
  expect_named(opts2, c("filter_rows", "board_name", "n_rows", "page_size"))

  opts3 <- c(opts1, new_filter_rows_option())

  expect_s3_class(opts3, "board_options")
  expect_length(opts3, 4L)
  expect_named(opts3, c("board_name", "n_rows", "page_size", "filter_rows"))

  chat_fun <- function(system_prompt = NULL, params = NULL) {}

  expect_warning(
    withr::with_options(
      list(blockr.chat_function = chat_fun),
      new_llm_model_option("foo")
    ),
    class = "single_llm_model"
  )

  expect_error(
    withr::with_options(
      list(blockr.chat_function = list(foo = chat_fun, bar = chat_fun)),
      new_llm_model_option("baz")
    ),
    class = "invalid_llm_model_option_value"
  )

  expect_error(
    withr::with_options(
      list(blockr.chat_function = list(foo = chat_fun, bar = chat_fun)),
      new_llm_model_option("baz")
    ),
    class = "invalid_llm_model_option_value"
  )

  expect_error(
    withr::with_options(
      list(blockr.chat_function = chat_fun),
      new_llm_model_option(chat_fun)
    ),
    class = "invalid_llm_model_option_value"
  )
})

test_that("opt ser/deser", {

  for (opt in default_board_options()) {
    expect_identical(
      opt,
      blockr_deser(blockr_ser(opt)),
      ignore_function_env = TRUE
    )
  }

  reg_opts <- combine_board_options(lapply(available_blocks(), board_options))

  for (opt in reg_opts) {
    expect_identical(
      opt,
      blockr_deser(blockr_ser(opt)),
      ignore_function_env = TRUE
    )
  }

  opts <- list(
    new_board_name_option("foo"),
    new_thematic_option(TRUE),
    new_dark_mode_option("dark"),
    new_show_conditions_option(character()),
    new_show_conditions_option("error"),
    new_show_conditions_option(c("warning", "error", "message"))
  )

  for (opt in opts) {
    expect_identical(
      opt,
      blockr_deser(blockr_ser(opt)),
      ignore_function_env = TRUE
    )
  }

  chat_fun <- function(system_prompt = NULL, params = NULL) {}

  withr::with_options(
    list(blockr.chat_function = chat_fun),
    {
      expect_identical(
        new_llm_model_option(),
        blockr_deser(blockr_ser(new_llm_model_option())),
        ignore_function_env = TRUE
      )
    }
  )

  withr::with_options(
    list(blockr.chat_function = list(foo = chat_fun, bar = chat_fun)),
    {
      expect_identical(
        new_llm_model_option(),
        blockr_deser(blockr_ser(new_llm_model_option())),
        ignore_function_env = TRUE
      )
    }
  )

  withr::with_options(
    list(blockr.chat_function = list(foo = chat_fun, bar = chat_fun)),
    {
      expect_identical(
        new_llm_model_option("bar"),
        blockr_deser(blockr_ser(new_llm_model_option("bar"))),
        ignore_function_env = TRUE
      )
    }
  )
})
